type t = Mustache.t

module Configuration = struct
  type path_t = Fpath.t option

  type paths_t = {
      dir    : path_t;
      header : path_t;
      note   : path_t;
      front  : path_t;
      list   : path_t;
      item   : path_t;
    }

  let default_paths = {
      dir    = None;
      header = None;
      note   = None;
      front  = None;
      list   = None;
      item   = None;
    }

  let of_toml_file toml =
    let open Logarion_toml in
    let path_tpl = path_opt toml "templates" in
    {
      dir    = path_tpl "dir";
      header = path_tpl "header";
      note   = path_tpl "note";
      front  = path_tpl "front";
      list   = path_tpl "list";
      item   = path_tpl "item";
    }
end

type header_t = Header of t
type footer_t = Footer of t
type list_t   = List of t
type item_t   = Item of t
type note_t   = Note of t
type front_t  = Front of t

let of_string = Mustache.of_string
let of_file f = File.load f |> of_string

let map_tpl_opt f field ps =
  let open Configuration in
  match ps.dir, field with
  | Some dir, Some bn -> Some (f (of_file (Fpath.append dir bn)))
  | _ -> None

let header ps = map_tpl_opt (fun v -> Header v) ps.Configuration.header ps
let note   ps = map_tpl_opt (fun v -> Note v)   ps.Configuration.note   ps
let front  ps = map_tpl_opt (fun v -> Front v)  ps.Configuration.front  ps
let list   ps = map_tpl_opt (fun v -> List v)   ps.Configuration.list   ps
let item   ps = map_tpl_opt (fun v -> Item v)   ps.Configuration.item   ps

let string s = s
let section ~inverted name contents = "section"
let unescaped u = u
let partial p = p
let comment c = c
let concat l = String.concat "" l

let fold_note ymd =
  let escaped e = match e with
    | "body" -> Omd.to_html @@ Omd.of_string ymd.Note.body
    | tag    -> Meta.value_with_name ymd.Note.meta tag in
  Mustache.fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat

let fold_entry (entry : Logarion.Entry.t) =
  let open Logarion.Entry in
  let escaped e = match e with
    | "url" -> "/note/" ^ slug entry
    | "date" | "date_created" | "date_edited" | "date_published" | "date_human" ->
       "<time>" ^ Meta.value_with_name entry.attributes e ^ "</time>"
    | tag -> Meta.value_with_name entry.attributes tag in
  Mustache.fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat

let fold_header blog_url title =
  let escaped e = match e with
    | "blog_url" -> blog_url
    | "title"    -> title
    | _ -> prerr_endline ("unknown tag: " ^ e); "" in
  Mustache.fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat

let fold_list ?(item_tpl=None) lgrn =
  let open Logarion.Entry in
  let simple entry =
    "<li><a href=\"/note/" ^ slug entry ^ "\">"
    ^ entry.attributes.Meta.title ^ " ~ " ^ Meta.Date.(pretty_date (entry |> date |> last)) ^ "</a></li>" in
  let fold_entry tpl entry = fold_entry entry tpl in
  let entry = match item_tpl with Some (Item e) -> fold_entry e | None -> simple in
  let escaped e = match e with
    | "recent_texts_listing" ->
       let entries = Logarion.Archive.(of_repo lgrn.Logarion.Configuration.repository |> latest_listed) in
       (ListLabels.fold_left ~init:("<ul>") ~f:(fun a e -> a ^ (entry e)) entries)
       ^ "</ul>"
    | "topics" ->
       let entries = Logarion.(Archive.of_repo lgrn.Configuration.repository |> Archive.listed |> Archive.topics) in
       (ListLabels.fold_left ~init:("<ul>") ~f:(fun a e -> a ^ "<li>" ^ e ^ "</li>") entries)
       ^ "</ul>"
    | _ -> prerr_endline ("unknown tag: " ^ e); "" in
  Mustache.fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat
