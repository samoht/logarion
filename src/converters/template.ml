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
    let open Logarion.Config in
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
let unescaped u = List.fold_left (fun a s -> a ^ s) "" u
let partial ?indent name _ _ = "partials not supported"
let comment c = c
let concat l = String.concat "" l

let fold_note ymd =
  let escaped e = match List.hd e with
    | "body" -> Omd.to_html @@ Omd.of_string ymd.Logarion.Note.body
    | tag    -> Logarion.Meta.value_with_name ymd.Logarion.Note.meta tag in
  Mustache.fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat

let fold_meta (meta : Logarion.Meta.t) =
  let open Logarion in
  let escaped e =
    let e = List.hd e in
    match e with
    | "url" -> "/note/" ^ Meta.alias meta
    | "date" | "date_created" | "date_edited" | "date_published" | "date_human" ->
       "<time>" ^ Meta.value_with_name meta e ^ "</time>"
    | tag -> Meta.value_with_name meta tag in
  Mustache.fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat

let fold_header blog_url title =
  let escaped e =
    let e = List.hd e in
    match e with
    | "blog_url" -> blog_url
    | "title"    -> title
    | _ -> prerr_endline ("unknown tag: " ^ e); "" in
  Mustache.fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat

let fold_list ?(item_tpl=None) ~from ~n notes =
  let module Meta = Logarion.Meta in
  let simple meta =
    "<li><a href=\"/note/" ^ Meta.alias meta ^ "\"><p class=\"title\">"
    ^ meta.Meta.title ^ "</p><p class=\"time\">" ^ Meta.Date.(pretty_date (last meta.Meta.date))
    ^ "</p></a></li>"
  in
  let fold_meta tpl meta = fold_meta meta tpl in
  let meta = match item_tpl with Some (Item e) -> fold_meta e | None -> simple in
  let escaped e =
    let e = List.hd e in
    match e with
    | "navigation" ->
       ""
       ^ (if from > 0 then ("<a href=\"?p=" ^ string_of_int (pred from) ^ "\">previous</a> | ") else "")
       ^ (if n <= List.length notes then ("<a href=\"?p=" ^  string_of_int (succ from) ^ "\">next</a>") else "")
    | "recent_texts_listing" ->
       let open Logarion in
       ListLabels.fold_left ~init:"" ~f:(fun a e -> a ^ meta e) notes
    | "topics" ->
       let topics =
         ListLabels.fold_left
           ~init:(Meta.StringSet.empty)
           ~f:(fun a e -> Meta.unique_topics a e ) notes
       in
       Meta.StringSet.fold (fun e a -> a ^ "<li>" ^ e ^ "</li>") topics ""
    | _ -> prerr_endline ("unknown tag: " ^ e); "" in
  Mustache.fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat
