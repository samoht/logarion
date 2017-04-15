type t = Mustache.t

type header = Header of t
type footer = Footer of t
type listing = Listing of t
type listing_entry = Listing_entry of t
type text = Text of t
type index = Index of t

let of_string = Mustache.of_string
let of_file f = Logarion.File.load f |> of_string

let header  f = Header (of_file f)
let listing f = Listing (of_file f)
let listing_entry f = Listing_entry (of_file f)
let text    f = Text (of_file f)
let index   f = Index (of_file f)

let string s = s
let section ~inverted name contents = "section"
let unescaped u = u
let partial p = p
let comment c = c
let concat l = String.concat "" l

let fold_text ymd =
  let escaped e = match e with
    | "body" -> Omd.to_html @@ Omd.of_string ymd.Note.body
    | tag -> Meta.value_with_name ymd.Note.meta tag in
  Mustache.fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat

let fold_entry (entry : Logarion.Entry.t) =
  let open Logarion.Entry in
  let escaped e = match e with
    | "url" -> "/note/" ^ slug entry
    | "date"
      | "date_created"
      | "date_edited"
      | "date_published"
      | "date_human"    -> "<time>" ^ Meta.value_with_name entry.attributes e ^ "</time>"
    | tag -> Meta.value_with_name entry.attributes tag in
  Mustache.fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat

let fold_header blog_url title =
  let escaped e = match e with
    | "blog_url" -> blog_url
    | "title" -> title
    | _ -> prerr_endline ("unknown tag: " ^ e); "" in
  Mustache.fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat

let fold_index ?(entry_tpl=None) lgrn =
  let open Logarion.Entry in
  let simple entry =
    "<li><a href=\"/note/" ^ slug entry ^ "\">"
    ^ entry.attributes.Meta.title ^ " ~ " ^ Meta.Date.(pretty_date (entry |> date |> last)) ^ "</a></li>" in
  let fold_entry tpl entry = fold_entry entry tpl in
  let entry = match entry_tpl with Some (Listing_entry e) -> fold_entry e | None -> simple in
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
