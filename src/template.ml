open Ymd

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
    | "title" -> ymd.meta.title
    | "abstract" -> ymd.meta.abstract
    | "author_name" -> ymd.meta.author.Author.name
    | "author_email" -> ymd.meta.author.Author.email
    | "date_edited" -> Date.(rfc_string ymd.meta.date.edited)
    | "date_published" -> Date.(rfc_string ymd.meta.date.published)
    | "date_human" -> Date.(pretty_date @@ last ymd.meta.date)
    | "date" -> Date.(rfc_string @@ last ymd.meta.date)
    | "topics" -> String.concat ", " ymd.meta.topics;
    | "categories" -> CategorySet.to_csv ymd.meta.categories;
    | "keywords" -> String.concat ", " ymd.meta.keywords;
    | "series" -> String.concat ", " ymd.meta.series;
    | "body" -> Omd.to_html (Omd.of_string Ymd.(ymd.body))
    | "uuid" -> Id.to_string ymd.meta.uuid
    | _ -> prerr_endline ("unknown tag: " ^ e); "" in
  Mustache.fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat

let fold_entry (entry : Logarion.Entry.t) =
  let meta = entry.meta in
  let escaped e = match e with
    | "url" -> "text/" ^ Filename.chop_extension entry.Logarion.Entry.filepath
    | "title" -> meta.title
    | "abstract" -> meta.abstract
    | "author_name" -> meta.author.Author.name
    | "author_email" -> meta.author.Author.email
    | "date_edited" -> Date.(rfc_string meta.date.edited)
    | "date_published" -> Date.(rfc_string meta.date.published)
    | "date_human" -> Date.(pretty_date @@ last meta.date)
    | "date" -> Date.(rfc_string @@ last meta.date)
    | "topics" -> String.concat ", " meta.topics;
    | "categories" -> CategorySet.to_csv meta.categories;
    | "keywords" -> String.concat ", " meta.keywords;
    | "series" -> String.concat ", " meta.series;
    | "uuid" -> Id.to_string meta.uuid
    | _ -> prerr_endline ("unknown tag: " ^ e); "" in
  Mustache.fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat

let fold_header blog_url title =
  let escaped e = match e with
    | "blog_url" -> blog_url
    | "title" -> title
    | _ -> prerr_endline ("unknown tag: " ^ e); "" in
  Mustache.fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat

let fold_index ?(entry_tpl=None) lgrn =
  let simple entry =
    "<li><a href=\"/text/" ^ Filename.chop_extension entry.Logarion.Entry.filepath ^ "\">"
    ^ entry.meta.title ^ " ~ " ^ Ymd.Date.(pretty_date @@ last entry.meta.date) ^ "</a></li>" in
  let fold_entry tpl entry = fold_entry entry tpl in
  let entry = match entry_tpl with Some (Listing_entry e) -> fold_entry e | None -> simple in
  let escaped e = match e with
    | "recent_texts_listing" ->
       let entries = Logarion.(Archive.of_repo lgrn.Configuration.repository |> latest_listed_entries) in
       (ListLabels.fold_left ~init:("<ul>") ~f:(fun a e -> a ^ (entry e)) entries)
       ^ "</ul>"
    | "topics" ->
       let entries = Logarion.(Archive.of_repo lgrn.Configuration.repository |> Archive.listed |> Archive.topics) in
       (ListLabels.fold_left ~init:("<ul>") ~f:(fun a e -> a ^ "<li>" ^ e ^ "</li>") entries)
       ^ "</ul>"
    | _ -> prerr_endline ("unknown tag: " ^ e); "" in
  Mustache.fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat
