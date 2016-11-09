open Ymd

type t = Mustache.t

type header = Header of t
type footer = Footer of t
type listing = Listing of t
type listing_entry = Listing_entry of t
type text = Text of t

let of_string = Mustache.of_string
let of_file f = Logarion.load_file f |> of_string

let header  f = Header (of_file f)
let listing f = Listing (of_file f)
let listing_entry f = Listing_entry (of_file f)
let text    f = Text (of_file f)

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
    | "categories" -> String.concat ", " ymd.meta.categories;
    | "keywords" -> String.concat ", " ymd.meta.keywords;
    | "series" -> String.concat ", " ymd.meta.series;
    | "body" -> Omd.to_html (Omd.of_string Ymd.(ymd.body))
    | "uuid" -> Id.to_string ymd.meta.uuid
    | _ -> prerr_endline ("unknown tag: " ^ e); "" in
  Mustache.fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat

let fold_entry (file, meta) =
  let escaped e = match e with
    | "url" -> "text/" ^ Filename.chop_extension file
    | "title" -> meta.title
    | "abstract" -> meta.abstract
    | "author_name" -> meta.author.Author.name
    | "author_email" -> meta.author.Author.email
    | "date_edited" -> Date.(rfc_string meta.date.edited)
    | "date_published" -> Date.(rfc_string meta.date.published)
    | "date_human" -> Date.(pretty_date @@ last meta.date)
    | "date" -> Date.(rfc_string @@ last meta.date)
    | "topics" -> String.concat ", " meta.topics;
    | "categories" -> String.concat ", " meta.categories;
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
  
let fold_index ?(entry_tpl=None) ymd_meta_pairs =
  let simple (file, meta) =
    "<li><a href=\"/text/" ^ Filename.chop_extension file ^ "\">"
    ^ meta.title ^ " ~ " ^ Ymd.Date.(pretty_date @@ last meta.date) ^ "</a></li>" in
  let fold_entry tpl (file, meta) = fold_entry (file, meta) tpl in
  let entry = match entry_tpl with Some (Listing_entry e) -> fold_entry e | None -> simple in
  let escaped e = match e with
    | "recent_texts_listing" ->
       (ListLabels.fold_left
         ~init:("<ul>")
         ~f:(fun a (file, meta) -> a ^ (entry (file, meta)))
         ymd_meta_pairs) ^ "</ul>"
    | _ -> prerr_endline ("unknown tag: " ^ e); "" in
  Mustache.fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat
