open Ymd

type header = Header of string
type footer = Footer of string
type listing = Listing of string
type listing_entry = Listing_entry of string
type text = Text of string

let of_string = Mustache.of_string

let header  f = Header (Logarion.load_file f)
let listing f = Listing (Logarion.load_file f)
let text    f = Text (Logarion.load_file f)

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
    | "title" -> meta.title
    | "abstract" -> meta.abstract
    | "author_name" -> meta.author.Author.name
    | "author_email" -> meta.author.Author.email
    | "uuid" -> Id.to_string meta.uuid
    | _ -> prerr_endline ("unknown tag: " ^ e); "" in
  Mustache.fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat

let fold_header blog_url title =
  let escaped e = match e with
    | "blog_url" -> blog_url
    | "title" -> title
    | _ -> prerr_endline ("unknown tag: " ^ e); "" in
  Mustache.fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat
  
let fold_index ymd_meta_pairs =
  let escaped e = match e with
    | "recent_texts_listing" ->
       (ListLabels.fold_left
         ~init:("<ul>")
         ~f:(fun a (file, meta) ->
           a ^ "<li><a href=\"/text/" ^ Filename.chop_extension file ^ "\">"
           ^ meta.title ^ " ~ " ^ Ymd.Date.(pretty_date @@ last meta.date) ^ "</a></li>")
         ymd_meta_pairs) ^ "</ul>"
    | _ -> prerr_endline ("unknown tag: " ^ e); "" in
  Mustache.fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat
