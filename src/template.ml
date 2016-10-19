open Ymd

let of_string = Mustache.of_string

let of_file f =
  let open Lwt in
  Lwt_io.(open_file ~mode:(Input) f >|= read_lines)
  >|= (fun stream -> Lwt_stream.fold (^) stream "")

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
    | "author_name" -> ymd.meta.author.name
    | "author_email" -> ymd.meta.author.email
    | "date_edited" -> rfc_string_of ymd.meta.date.edited
    | "date_published" -> rfc_string_of ymd.meta.date.published;
    | "date_human" ->  pretty_date_of @@ modification_date ymd
    | "date" -> rfc_string_of @@ modification_date ymd
    | "topics" -> String.concat ", " ymd.meta.topics;
    | "categories" -> String.concat ", " ymd.meta.categories;
    | "keywords" -> String.concat ", " ymd.meta.keywords;
    | "series" -> String.concat ", " ymd.meta.series;
    | "body" -> Omd.to_html (Omd.of_string Ymd.(ymd.body))
    | _ -> prerr_endline ("unknown tag: " ^ e); "" in
  Mustache.fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat

let fold_entry (file, meta) =
  let escaped e = match e with
    | "title" -> meta.title
    | "abstract" -> meta.abstract
    | "author_name" -> meta.author.name
    | "author_email" -> meta.author.email
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
           a ^ "<li><a href=\"/text/" ^ Filename.chop_extension file ^ "\">" ^ meta.title ^ "</a></li>")
         ymd_meta_pairs) ^ "</ul>"
    | _ -> prerr_endline ("unknown tag: " ^ e); "" in
  Mustache.fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat
