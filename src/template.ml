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
    | "title" -> meta.title
    | "abstract" -> meta.abstract
    | "authors" -> meta.authors
    | "text" ->
    | _ -> prerr_endline ("unknown tag: " ^ e); "" in
  Mustache.fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat

let fold_entry (file, meta) =
  let escaped e = match e with
    | "title" -> meta.title
    | "abstract" -> meta.abstract
    | "authors" -> meta.authors
    | _ -> prerr_endline ("unknown tag: " ^ e); "" in
  Mustache.fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat

let fold_index ymd_meta_pairs =
  let string s = s in
  let section ~inverted name contents = "section" in
  let escaped e = match e with
    | "recent_texts_listing" ->
       (ListLabels.fold_left
         ~init:("<ul>")
         ~f:(fun a (file, meta) ->
           a ^ "<li><a href=\"/text/" ^ Filename.chop_extension file ^ "\">" ^ meta.title ^ "</a></li>")
         ymd_meta_pairs) ^ "</ul>"
    | _ -> prerr_endline ("unknown tag: " ^ e); "" in
  let unescaped u = u in
  let partial p = p in
  let comment c = c in
  let concat l = String.concat "" l in
  Mustache.fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat
