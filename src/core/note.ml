type t = {
    meta: Meta.t;
    body: string;
  } [@@deriving lens { submodule = true }]

let blank ?(uuid=(Meta.Id.generate ())) () = { meta = Meta.blank ~uuid (); body = "" }

let title ymd =
  let mtitle = ymd.meta.Meta.title in
  if String.length mtitle > 0 then mtitle else
    let open Omd in
    try List.find (function H1 t -> true | _ -> false) (Omd.of_string ymd.body)
        |> function H1 h -> to_text h | _ -> ""
    with Not_found -> ""

let categorised categs ymd = Meta.CategorySet.categorised categs ymd.meta.Meta.categories

let with_kv ymd (k,v) =
  match k with
  | "body" -> { ymd with body = String.trim v }
  | _      -> { ymd with meta = Meta.with_kv ymd.meta (k,v) }

let meta_pair_of_string line =
  let e = Re_str.(bounded_split (regexp ": *")) line 2 in
  if List.length e = 2
  then (Re_str.(replace_first (regexp "^[ -] ") "" (List.nth e 0)), List.nth e 1)
  else (Re_str.(replace_first (regexp "^[ -] ") "" line), "")

let meta_of_string front_matter =
  let fields = List.map meta_pair_of_string (BatString.nsplit front_matter "\n") in
  List.fold_left Meta.with_kv (Meta.blank ()) fields

exception Syntax_error of string

let front_matter_body_split s =
  if BatString.starts_with s "---"
  then let l = Re_str.(bounded_split (regexp "^---$")) s 2 in List.(nth l 0, nth l 1)
  else (
    let has_meta =
      let tokens a c =
        if a = None
        then match c with ':' -> Some true | '\n' | ' ' -> Some false | _ -> None
        else a
      in
      match BatString.fold_left tokens None s with Some true -> true | _ -> false
    in
    if has_meta
    then BatString.split s "\n\n"
    else ("", s)
  )

let of_string s =
  let (front_matter, body) = front_matter_body_split s in
  try
    let note = { meta = meta_of_string front_matter; body } in
    { note with meta = { note.meta with title = title note } }
  with _ -> prerr_endline ("Failed parsing" ^ s); blank ()

let to_string ymd = Meta.to_string ymd.meta ^ "\n" ^ ymd.body
