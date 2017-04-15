open Lens

type t = {
    meta: Meta.t;
    body: string;
  } [@@deriving lens { submodule = true }]

let blank ?(uuid=(Meta.Id.generate ())) () = { meta = Meta.blank ~uuid (); body = "" }

let filename_of_title t =
  let is_reserved = function
    | '!' | '*' | '\'' | '(' | ')' | ';' | ':' | '@' | '&' | '=' | '+' | '$'
      | ',' | '/' | '?' | '#' | '[' | ']' | ' ' | '\t' | '\x00' -> true
    | _ -> false in
  let drop h t = t in
  let dash h t = '-' :: t in
  let rec filter fn = function
    | [] -> []
    | head :: tail ->
       if is_reserved head
       then fn head (filter drop tail)
       else Char.lowercase_ascii head :: (filter dash tail) in
  Batteries.String.of_list @@ filter drop (Batteries.String.to_list t)

let title ymd =
  let mtitle = ymd.meta.Meta.title in
  if String.length mtitle > 0 then mtitle else
    let open Omd in
    try List.find (function H1 t -> true | _ -> false) (Omd.of_string ymd.body)
        |> function H1 h -> to_text h | _ -> ""
    with Not_found -> ""

let filename ymd = filename_of_title ymd.meta.Meta.title
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
  let open Infix in
  List.fold_left Meta.with_kv (Meta.blank ()) fields

exception Syntax_error of string

let of_string s =
  let (front_matter, body) =
    if BatString.starts_with s "---"
    then let l = Re_str.(bounded_split (regexp "^---$")) s 2 in List.(nth l 0, nth l 1)
    else BatString.split s "\n\n"
  in
  try { meta = meta_of_string front_matter; body }
  with _ -> prerr_endline ("Failed paring" ^ s); blank ()

let to_string ymd = Meta.to_string ymd.meta ^ "\n" ^ ymd.body
