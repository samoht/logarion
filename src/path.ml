open Fpath
type repo_t  = Repo of t
type note_t  = Note of { repo: repo_t; basename: t }
type notes_t = Notes of t

let extension = ".ymd"
let notes = v "notes"

let fpath_of_repo = function Repo p -> p
let string_of_repo r = fpath_of_repo r |> to_string
let repo_of_string s = Repo (v s)

let fpath_of_notes = function Notes ns -> ns
let string_of_notes ns = fpath_of_notes ns |> to_string
let notes_of_repo r = Notes (fpath_of_repo r // notes)

let fpath_of_note = function Note n -> (fpath_of_repo n.repo // notes // n.basename)
let string_of_note n = fpath_of_note n |> to_string
let note_of_basename repo s = Note { repo; basename = v s }

let basename_of_title t =
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

let slug_of_note = function Note n -> n.basename |> rem_ext |> to_string
let note_of_slug repo slug = note_of_basename repo (slug ^ extension)

let versioned_basename_of_title ?(version=0) repo (title : string) =
  let notes_fpath = fpath_of_repo repo // notes in
  let basename = v @@ basename_of_title title in
  let rec next version =
    let candidate = basename |> add_ext (string_of_int version) |> add_ext extension in
    if Sys.file_exists (to_string (notes_fpath // candidate))
    then next (succ version)
    else note_of_basename repo (to_string candidate)
  in
  next version
