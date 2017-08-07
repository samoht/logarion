open Fpath
type repo_t  = Repo of t
type note_t  = Note of { repo: repo_t; basename: t }
type notes_t = Notes of t
type config_t = Config of t

let extension = ".ymd"
let notes = v "notes"

let path_exists x = to_string x |> Sys.file_exists

let config_of_string s = Config (of_string s |> function Ok p -> p | _ -> invalid_arg "Config")
let fpath_of_config = function Config c -> c
let string_of_config c = fpath_of_config c |> to_string

let config_paths =
  let paths =
    try [ "."; Sys.getenv "HOME" ^ "/.config/logarion/"; "/etc/logarion/" ]
    with Not_found -> [ "."; "/etc/logarion/" ] in
  List.map v paths

let from_config_paths config_file =
  let basepath = v config_file in
  let existing dir = path_exists (dir // basepath) in
  Config (List.find existing config_paths // basepath)

let fpath_of_repo = function Repo p -> p
let string_of_repo r = fpath_of_repo r |> to_string
let repo_of_string s = Repo (v s)

let fpath_of_notes = function Notes ns -> ns
let string_of_notes ns = fpath_of_notes ns |> to_string
let notes_of_repo r = Notes (fpath_of_repo r // notes)

let fpath_of_note = function Note n -> (fpath_of_repo n.repo // notes // n.basename)
let string_of_note n = fpath_of_note n |> to_string
let note_of_basename repo s = Note { repo; basename = v s }

let alias_of_note = function Note n -> n.basename |> rem_ext |> to_string
let note_of_alias repo alias = note_of_basename repo (alias ^ extension)

let versioned_basename_of_title ?(version=0) repo (title : string) =
  let notes_fpath = fpath_of_repo repo // notes in
  let basename = v @@ Meta.string_alias title in
  let rec next version =
    let candidate = basename |> add_ext (string_of_int version) |> add_ext extension in
    if Sys.file_exists (to_string (notes_fpath // candidate))
    then next (succ version)
    else note_of_basename repo (to_string candidate)
  in
  next version
