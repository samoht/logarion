open Logarion
let load f =
  let ic = open_in (Fpath.to_string f) in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let note path = Lpath.fpath_of_note path |> load |> Note.of_string

type t = { repo_path : Lpath.repo_t }

let to_list ?(order) lens_fn store =
  let repo_path = store.repo_path in
  let cons_valid_meta list path =
    try
      let note = note (Lpath.note_of_basename repo_path path) in
      lens_fn note :: list
    with Note.Syntax_error str -> prerr_endline str; list
  in
  Lpath.(notes_of_repo repo_path |> string_of_notes)
  |> Sys.readdir
  |> Array.to_list
  |> List.filter (fun file -> BatString.ends_with file Lpath.extension)
  |> List.fold_left cons_valid_meta []
  |> match order with
     | Some fn -> List.fast_sort fn
     | None -> (fun x -> x)

let note_with_id store id =
  let repo_path = store.repo_path in
  let note_of_path path = note (Lpath.note_of_basename repo_path path) in
  let with_id path =
    try
      let note = note_of_path path in
      note.Note.meta.Meta.uuid = id
    with Note.Syntax_error str -> prerr_endline str; false
  in
  let notes =
    Lpath.(notes_of_repo repo_path |> string_of_notes)
    |> Sys.readdir
    |> Array.to_list
    |> List.filter (fun file -> BatString.ends_with file Lpath.extension)
  in
  try Some (note_of_path (List.find with_id notes))
  with Not_found -> None

let note_with_alias store alias =
  let repo_path = store.repo_path in
  let cons_valid_meta list path =
    try (note (Lpath.note_of_basename repo_path path)) :: list
    with Note.Syntax_error str -> prerr_endline str; list
  in
  let recency_order a b = Meta.(Date.compare b.date a.date) in
  let notes =
    Lpath.(notes_of_repo repo_path |> string_of_notes)
    |> Sys.readdir
    |> Array.to_list
    |> List.filter (fun file -> BatString.ends_with file Lpath.extension)
    |> List.fold_left cons_valid_meta []
    |> List.filter (fun note -> Meta.alias note.Note.meta = alias)
    |> List.fast_sort (fun a b -> recency_order a.Note.meta b.Note.meta)
  in
  try Some (List.hd notes)
  with Failure _ -> None

let notepath_with_id store id = None

let store repo_path = { repo_path }

module Lwt = struct
  let of_filename f =
    let open Lwt in
    Lwt_io.(open_file ~mode:(Input) f >|= read_lines)
    >|= (fun stream -> Lwt_stream.fold (^) stream "")

  let with_note store new_note =
    let open Lwt in
    let write_note out = Lwt_io.write out (Note.to_string new_note) in
    let () = match notepath_with_id store new_note.Note.meta.Meta.uuid with
      | Some previous_path ->
         let filepath =
           let open Note in
           let open Meta in
           if (note previous_path).meta.title <> new_note.meta.title
           then Lpath.versioned_basename_of_title store.repo_path new_note.meta.title
           else previous_path
         in
         ignore_result (Lwt_io.with_file ~mode:Lwt_io.output (Lpath.string_of_note filepath) write_note);
         if previous_path <> filepath then ignore_result (Lwt_unix.unlink @@ Lpath.string_of_note previous_path)
      | None ->
         let filepath = Lpath.versioned_basename_of_title store.repo_path new_note.meta.title in
         ignore_result (Lwt_io.with_file ~mode:Lwt_io.output (Lpath.string_of_note filepath) write_note);
    in
    return new_note;
end

let with_note = Lwt.with_note
