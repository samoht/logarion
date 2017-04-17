let load f =
  let ic = open_in (Fpath.to_string f) in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let note path = Path.fpath_of_note path |> load |> Note.of_string

let notes_of_repo repo =
  Path.(notes_of_repo repo |> string_of_notes)
  |> Sys.readdir
  |> Array.to_list

module Lwt = struct
  let of_filename f =
    let open Lwt in
    Lwt_io.(open_file ~mode:(Input) f >|= read_lines)
    >|= (fun stream -> Lwt_stream.fold (^) stream "")

  let with_note note (previous, filepath) =
    let open Lwt in
    let write_note out = Lwt_io.write out (Note.to_string note) in
    ignore_result (Lwt_io.with_file ~mode:Lwt_io.output (Path.string_of_note filepath) write_note);
    (match previous with
     | Some path -> if path <> filepath then ignore_result (Lwt_unix.unlink @@ Path.string_of_note path);
     | None -> ());
    return note;
end
