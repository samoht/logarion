let of_filename f =
  let open Lwt in
  Lwt_io.(open_file ~mode:(Input) f >|= read_lines)
  >|= (fun stream -> Lwt_stream.fold (^) stream "")
