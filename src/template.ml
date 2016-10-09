
let string s = prerr_endline ("string:"^s); s
let section ~inverted name contents = "section"
let escaped e = prerr_endline ("escaped:"^e); e
let unescaped u = u
let partial p = p
let comment c = c
let concat l = String.concat "," l

let of_string = Mustache.of_string

let of_file f =
  let open Lwt in
  Lwt_io.(open_file ~mode:(Input) f >|= read_lines)
  >|= (fun stream -> Lwt_stream.fold (^) stream "")

let fold = Mustache.fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat
