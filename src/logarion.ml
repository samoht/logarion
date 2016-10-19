module Configuration = struct
  type t = {
      title : string;
      owner : string;
      email : string;
    }

  let of_filename fn =
    let result = Toml.Parser.from_filename fn in
    match result with
    | `Error (str, loc) -> { title = ""; owner = ""; email = "" }
    | `Ok tbl ->
       let str_of key_name = match TomlLenses.(get tbl (key "general" |-- table |-- key key_name |-- string)) with
           Some v -> v | None -> "" in
       { title = str_of "title"; owner = str_of "owner"; email = str_of "email" }
end

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let of_file s =
  let segments = Re_str.(split (regexp "^---$")) (load_file s) in
  let open Ymd in
  if List.length segments = 2 then
    let yaml_str = List.nth segments 0 in
    let md_str = List.nth segments 1 in
    let m = meta_of_yaml yaml_str in
    { meta = m; body = md_str }
  else
    { blank_ymd with body = "Error parsing file" }

let to_file ymd =
  let open Lwt.Infix in
  let path = "ymd/" ^ (Ymd.filename ymd) in
  Lwt_io.with_file ~mode:Lwt_io.output path  (fun out ->
      Lwt_io.write out (Ymd.to_string ymd)
    )

let file_meta_pairs () =
  let files = Array.to_list @@ Sys.readdir "ymd/" in
  let ymd_list a e =  if BatString.ends_with e ".ymd" then List.cons e a else a in
  let ymds = List.fold_left ymd_list [] files in
  let t y = (y, (of_file ("ymd/" ^ y)).meta) in
  List.map t ymds

let latest_file_meta_pair fragment =
  let open Ymd in
  let latest p (path', meta') =
    if not @@ BatString.exists (meta'.title) fragment then None
    else
      match p with
      | Some (path, meta) ->
         if meta.date.published < meta'.date.published
         then Some (path', meta') else p
      | None -> Some (path', meta') in
  ListLabels.fold_left ~f:latest ~init:(None) (file_meta_pairs ())
