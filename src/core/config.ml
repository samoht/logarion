open TomlLenses

let int toml table_name key_name default =
  match get toml (key table_name |-- table |-- key key_name |-- int) with
    Some i -> i | None -> default

let float toml table_name key_name default =
  match get toml (key table_name |-- table |-- key key_name |-- float) with
    Some f -> f | None -> default

let str_opt toml table_name key_name =
  get toml (key table_name |-- table |-- key key_name |-- string)

let str toml table_name key_name default =
  match str_opt toml table_name key_name with
    Some s -> s | None -> default

let strs_opt toml table_name key_name =
  get toml (key table_name |-- table |-- key key_name |-- array |-- strings)

let strs toml table_name key_name default =
  match strs_opt toml table_name key_name with
    Some ss -> ss | None -> default

let path_opt toml table_name key_name =
  match str_opt toml table_name key_name with
    Some s -> Some (Fpath.v s) | None -> None

let path toml table_name key_name default =
  match str_opt toml table_name key_name with
    Some s -> Fpath.v s | None -> default

let paths_opt toml table_name key_name =
  match strs_opt toml table_name key_name with
    Some ss -> Some (List.map Fpath.v ss) | None -> None

let paths toml table_name key_name default =
  match strs_opt toml table_name key_name with
    Some ss -> List.map Fpath.v ss | None -> default

module Validation = struct
  let empty = []

  let (&>) report = function None -> report | Some msg -> msg :: report
  let (&&>) report = function [] -> report | msgs -> msgs @ report

  let check ok msg = if ok then None else Some msg

  let file_exists ?(msg=(fun s -> (s ^ " is not a file"))) ?(parent_dir=Fpath.v ".") file =
    let str = Fpath.(to_string (parent_dir // file)) in
    check (Sys.file_exists str) (msg str)

  let is_directory ?(msg=(fun s -> (s ^ " is not a directory"))) dir =
    let str = Fpath.to_string dir in
    check (Sys.file_exists str && Sys.is_directory str) (msg str)

  let files_exist ?(msg=(fun s -> (s ^ " is not a file"))) ?(parent_dir=Fpath.v ".") files =
    let f report file = report &> file_exists ~msg ~parent_dir file  in
    List.fold_left f empty files

  let terminate_when_invalid =
    let error i msg = prerr_endline ("Error " ^ string_of_int i ^ ": " ^ msg) in
    function
    | [] -> ()
    | msgs -> List.iteri error (List.rev msgs); exit 1

end
