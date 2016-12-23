module Id = struct
  include Ymd.Id
end

module Configuration = struct
  type t = {
      repository : string;
      title : string;
      owner : string;
      email : string;
      id : Id.t;
    }

  let default ?(id=(Id.generate ())) () = {
      repository = Sys.getenv "HOME" ^ "/ymd";
      title = "Logarion journal";
      owner = "";
      email = "";
      id;
    }

  let of_filename fn =
    let result = Toml.Parser.from_filename fn in
    match result with
    | `Error (str, loc) -> default ()
    | `Ok toml ->
       let str = Logarion_toml.str toml "general" in
       let default = default () in
       {
         repository = str "repository" default.repository;
         title = str "title" default.title;
         owner = str "owner" default.owner;
         email = str "email" default.email;
         id = match Id.of_string (str "uuid" "") with Some id -> id | None -> Id.generate();
       }
end

let ymddir = "ymd/"
let titledir = ymddir ^ "title/"
let uuiddir = ymddir ^ "uuid/"
let extension = ".ymd"

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
    { (blank_ymd ()) with body = "Error parsing file" }

let file_meta_pairs () =
  let files = Array.to_list @@ Sys.readdir titledir in
  let ymd_list a e =  if BatString.ends_with e extension then List.cons e a else a in
  let ymds = List.fold_left ymd_list [] files in
  let t y = (y, (of_file (titledir ^ y)).Ymd.meta) in
  List.map t ymds

let file_ymd_pairs () =
  let files = Array.to_list @@ Sys.readdir titledir in
  let ymd_list a e =  if BatString.ends_with e extension then List.cons e a else a in
  let ymds = List.fold_left ymd_list [] files in
  let t y = (y, (of_file (titledir ^ y))) in
  List.map t ymds

let rec next_semantic_filepath ?(version=0) ymd =
  let candidate = titledir ^ (Ymd.filename ymd) ^ "." ^ (string_of_int version) ^ extension in
  if Sys.file_exists candidate then next_semantic_filepath ~version:(version+1) ymd
  else candidate

let path_of_title title = titledir ^ Ymd.filename_of_title title ^ extension

let uuid_path ymd = uuiddir ^ Ymd.(Id.to_string ymd.meta.uuid) ^ extension

let slug_of_filename filename = List.hd @@ BatString.split_on_char '.' filename

let to_file ymd =
  let open Lwt.Infix in
  let uuid_path = uuid_path ymd in
  let write_ymd out = Lwt_io.write out (Ymd.to_string ymd) in
  Lwt_io.with_file ~mode:Lwt_io.output uuid_path write_ymd;
  >>= fun () ->
  let open Ymd in
  if not (categorised [Category.Draft] ymd) && ymd.meta.title <> "" then 
    let fmp = file_meta_pairs () in
    begin try
        let (file, m) = List.find (fun (_, meta) -> meta.uuid = ymd.meta.uuid) fmp in
        if slug_of_filename file <> (Ymd.filename ymd) then
          let found_filepath = titledir ^ file in
          Lwt_unix.rename found_filepath (next_semantic_filepath ymd);
        else Lwt.return ()
      with Not_found ->
        Lwt_unix.link uuid_path (next_semantic_filepath ymd);
    end
  else
    Lwt.return ()

let latest_file_meta_pair fragment =
  let open Ymd in
  let latest p (path', meta') =
    if not @@ BatString.exists (meta'.title) fragment then None
    else
      match p with
      | Some (path, meta) ->
         if meta.date.Date.published < meta'.date.Date.published
         then Some (path', meta') else p
      | None -> Some (path', meta') in
  ListLabels.fold_left ~f:latest ~init:(None) (file_meta_pairs ())
