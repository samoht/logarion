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

module File = struct
  let load f =
    let ic = open_in f in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    (s)
end

let titledir ymddir = ymddir ^ "/title/"
let uuiddir  ymddir = ymddir ^ "/uuid/"
let extension = ".ymd"
let title_path repo title = titledir repo ^ Ymd.filename_of_title title ^ extension
let uuid_path  repo ymd   = uuiddir  repo ^ Ymd.(Id.to_string ymd.meta.uuid) ^ extension

module Entry = struct
  type t = { filepath : string; meta : Ymd.meta; body : string option }

  let of_file s =
    let segments = Re_str.(split (regexp "^---$")) (File.load s) in
    let open Ymd in
    if List.length segments = 2 then
      let yaml_str = List.nth segments 0 in
      let md_str = List.nth segments 1 in
      let m = meta_of_yaml yaml_str in
      { filepath = s; meta = m; body = Some md_str }
    else
      { filepath = s; meta = blank_meta (); body = Some "Error parsing file" }

  let to_file config ymd =
    let repo = Configuration.(config.repository) in
    let uuid_path = uuid_path repo ymd in
    let write_ymd out = Lwt_io.write out (Ymd.to_string ymd) in
    Lwt_io.with_file ~mode:Lwt_io.output uuid_path write_ymd

  let to_ymd entry = { Ymd.meta = entry.meta; Ymd.body = match entry.body with Some b -> b | None -> "" }
end

let slug_of_filename filename = List.hd @@ BatString.split_on_char '.' filename

let rec next_semantic_filepath ?(version=0) titles ymd =
  let candidate = titles ^ (Ymd.filename ymd) ^ "." ^ (string_of_int version) ^ extension in
  if Sys.file_exists candidate then next_semantic_filepath ~version:(version+1) titles ymd
  else candidate

module Archive = struct
  type t = Entry.t list

  let of_repo ?(bodies=false) repo =
    let files = Array.to_list @@ Sys.readdir (titledir repo) in
    let ymd_list a e = if BatString.ends_with e extension then List.cons e a else a in
    let ymds = List.fold_left ymd_list [] files in
    let t y =
      let entry = Entry.of_file (titledir repo ^ y) in
      Entry.({ entry with body = if bodies then entry.body else None })
    in
    List.map t ymds

  let add config ymd =
    let open Lwt.Infix in
    Entry.to_file config ymd >>= fun () ->
    let open Ymd in
    if not (categorised [Category.Draft] ymd) && ymd.meta.title <> "" then
      let archive_path = config.Configuration.repository in
      let archive = of_repo archive_path in
      let dir = titledir archive_path in
      begin try
          let entry = List.find (fun entry -> entry.Entry.meta.uuid = ymd.meta.uuid) archive in
          if slug_of_filename entry.filepath <> (Ymd.filename ymd) then
            let found_filepath = dir ^ entry.filepath in
            Lwt_unix.rename found_filepath (next_semantic_filepath dir ymd);
          else Lwt.return ()
        with Not_found ->
          Lwt_unix.link (uuid_path archive_path ymd) (next_semantic_filepath dir ymd);
      end
    else
      Lwt.return ()
end

let latest_entry config fragment =
  let repo = Configuration.(config.repository) in
  let latest p entry' =
    let open Entry in
    if not @@ BatString.exists (entry'.meta.title) fragment then None
    else
      match p with
      | Some entry ->
         if entry.meta.date.Ymd.Date.published < entry'.meta.date.Ymd.Date.published
         then Some entry' else p
      | None -> Some entry' in
  ListLabels.fold_left ~f:latest ~init:(None) (Archive.of_repo repo)
