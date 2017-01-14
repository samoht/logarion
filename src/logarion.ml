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

  let ymd f = Ymd.of_string (load f)
end

let titledir ymddir = ymddir ^ "/title/"
let uuiddir  ymddir = ymddir ^ "/uuid/"
let extension = ".ymd"
let title_path repo title = titledir repo ^ Ymd.filename_of_title title ^ extension
let uuid_path  repo ymd   = uuiddir  repo ^ Ymd.(Id.to_string ymd.meta.Meta.uuid) ^ extension

module Entry = struct
  open Ymd.Meta
  type t = { filepath : string; attributes : Ymd.Meta.t } [@@deriving lens]

  let title entry = entry.attributes.title
  let date entry = entry.attributes.date
  let published entry = entry.attributes.date.Ymd.Date.published

  let of_file s =
    let ymd = File.ymd s in
    { filepath = s; attributes = ymd.Ymd.meta }

  let to_file config ymd =
    let repo = Configuration.(config.repository) in
    let uuid_path = uuid_path repo ymd in
    let write_ymd out = Lwt_io.write out (Ymd.to_string ymd) in
    Lwt_io.with_file ~mode:Lwt_io.output uuid_path write_ymd

  let to_ymd entry = File.ymd entry.filepath
end

let slug_of_filename filename = List.hd @@ BatString.split_on_char '.' filename

let rec next_semantic_filepath ?(version=0) titles ymd =
  let candidate = titles ^ (Ymd.filename ymd) ^ "." ^ (string_of_int version) ^ extension in
  if Sys.file_exists candidate then next_semantic_filepath ~version:(version+1) titles ymd
  else candidate

module Archive = struct
  type t = Entry.t list

  let latest = List.fast_sort (fun b a -> Ymd.Date.compare (Entry.date a) (Entry.date b))
  let listed = List.filter Ymd.(fun a -> not @@ CategorySet.categorised [Category.Unlisted] (a.Entry.attributes.Meta.categories))

  let of_repo repo =
    let files = Array.to_list @@ Sys.readdir (titledir repo) in
    let to_entry y = Entry.of_file (titledir repo ^ y) in
    let fold_file a file =
      if BatString.ends_with file extension
      then try List.cons (to_entry file) a with
             Ymd.Syntax_error str -> prerr_endline str; a
      else a
    in
    List.fold_left fold_file [] files

  let add config ymd =
    let open Lwt.Infix in
    Entry.to_file config ymd >>= fun () ->
    let open Ymd in
    if not (categorised [Category.Draft] ymd) && ymd.meta.Meta.title <> "" then
      let archive_path = config.Configuration.repository in
      let archive = of_repo archive_path in
      let dir = titledir archive_path in
      begin try
          let uuid x = x.Ymd.Meta.uuid in
          let entry = List.find (fun entry -> uuid entry.Entry.attributes = uuid ymd.meta) archive in
          if slug_of_filename entry.Entry.filepath <> filename ymd then
            let found_filepath = dir ^ entry.Entry.filepath in
            Lwt_unix.rename found_filepath (next_semantic_filepath dir ymd);
          else Lwt.return ()
        with Not_found ->
          Lwt_unix.link (uuid_path archive_path ymd) (next_semantic_filepath dir ymd);
      end
    else
      Lwt.return ()

  let topics archive =
    let rec unique_entry ts = function
      | h :: t ->
         let p x = x = h in
         if not (List.exists p ts) then unique_entry (List.cons h ts) t else unique_entry ts t
      | [] -> ts
    in
    let unique_topics ts x = unique_entry ts x.Entry.attributes.Ymd.Meta.topics in
    List.fold_left unique_topics [] archive

  let latest_listed entries = entries |> listed |> latest
end

let latest_entry config fragment =
  let repo = Configuration.(config.repository) in
  let latest last_match entry =
    let open Entry in
    if not @@ BatString.exists (title entry) fragment then last_match
    else
      match last_match with
      | Some last_entry ->
         if published last_entry >= published entry then last_match else Some entry
      | None -> Some entry in
  ListLabels.fold_left ~f:latest ~init:(None) (Archive.of_repo repo)

let entry_with_slug config slug =
  let repo = Archive.of_repo @@ Configuration.(config.repository) in
  let split_slug = BatString.split_on_char '.' slug in
  let open Entry in
  if List.length split_slug > 2 then Some (of_file slug)
  else
    let slug = List.hd split_slug in
    let slugged last_match entry =
      if slug <> Ymd.filename_of_title (title entry) then last_match
      else
        match last_match with
        | Some last_entry ->
           if published last_entry >= published entry then last_match else Some entry
        | None -> Some entry in
    ListLabels.fold_left ~f:slugged ~init:(None) repo
