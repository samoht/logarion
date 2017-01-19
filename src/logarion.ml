module Id = struct
  include Ymd.Id
end

type repodir_t = Repodir of string
type uuiddir_t = UUIDdir of string
type titledir_t = Titledir of string
type articlefilename_t = Articlefilename of string

let repodir_string = function Repodir path -> path
let uuiddir_string = function UUIDdir path -> path
let titledir_string = function Titledir path -> path
let articlefilename_string = function Articlefilename path -> path

module Configuration = struct
  type t = {
      repository : repodir_t;
      title : string;
      owner : string;
      email : string;
      id : Id.t;
    }

  let default ?(id=(Id.generate ())) () = {
      repository = Repodir (Sys.getcwd ());
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
         repository = Repodir (str "repository" (repodir_string default.repository));
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

let titledir (dir : repodir_t) = Titledir (repodir_string dir ^ "/title/")
let uuiddir  (dir : repodir_t) = UUIDdir  (repodir_string dir ^ "/uuid/")

let extension = ".ymd"

let article_path (repo : repodir_t) articlepath =
  let titledir = titledir_string (titledir repo) in
  Articlefilename (titledir ^ articlefilename_string articlepath)

let title_path (repo : repodir_t) title =
  let titledir = titledir_string (titledir repo) in
  Articlefilename (titledir ^ Ymd.filename_of_title title ^ extension)

let uuid_path  (repo : repodir_t) ymd =
  let uuiddir = uuiddir_string (uuiddir repo) in
  Articlefilename (uuiddir ^ Ymd.(Id.to_string ymd.meta.Meta.uuid) ^ extension)

let slug string = Filename.(string |> basename |> chop_extension)

module Entry = struct
  open Ymd.Meta
  type t = { filename : articlefilename_t; attributes : Ymd.Meta.t } [@@deriving lens]

  let title e = e.attributes.title
  let date e = e.attributes.date
  let date_edited e = e.attributes.date.Ymd.Date.edited
  let date_published e = e.attributes.date.Ymd.Date.published
  let published e = Ymd.CategorySet.published e.attributes.categories
  let listed e = Ymd.CategorySet.listed e.attributes.categories
  let author_name e = e.attributes.author.Ymd.Author.name
  let author_email e = e.attributes.author.Ymd.Author.email

  let of_filename repo (s : articlefilename_t) =
    let ymd = File.ymd (articlefilename_string (article_path repo s)) in
    { filename = s; attributes = ymd.Ymd.meta }

  let to_filename repo ymd =
    let uuid_path = articlefilename_string @@ uuid_path repo ymd in
    let write_ymd out = Lwt_io.write out (Ymd.to_string ymd) in
    Lwt_io.with_file ~mode:Lwt_io.output uuid_path write_ymd

  let to_ymd repo entry = File.ymd (articlefilename_string (article_path repo entry.filename))

  let slug entry =
    Filename.(entry.filename |> articlefilename_string |> basename |> chop_extension)

  let compare_recency a b = Ymd.Date.compare (date b) (date a)
end

let rec next_semantic_filepath ?(version=0) titles ymd =
  let candidate = titledir_string titles ^ (Ymd.filename ymd) ^ "." ^ (string_of_int version) ^ extension in
  if Sys.file_exists candidate then next_semantic_filepath ~version:(version+1) titles ymd
  else candidate

module Archive = struct
  type t = Entry.t list

  let latest = List.fast_sort Entry.compare_recency
  let listed = List.filter Entry.listed
  let published =  List.filter Entry.published

  let of_repo repo =
    let files = Array.to_list @@ Sys.readdir (titledir_string (titledir repo)) in
    let to_entry y = Entry.of_filename repo (Articlefilename y) in
    let fold_file a file =
      if BatString.ends_with file extension
      then try List.cons (to_entry file) a with Ymd.Syntax_error str -> prerr_endline str; a
      else a
    in
    List.fold_left fold_file [] files

  let add repo ymd =
    let open Entry in
    let open Lwt.Infix in
    to_filename repo ymd >>= fun () ->
    let open Ymd in
    (if not (categorised [Category.Draft] ymd) && ymd.meta.Meta.title <> "" then
       let entries = of_repo repo in
       let titledir = titledir repo in
       begin try
           let uuid x = x.Meta.uuid in
           let entry = List.find (fun entry -> uuid entry.attributes = uuid ymd.meta) entries in
           if slug entry <> filename ymd then
             let found_filepath = articlefilename_string (article_path repo entry.filename) in
             Lwt_unix.rename found_filepath (next_semantic_filepath titledir ymd)
           else
             Lwt.return_unit
         with Not_found ->
           Lwt_unix.link (articlefilename_string (uuid_path repo ymd)) (next_semantic_filepath titledir ymd);
       end
     else
       Lwt.return_unit)
    >>= fun () -> Lwt.return ymd
      

  let topics archive =
    let open List in
    let rec unique_entry ts = function
      | h :: t -> unique_entry (if not (exists (fun x -> x = h) ts) then cons h ts else ts) t
      | [] -> ts
    in
    let unique_topics ts x = unique_entry ts x.Entry.attributes.Ymd.Meta.topics in
    fold_left unique_topics [] archive

  let latest_listed entries = entries |> listed |> latest
end

let latest_entry repo fragment =
  let latest last_match entry =
    let open Entry in
    if not @@ BatString.exists (title entry) fragment then last_match
    else
      match last_match with
      | Some last_entry ->
         if date_published last_entry >= date_published entry then last_match else Some entry
      | None -> Some entry in
  ListLabels.fold_left ~f:latest ~init:(None) (Archive.of_repo repo)

let entry_with_slug repo (slug as s) =
  let open Entry in
  try Some (of_filename repo (Articlefilename (s ^ extension)))
  with _ ->
    let slugged last_match entry =
      if s <> Ymd.filename_of_title (title entry) then last_match
      else
        match last_match with
        | Some last_entry ->
           if published last_entry >= published entry then last_match else Some entry
        | None -> Some entry in
    ListLabels.fold_left ~f:slugged ~init:(None) (Archive.of_repo repo)
