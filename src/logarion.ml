module Id = Meta.Id

type repo_t = Repodir of Fpath.t
type uuid_t = UUIDdir of Fpath.t
type titles_t = Titles of Fpath.t
type article_t = Article of Fpath.t

let repo_path = function Repodir path -> path
let uuiddir_path = function UUIDdir path -> path
let titledir_path = function Titles path -> path
let articlefilename_path = function Article path -> path

module Configuration = struct
  type t = {
      repository : repo_t;
      title : string;
      owner : string;
      email : string;
      id : Id.t;
    }

  let default ?(id=(Id.generate ())) () = {
      repository = Repodir Fpath.(v (Sys.getcwd ()));
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
       let default_repo = default.repository |> repo_path |> Fpath.to_string in
       {
         repository = Repodir (str "repository" default_repo |> Fpath.v);
         title = str "title" default.title;
         owner = str "owner" default.owner;
         email = str "email" default.email;
         id = match Id.of_string (str "uuid" "") with Some id -> id | None -> Id.generate();
       }
end

module File = struct
  let load f =
    let ic = open_in (Fpath.to_string f) in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    (s)

  let note f = Note.of_string (load f)

  let name_of_title t =
    let is_reserved = function
      | '!' | '*' | '\'' | '(' | ')' | ';' | ':' | '@' | '&' | '=' | '+' | '$'
        | ',' | '/' | '?' | '#' | '[' | ']' | ' ' | '\t' | '\x00' -> true
      | _ -> false in
    let drop h t = t in
    let dash h t = '-' :: t in
    let rec filter fn = function
      | [] -> []
      | head :: tail ->
         if is_reserved head
         then fn head (filter drop tail)
         else Char.lowercase_ascii head :: (filter dash tail) in
    Batteries.String.of_list @@ filter drop (Batteries.String.to_list t)

  let name note = name_of_title note.Note.meta.Meta.title
end

let titledir (dir : repo_t) = Titles Fpath.(repo_path dir / "title")
let uuiddir  (dir : repo_t) = UUIDdir  Fpath.(repo_path dir / "uuid")

let extension = ".ymd"

let article_path (repo : repo_t) articlepath =
  Article Fpath.(repo_path repo / "title" // articlefilename_path articlepath)
let title_path (repo : repo_t) title =
  Article Fpath.(repo_path repo / "title" / (File.name_of_title title ^ extension))
let uuid_path  (repo : repo_t) note =
  Article Fpath.(repo_path repo / "uuid"  / (Meta.Id.to_string note.Note.meta.Meta.uuid ^ extension))

let slug string = Filename.(string |> basename |> chop_extension)

module Entry = struct
  type t = { filename : article_t; attributes : Meta.t } [@@deriving lens { submodule = true }]

  open Meta
  let title e = e.attributes.title
  let date e = e.attributes.date
  let date_edited e = (date e).Date.edited
  let date_published e = (date e).Date.published
  let author_name e = e.attributes.author.Author.name
  let author_email e = e.attributes.author.Author.email
  let published e = CategorySet.published e.attributes.categories
  let listed e = CategorySet.listed e.attributes.categories

  let of_filename repo (filename : article_t) =
    let note = File.note (articlefilename_path (article_path repo filename)) in
    let attributes = { note.Note.meta with title = Note.title note } in
    { filename; attributes }

  let to_filename repo note =
    let uuid_path = Fpath.to_string @@ articlefilename_path @@ uuid_path repo note in
    let write_note out = Lwt_io.write out (Note.to_string note) in
    Lwt_io.with_file ~mode:Lwt_io.output uuid_path write_note

  let to_ymd repo entry = File.note (articlefilename_path (article_path repo entry.filename))

  let slug entry =
    Fpath.(entry.filename |> articlefilename_path |> base |> rem_ext |> to_string)

  let compare_recency a b = Date.compare (date b) (date a)
end

let rec next_semantic_filepath ?(version=0) titles note =
  let candidate =
    let open Fpath in
    titledir_path titles / (File.name note)
    |> add_ext (string_of_int version)
    |> add_ext extension in
  if Sys.file_exists Fpath.(to_string candidate) then next_semantic_filepath ~version:(version+1) titles note
  else candidate

module Archive = struct
  type t = Entry.t list

  let latest = List.fast_sort Entry.compare_recency
  let listed = List.filter Entry.listed
  let published = List.filter Entry.published

  let of_repo repo =
    let files = Array.to_list @@ Sys.readdir Fpath.(to_string @@ titledir_path (titledir repo)) in
    let to_entry y = Entry.of_filename repo (Article (Fpath.v y)) in
    let fold_file a file =
      if BatString.ends_with file extension
      then try List.cons (to_entry file) a with Note.Syntax_error str -> prerr_endline str; a
      else a
    in
    List.fold_left fold_file [] files

  let add repo note =
    let open Entry in
    let open Lwt.Infix in
    to_filename repo note >>= fun () ->
    let open Note in
    (if not (categorised [Meta.Category.Draft] note) && note.Note.meta.Meta.title <> "" then
       let entries = of_repo repo in
       let titledir = titledir repo in
       begin try
           let uuid x = x.Meta.uuid in
           let entry = List.find (fun entry -> uuid entry.attributes = uuid note.meta) entries in
           if slug entry <> File.name note then
             let found_filepath = Fpath.to_string @@ articlefilename_path (article_path repo entry.filename) in
             Lwt_unix.rename found_filepath (Fpath.to_string @@ next_semantic_filepath titledir note)
           else
             Lwt.return_unit
         with Not_found ->
           Lwt_unix.link (Fpath.to_string @@ articlefilename_path (uuid_path repo note)) (Fpath.to_string @@ next_semantic_filepath titledir note);
       end
     else
       Lwt.return_unit)
    >>= fun () -> Lwt.return note

  let topics archive =
    let open List in
    let rec unique_entry ts = function
      | h :: t -> unique_entry (if not (exists (fun x -> x = h) ts) then cons h ts else ts) t
      | [] -> ts
    in
    let unique_topics ts x = unique_entry ts x.Entry.attributes.Meta.topics in
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
  try Some (of_filename repo (Article (Fpath.v @@ s ^ extension)))
  with _ ->
    let slugged last_match entry =
      if s <> File.name_of_title (title entry) then last_match
      else
        match last_match with
        | Some last_entry ->
           if published last_entry >= published entry then last_match else Some entry
        | None -> Some entry in
    ListLabels.fold_left ~f:slugged ~init:(None) (Archive.of_repo repo)
