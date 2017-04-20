module Id = Meta.Id

module Configuration = struct
  type t = {
      repository : Path.repo_t;
      title : string;
      owner : string;
      email : string;
      id : Id.t;
    }

  let default ?(id=(Id.generate ())) () = {
      repository = Path.repo_of_string (Sys.getcwd ());
      title = "Logarion journal";
      owner = "";
      email = "";
      id;
    }

  let of_toml_file path =
    let result = Toml.Parser.from_filename (Path.string_of_config path) in
    match result with
    | `Error (str, loc) -> default ()
    | `Ok toml ->
       let str = Logarion_toml.str toml "general" in
       let default = default () in
       let default_repo = default.repository |> Path.string_of_repo in
       {
         repository = Path.repo_of_string (str "repository" default_repo);
         title = str "title" default.title;
         owner = str "owner" default.owner;
         email = str "email" default.email;
         id = match Id.of_string (str "uuid" "") with Some id -> id | None -> Id.generate();
       }
end

module Entry = struct
  type t = { path : Path.note_t; attributes : Meta.t } [@@deriving lens { submodule = true }]

  open Meta
  let title e = e.attributes.title
  let date e = e.attributes.date
  let date_edited e = (date e).Date.edited
  let date_published e = (date e).Date.published
  let author_name e = e.attributes.author.Author.name
  let author_email e = e.attributes.author.Author.email
  let published e = CategorySet.published e.attributes.categories
  let listed e = CategorySet.listed e.attributes.categories

  let of_path (path : Path.note_t) =
    let note = File.note path in
    { path; attributes = { note.Note.meta with title = Note.title note } }

  let slug entry = Path.slug_of_note entry.path

  let compare_recency a b = Date.compare (date b) (date a)
end

module Archive = struct
  type t = Entry.t list

  let latest = List.fast_sort Entry.compare_recency
  let listed = List.filter Entry.listed
  let published = List.filter Entry.published

  let of_repo repo =
    let files = File.notes_of_repo repo in
    let to_entry basename = Entry.of_path Path.(note_of_basename repo basename) in
    let fold_file a file =
      if BatString.ends_with file Path.extension
      then try List.cons (to_entry file) a with Note.Syntax_error str -> prerr_endline str; a
      else a
    in
    List.fold_left fold_file [] files

  let delta_of repo note =
    let open Note in
    let open Entry in
    let open Meta in
    let identical entry = entry.attributes.uuid = note.meta.uuid in
    let next_basename title = Path.versioned_basename_of_title repo title in
    let next entry = if title entry <> note.meta.title then next_basename note.meta.title else entry.path in
    match List.find identical (of_repo repo) with
    | entry -> Some entry.path, next entry
    | exception Not_found -> None, Path.versioned_basename_of_title repo note.meta.title

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
  try Some (of_path (Path.note_of_slug repo s))
  with _ ->
    let slugged last_match entry =
      if s <> Path.basename_of_title (title entry) then last_match
      else
        match last_match with
        | Some last_entry ->
           if published last_entry >= published entry then last_match else Some entry
        | None -> Some entry in
    ListLabels.fold_left ~f:slugged ~init:(None) (Archive.of_repo repo)
