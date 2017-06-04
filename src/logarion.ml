module Id = Meta.Id
type alias_t = string

module Configuration = struct
  type t = {
      repository : Lpath.repo_t;
      title : string;
      owner : string;
      email : string;
      id : Id.t;
    }

  let default ?(id=(Id.generate ())) () = {
      repository = Lpath.repo_of_string (Sys.getcwd ());
      title = "Logarion journal";
      owner = "";
      email = "";
      id;
    }

  let of_toml_file path =
    let result = Toml.Parser.from_filename (Lpath.string_of_config path) in
    match result with
    | `Error (str, loc) -> default ()
    | `Ok toml ->
       let str = Logarion_toml.str toml "general" in
       let default = default () in
       let default_repo = default.repository |> Lpath.string_of_repo in
       {
         repository = Lpath.repo_of_string (str "repository" default_repo);
         title = str "title" default.title;
         owner = str "owner" default.owner;
         email = str "email" default.email;
         id = match Id.of_string (str "uuid" "") with Some id -> id | None -> Id.generate();
       }
end

module AliasMap = Meta.AliasMap

module Make (Store : Store.T) = struct
  type t = {
      config : Configuration.t;
      store  : Store.t;
    }

  let note_lens note = note
  let meta_lens note = note.Note.meta

  let recency_order a b = Meta.(Date.compare b.date a.date)

  let latest archive =
    Store.to_list ~order:recency_order meta_lens archive.store

  let listed archive =
    let notes = Store.to_list meta_lens archive.store in
    List.filter Meta.(fun e -> CategorySet.listed e.categories) notes

  let published archive =
    let notes = Store.to_list meta_lens archive.store in
    List.filter Meta.(fun e -> CategorySet.published e.categories) notes

  let latest_listed archive =
    let notes = Store.to_list ~order:recency_order meta_lens archive.store in
    List.filter Meta.(fun e -> CategorySet.listed e.categories) notes

  let topics archive =
    let notes = Store.to_list meta_lens archive.store in
    List.fold_left Meta.(fun a e -> unique_topics a e) Meta.StringSet.empty notes

  let latest_entry archive fragment =
    let notes = Store.to_list ~order:recency_order meta_lens archive.store in
    try Some (List.find (fun e -> BatString.exists (e.Meta.title) fragment) notes)
    with Not_found -> None

  let note_with_id archive id = Store.note_with_id archive.store id
  let note_with_alias archive alias = Store.note_with_alias archive.store alias

  let with_note archive note = Store.with_note archive.store note

  let sublist ~from ~n list = BatList.(take n (drop from list)) 

end
