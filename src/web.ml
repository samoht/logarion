open Opium.Std

module Configuration = struct
  type template_set = {
      header : Fpath.t option;
      index  : Fpath.t option;
      listing: Fpath.t option;
      listing_entry : Fpath.t option;
      text : Fpath.t option;
    }

  let default_template_set = {
      header = None;
      index = None;
      listing = None;
      listing_entry = None;
      text = None
    }

  type t = {
      url : string;
      port : int;
      stylesheets : Fpath.t list;
      static_dir : Fpath.t;
      template : template_set;
    }

  let default = {
      url = "";
      port = 3666;
      stylesheets = [];
      static_dir = Fpath.v "/usr/share/logarion/static";
      template = default_template_set;
    }

  let of_toml_file fn =
    let result = Toml.Parser.from_filename (Path.string_of_config fn) in
    match result with
    | `Error (str, loc) -> default
    | `Ok toml ->
       let module LT = Logarion_toml in
       let path_tpl = LT.path_opt toml "templates" in
       {
         url = LT.str toml "general" "url" default.url;
         port = LT.int toml "general" "port" default.port;
         stylesheets = LT.paths toml "general" "stylesheets" default.stylesheets;
         static_dir  = LT.path  toml "general" "static_dir"  default.static_dir;
         template = {
             header = path_tpl "header";
             index = path_tpl "index";
             listing = path_tpl "listing";
             listing_entry = path_tpl "listing_entry";
             text = path_tpl "text";
           }
       }
end

let note_of_body_pairs pairs =
  let open Lens.Infix in
  ListLabels.fold_left ~f:(fun a (k,vl) -> Note.with_kv a (k, List.hd vl) ) ~init:(Note.blank ()) pairs
  |> ((Note.Lens.meta |-- Meta.Lens.date |-- Meta.Date.Lens.edited) ^= Some (Ptime_clock.now ()))

let note_of_req req =
  Lwt.map note_of_body_pairs (App.urlencoded_pairs_of_body req)

let string_response s = `String s |> respond'
let html_response   h = `Html h |> respond'
let optional_html_response = function Some h -> html_response h | None -> html_response "Not found"

let () =
  Random.self_init();

  let wcfg = try Configuration.of_toml_file (Path.from_config_paths "web.toml") with Not_found -> Configuration.default in
  let option_load tpl o = match o with Some f -> Some (tpl f) | None -> None in
  let header_tpl = option_load Template.header Configuration.(wcfg.template.header) in
  let listing_tpl = option_load Template.listing Configuration.(wcfg.template.listing) in
  let entry_tpl = option_load Template.listing_entry Configuration.(wcfg.template.listing_entry) in
  let text_tpl = option_load Template.text Configuration.(wcfg.template.text) in
  let blog_url = Configuration.(wcfg.url) in

  let lgrn =
    let open Logarion.Configuration in
    try of_toml_file (Path.from_config_paths "logarion.toml") with Not_found -> default ()
  in
  let page_of_msg = Html.of_message ~header_tpl blog_url lgrn in
  let page_of_note = Html.of_note ~header_tpl ~text_tpl blog_url lgrn in
  let form_of_note = Html.form ~header_tpl blog_url lgrn in
  let list_of_notes = Html.of_entries ~header_tpl ~listing_tpl ~entry_tpl blog_url lgrn in

  let module L = Logarion in

  let lwt_archive repo = Lwt.return L.Archive.(of_repo repo) in
  let lwt_blanknote () = Lwt.return (Note.blank ()) in

  let (>>=) = Lwt.(>>=) and (>|=) = Lwt.(>|=) in
  let atom_response repo req =
    lwt_archive repo >|= L.Archive.latest_listed
    >|= Atom.feed repo wcfg.Configuration.url lgrn >>= html_response in
  let post_note repo req = note_of_req req >>= (fun note -> L.Archive.delta_of repo note |> File.Lwt.with_note note) >|= page_of_note >>= html_response in
  let some_note converter par_name repo selector req =
    let selector x = try selector repo x with Sys_error _ -> None in
    param req par_name |> Lwt.return >|= selector >>=
      (function Some entry ->
                (try File.note entry.L.Entry.path |> Lwt.return >|= converter
                 with Sys_error _ -> Lwt.return @@ page_of_msg "Failed" "Conversion failure")
              | None -> Lwt.return @@ page_of_msg "Not found" "Article not found")
    >>= html_response
  in
  let edit_note = some_note form_of_note in
  let view_note = some_note page_of_note in

  let repo = lgrn.L.Configuration.repository in
  App.empty
  |> App.port wcfg.Configuration.port
  |> middleware @@
       Middleware.static
         ~local_path:(Fpath.to_string wcfg.Configuration.static_dir)
         ~uri_prefix:"/static"
  |> get "/:ttl"      @@ view_note "ttl" repo L.entry_with_slug
  |> post "/post.note" @@ post_note repo
  |> get "/edit.note/:ttl" @@ edit_note "ttl" repo L.entry_with_slug
  |> get "/new.note"   (fun _ -> lwt_blanknote () >|= form_of_note >>= html_response)
  |> get "/note/:ttl" @@ view_note "ttl" repo L.entry_with_slug
  |> get "/!/:ttl"    @@ view_note "ttl" repo L.latest_entry
  |> get "/feed.atom" @@ atom_response repo
  |> get "/"          (fun _ -> Lwt.return list_of_notes >>= html_response)
  |> App.run_command
