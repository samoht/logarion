open Opium.Std

module Configuration = struct
  type t = {
      url      : string;
      port     : int;
      static   : Fpath.t;
      styles   : Fpath.t list;
      template : Template.Configuration.paths_t;
    }

  let default = {
      url      = "";
      port     = 3666;
      static   = Fpath.v "/usr/share/logarion/static";
      styles   = [];
      template = Template.Configuration.default_paths;
    }

  let of_toml_file fn =
    let result = Toml.Parser.from_filename (Lpath.string_of_config fn) in
    match result with
    | `Error (str, loc) -> default
    | `Ok toml ->
       let open Logarion_toml in
       {
         url    = str   toml "general" "url"         default.url;
         port   = int   toml "general" "port"        default.port;
         static = path  toml "general" "static_dir"  default.static;
         styles = paths toml "general" "stylesheets" default.styles;
         template = Template.Configuration.of_toml_file toml
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
  let module L = Logarion in
  Random.self_init();

  let wcfg =
    try Configuration.of_toml_file (Lpath.from_config_paths "web.toml")
    with Not_found -> Configuration.default in
  let config =
    let open L.Configuration in
    try of_toml_file (Lpath.from_config_paths "logarion.toml")
    with Not_found -> default ()
  in
  let module L = Logarion.Make(File) in
  let store = File.store config.repository in
  let lgrn = L.{ config; store; } in

  let header_tpl = Template.header wcfg.Configuration.template in
  let list_tpl   = Template.list wcfg.Configuration.template in
  let item_tpl   = Template.item wcfg.Configuration.template in
  let note_tpl   = Template.note wcfg.Configuration.template in

  let blog_url = Configuration.(wcfg.url) in
  let page_of_msg   = Html.of_message ~header_tpl blog_url config in
  let page_of_note  = Html.of_note    ~header_tpl ~note_tpl blog_url config in
  let form_of_note  = Html.form       ~header_tpl blog_url config in
  let list_of_notes = Html.of_entries ~header_tpl ~list_tpl ~item_tpl blog_url config in

  let lwt_blanknote () = Lwt.return (Note.blank ()) in

  let (>>=) = Lwt.(>>=) and (>|=) = Lwt.(>|=) in
  let atom_response repo req =
    Lwt.return (L.latest_listed repo)
    >|= Atom.feed config wcfg.Configuration.url (L.note_with_id lgrn)
    >>= html_response
  in
  let post_note lgrn req =
    note_of_req req
    >>= L.with_note lgrn
    >|= page_of_note
    >>= html_response
  in

  let some_note converter par_name lgrn find_note req =
    param req par_name
    |> Lwt.return
    >|= find_note
    >>= (function
         | Some note -> Lwt.return note >|= converter
         | None -> Lwt.return @@ page_of_msg "Not found" "Article not found")
    >>= html_response
  in
  let edit_note = some_note form_of_note in
  let view_note = some_note page_of_note in

  App.empty
  |> App.port wcfg.Configuration.port
  |> middleware @@
       Middleware.static
         ~local_path:(Fpath.to_string wcfg.Configuration.static)
         ~uri_prefix:"/static"
  |> get "/:ttl"      @@ view_note "ttl" lgrn (L.note_with_alias lgrn)
  |> post "/post.note" @@ post_note lgrn
  |> get "/edit.note/:ttl" @@ edit_note "ttl" lgrn (L.note_with_alias lgrn)
  |> get "/new.note"   (fun _ -> lwt_blanknote () >|= form_of_note >>= html_response)
  |> get "/note/:ttl" @@ view_note "ttl" lgrn (L.note_with_alias lgrn)
  |> get "/!/:ttl"    @@ view_note "ttl" lgrn (fun t -> match L.latest_entry lgrn t with
                                                        | Some meta -> L.note_with_id lgrn meta.Meta.uuid
                                                        | None -> None)
  |> get "/feed.atom" @@ atom_response lgrn
  |> get "/"          (fun _ -> Lwt.return (L.latest_listed lgrn) >|= list_of_notes >>= html_response)
  |> App.run_command
