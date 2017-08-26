open Opium.Std

module Lpath = Logarion.Lpath
module Template = Converters.Template

module Configuration = struct
  type t = {
      url      : Uri.t;
      static   : Fpath.t;
      styles   : Fpath.t list;
      template : Template.Configuration.paths_t;
    }

  let default = {
      url      = Uri.empty;
      static   = Fpath.v "/usr/share/logarion/static";
      styles   = [];
      template = Template.Configuration.default_paths;
    }

  let of_toml_file fn =
    let result = Toml.Parser.from_filename (Lpath.string_of_config fn) in
    match result with
    | `Error (str, loc) -> default
    | `Ok toml ->
       let default_url = Uri.to_string default.url in
       let open Logarion.Config in
       {
         url    = str   toml "general" "url"         default_url |> Uri.of_string;
         static = path  toml "general" "static_dir"  default.static;
         styles = paths toml "general" "stylesheets" default.styles;
         template = Template.Configuration.of_toml_file toml
       }

  let validity config =
    let open Logarion.Config.Validation in
    empty
    &> is_directory config.static
    &&> files_exist ~parent_dir:config.static config.styles
end

let note_of_body_pairs pairs =
  let open Logarion in
  let note = ListLabels.fold_left ~f:(fun a (k,vl) -> Note.with_kv a (k, List.hd vl) ) ~init:(Note.blank ()) pairs in
  let open Meta in
  let open Date in
  { note with meta = { note.meta with date = { note.meta.date with edited = Some (Ptime_clock.now ()) }}}
(*  |> ((Note.Lens.meta |-- Meta.Lens.date |-- Meta.Date.Lens.edited) ^= )*)

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
    with Not_found -> Configuration.default
  in
  Logarion.Config.Validation.terminate_when_invalid (Configuration.validity wcfg);
  let config =
    let open L.Archive.Configuration in
    try of_toml_file (Lpath.from_config_paths "logarion.toml")
    with Not_found -> default ()
  in
  Logarion.Config.Validation.terminate_when_invalid (L.Archive.Configuration.validity config);
  let module L = Logarion.Archive.Make(File) in
  let store = File.store config.repository in
  let lgrn = L.{ config; store; } in

  let header_tpl = Template.header wcfg.Configuration.template in
  let list_tpl   = Template.list wcfg.Configuration.template in
  let item_tpl   = Template.item wcfg.Configuration.template in
  let note_tpl   = Template.note wcfg.Configuration.template in

  let blog_url = Uri.to_string wcfg.Configuration.url in
  let module Html = Converters.Html in
  let page_of_msg   = Html.of_message ~header_tpl blog_url config in
  let page_of_note  = Html.of_note    ~header_tpl ~note_tpl blog_url config in
  let form_of_note  = Html.form       ~header_tpl blog_url config in
  let list_of_notes ~from ~n = Html.of_entries ~header_tpl ~list_tpl ~item_tpl ~from ~n blog_url config in

  let lwt_blanknote () = Lwt.return (Logarion.Note.blank ()) in

  let (>>=) = Lwt.(>>=) and (>|=) = Lwt.(>|=) in
  let atom_response repo req =
    Lwt.return (L.latest_listed repo)
    >|= Converters.Atom.feed config blog_url (L.note_with_id lgrn)
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
  let list_notes param_name lgrn req =
    let n = 16 in
    let from = match Uri.get_query_param (Request.uri req) "p" with
      | Some p -> (try int_of_string p with Failure _ -> 0)
      | None -> 0
    in
    Lwt.return (L.latest_listed lgrn)
    >|= L.sublist ~from:(from * n) ~n
    >|= list_of_notes ~from ~n
    >>= html_response
  in

  App.empty
  |> App.port (match Uri.port wcfg.Configuration.url with Some p -> p | None -> 3666)
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
                                                        | Some meta -> L.note_with_id lgrn meta.Logarion.Meta.uuid
                                                        | None -> None)
  |> get "/feed.atom" @@ atom_response lgrn
  |> get "/"          @@ list_notes "p" lgrn
  |> App.run_command
