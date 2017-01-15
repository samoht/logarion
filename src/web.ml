open Opium.Std

module Configuration = struct
  type template_set = {
      header : string option;
      index : string option;
      listing : string option;
      listing_entry : string option;
      text : string option;
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
      stylesheets : string list;
      template : template_set;
    }

  let default = {
      url = "";
      port = 3666;
      stylesheets = [];
      template = default_template_set;
    }

  let of_filename fn =
    let result = Toml.Parser.from_filename fn in
    match result with
    | `Error (str, loc) -> default
    | `Ok toml ->
       let module LT = Logarion_toml in
       let str_tpl = LT.str_opt toml "templates" in
       {
         url = LT.str toml "general" "url" default.url;
         port = LT.int toml "general" "port" default.port;
         stylesheets = LT.strs toml "general" "stylesheets" default.stylesheets;
         template = {
             header = str_tpl "header";
             index = str_tpl "index";
             listing = str_tpl "listing";
             listing_entry = str_tpl "listing_entry";
             text = str_tpl "text";
           }
       }
end

let ymd_of_body_pairs pairs =
  let open Ymd in
  let open Lens.Infix in
  ListLabels.fold_left ~f:(fun a (k,vl) -> with_kv a (k, List.hd vl) ) ~init:(blank ()) pairs
  |> ((ymd_meta |-- Meta.lens_date |-- Date.lens_edited) ^= Some (Ptime_clock.now ()))

let ymd_of_req req =
  Lwt.map ymd_of_body_pairs (App.urlencoded_pairs_of_body req)

let string_response s = `String s |> respond'
let html_response   h = `Html h |> respond'

module L = Logarion

let unpublished_entry =
  L.(Entry.({ filename = Articlefilename ""; attributes = Ymd.Meta.blank () }))

let entry_option y = match y with Some entry -> entry | None -> unpublished_entry

let ymd repo f =
  try L.Entry.of_filename repo f |> (fun entry -> if L.Entry.listed entry then entry else unpublished_entry)
  with Sys_error _ -> unpublished_entry

let () =
  Random.self_init();

  let wcfg = Configuration.of_filename "web.toml" in
  let option_load tpl o = match o with Some f -> Some (tpl f) | None -> None in
  let header_tpl = option_load Template.header Configuration.(wcfg.template.header) in
  let listing_tpl = option_load Template.listing Configuration.(wcfg.template.listing) in
  let entry_tpl = option_load Template.listing_entry Configuration.(wcfg.template.listing_entry) in
  let text_tpl = option_load Template.text Configuration.(wcfg.template.text) in
  let blog_url = Configuration.(wcfg.url) in

  let lgrn = Logarion.Configuration.of_filename "logarion.toml" in
  let page_of_ymd = Html.of_ymd ~header_tpl ~text_tpl blog_url lgrn in
  let form_of_ymd = Html.form ~header_tpl blog_url lgrn in
  let list_of_ymds = Html.of_entries ~header_tpl ~listing_tpl ~entry_tpl blog_url lgrn in

  let lwt_param name req = Lwt.return (param req name) in
  let lwt_archive repo = Lwt.return L.Archive.(of_repo repo) in
  let lwt_blankymd () = Lwt.return (Ymd.blank ()) in

  let (>>=) = Lwt.(>>=) and (>|=) = Lwt.(>|=) in
  let page_of_slug repo s = entry_option s |> L.Entry.to_ymd repo |> Lwt.return in
  let view_page repo slug = page_of_slug repo slug >|= page_of_ymd in
  let edit_page repo slug = page_of_slug repo slug >|= form_of_ymd in
  let post_page repo ymd = L.Archive.add repo ymd >|= page_of_ymd in
  let atom_page repo entries = L.Archive.latest_listed entries |> Lwt.return >|= List.map (L.Entry.to_ymd repo) in

  let repo = lgrn.L.Configuration.repository in
  
  App.empty
  |> App.port wcfg.Configuration.port
  |> middleware @@ Middleware.static ~local_path:"./share/static" ~uri_prefix:"/static"
  |> post "/post"     (fun r -> ymd_of_req r      >>= post_page repo >>= html_response)
  |> get "/edit/:ttl" (fun r -> lwt_param "ttl" r >|= L.entry_with_slug repo >>= edit_page repo >>= html_response)
  |> get "/new"       (fun _ -> lwt_blankymd ()   >|= form_of_ymd >>= html_response)
  |> get "/text/:ttl" (fun r -> lwt_param "ttl" r >|= L.entry_with_slug repo >>= view_page repo >>= html_response)
  |> get "/!/:ttl"    (fun r -> lwt_param "ttl" r >|= L.latest_entry repo    >>= view_page repo >>= html_response)
  |> get "/feed.atom" (fun _ -> lwt_archive repo  >>= atom_page repo >|= Atom.feed wcfg.Configuration.url lgrn >>= html_response)
  |> get "/"          (fun _ -> Lwt.return list_of_ymds >>= html_response)
  |> App.run_command
