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
  |> ((ymd_meta |-- Meta.lens_date |-- Date.edited) ^= Some (Ptime_clock.now ()))

let ymd_of_req req =
  Lwt.map ymd_of_body_pairs (App.urlencoded_pairs_of_body req)

let string_response s = `String s |> respond'
let html_response h = `Html h |> respond'

let unpublished_entry = Logarion.(Entry.({ filename = Articlefilename ""; attributes = Ymd.Meta.blank () }))
let entry_option y = match y with Some entry -> entry | None -> unpublished_entry

let webcfg = Configuration.of_filename "web.toml"
let lgrn = Logarion.Configuration.of_filename "logarion.toml"

let () =
  Random.self_init();
  let (>>=) = Lwt.(>>=)
  and (>|=) = Lwt.(>|=) in
  let module L = Logarion in
  let ymd f =
    try
      L.Entry.of_filename lgrn.L.Configuration.repository f
      |> (fun entry -> if Ymd.(CategorySet.categorised [Category.Published]) entry.L.Entry.attributes.Ymd.Meta.categories
                       then entry else unpublished_entry)
    with Sys_error _ -> unpublished_entry
  in
  let ymdpath title = Lwt.return @@ Logarion.title_path lgrn.L.Configuration.repository title in
  let ret_param name req = Lwt.return (param req name) in
  let option_load tpl o = match o with Some f -> Some (tpl f) | None -> None in
  let header_tpl = option_load Template.header Configuration.(webcfg.template.header) in
  let listing_tpl = option_load Template.listing Configuration.(webcfg.template.listing) in
  let entry_tpl = option_load Template.listing_entry Configuration.(webcfg.template.listing_entry) in
  let text_tpl = option_load Template.text Configuration.(webcfg.template.text) in
  let blog_url = Configuration.(webcfg.url) in
  let page_of_ymd = Html.of_ymd ~header_tpl ~text_tpl blog_url lgrn in
  let form_of_ymd = Html.form ~header_tpl blog_url lgrn in
  let list_of_ymds = Html.of_entries ~header_tpl ~listing_tpl ~entry_tpl blog_url lgrn in
  let repo = lgrn.L.Configuration.repository in
  App.empty
  |> App.port webcfg.Configuration.port
  |> middleware @@ Middleware.static ~local_path:"./share/static" ~uri_prefix:"/static"
  |> post "/post"     (fun req -> ymd_of_req req >>= fun ymd -> L.Archive.add repo ymd >>= fun () -> html_response (page_of_ymd ymd))
  |> get "/edit/:ttl" (fun r   -> ret_param "ttl" r >>= ymdpath >|= ymd >|= L.Entry.to_ymd repo >|= form_of_ymd >>= html_response)
  |> get "/new"       (fun _   -> Lwt.return (Ymd.blank ()) >|= form_of_ymd >>= html_response)
  |> get "/text/:ttl" (fun req -> ret_param "ttl" req >|= L.entry_with_slug repo >|= entry_option >|= L.Entry.to_ymd repo >|= page_of_ymd >>= html_response)
  |> get "/!/:ttl"    (fun req -> ret_param "ttl" req >|= L.latest_entry repo >|= entry_option >|= L.Entry.to_ymd repo >|= page_of_ymd >>= html_response)
  |> get "/feed.atom" (fun _   -> Lwt.return L.Archive.(of_repo repo) >|= L.Archive.latest_listed >|= List.map (L.Entry.to_ymd repo) >|= Atom.feed webcfg.Configuration.url lgrn >>= html_response)
  |> get "/"          (fun _   -> Lwt.return list_of_ymds >>= html_response)
  |> App.run_command
