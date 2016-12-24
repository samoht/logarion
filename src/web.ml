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
  ListLabels.fold_left ~f:(fun a (k,vl) -> with_kv a (k, List.hd vl) ) ~init:(blank_ymd ()) pairs
  |> ((ymd_meta |-- meta_date |-- Date.edited) ^= Some (Ptime_clock.now ()))

let ymd_of_req req =
  Lwt.map ymd_of_body_pairs (App.urlencoded_pairs_of_body req)

let string_response s = `String s |> respond'
let html_response h = `Html h |> respond'

let unpublished_entry = Logarion.Entry.({ filepath = ""; meta = Ymd.blank_meta (); body = Some "No such published entry"; })
let entry_option y = match y with Some entry -> entry | None -> unpublished_entry

let webcfg = Configuration.of_filename "web.toml"
let lgrn = Logarion.Configuration.of_filename "logarion.toml"

let () =
  Random.self_init();
  let (>>=) = Lwt.(>>=)
  and (>|=) = Lwt.(>|=) in
  let module L = Logarion in
  let ymd f = L.Entry.of_file f |> (fun entry -> if Ymd.(CategorySet.categorised [Category.Published]) entry.meta.categories then entry else unpublished_entry) in
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
  let latest_listed_entries es =
    es
    |> List.filter Ymd.(fun a -> not @@ CategorySet.categorised [Category.Unlisted] a.L.Entry.meta.categories)
    |> List.fast_sort Ymd.(fun b a -> compare (Date.last a.L.Entry.meta.date) (Date.last b.L.Entry.meta.date))
  in
  let repo = lgrn.L.Configuration.repository in
  App.empty
  |> App.port webcfg.Configuration.port
  |> middleware @@ Middleware.static ~local_path:"./share/static" ~uri_prefix:"/static"
  |> post "/post"     (fun req -> ymd_of_req req >>= fun ymd -> L.Entry.to_file lgrn ymd >>= fun () -> html_response (page_of_ymd ymd))
  |> get "/edit/:ttl" (fun r   -> ret_param "ttl" r >>= ymdpath >|= ymd >|= L.Entry.to_ymd >|= form_of_ymd >>= html_response)
  |> get "/new"       (fun _   -> Lwt.return (Ymd.blank_ymd ()) >|= form_of_ymd >>= html_response)
  |> get "/text/:ttl" (fun req -> ret_param "ttl" req >>= ymdpath >|= ymd >|= L.Entry.to_ymd >|= page_of_ymd >>= html_response)
  |> get "/!/:ttl"    (fun req -> ret_param "ttl" req >|= L.latest_entry lgrn >|= entry_option >|= L.Entry.to_ymd >|= page_of_ymd >>= html_response)
  |> get "/feed.atom" (fun _   -> Lwt.return L.Archive.(of_repo ~bodies:true repo) >|= latest_listed_entries >|= List.map L.Entry.to_ymd >|= Atom.feed webcfg.url lgrn >>= html_response)
  |> get "/"          (fun _   -> Lwt.return L.Archive.(of_repo repo) >|= latest_listed_entries >|= list_of_ymds >>= html_response)
  |> App.run_command
