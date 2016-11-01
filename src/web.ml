open Opium.Std

module Configuration = struct
  type template_set = {
      header : string option;
      index : string option;
      listing: string option;
      text : string option;
    }

  let blank_template_set = { header = None; index = None; listing = None; text = None }

  type t = {
      url : string option;
      stylesheets : string list option;
      template : template_set;
    }

  let of_filename fn =
    let result = Toml.Parser.from_filename fn in
    match result with
    | `Error (str, loc) -> { url = None; stylesheets = None; template = blank_template_set }
    | `Ok tbl ->
       let open TomlLenses in
       let str_of table_name key_name = get tbl (key table_name |-- table |-- key key_name |-- string) in
       let strs_of table_name key_name = get tbl (key table_name |-- table |-- key key_name |-- array |-- strings) in
       {
         url = str_of "general" "url";
         stylesheets = strs_of "general" "stylesheets";
         template = {
             header = str_of "templates" "header";
             index = str_of "templates" "index";
             listing = str_of "templates" "listing";
             text = str_of "templates" "text";
           }
       }
end

let ymdpath title = return @@ "ymd/" ^ (Ymd.filename_of_title title) ^ ".ymd"

let ymd_of_body_pairs pairs =
  let open Ymd in
  let open Lens.Infix in
  ListLabels.fold_left ~f:(fun a (k,vl) -> with_kv a (k, List.hd vl) ) ~init:(blank_ymd ()) pairs
  |> ((ymd_meta |-- meta_date |-- Date.edited) ^= Some (Ptime_clock.now ()))

let ymd_of_req req =
  Lwt.map ymd_of_body_pairs (App.urlencoded_pairs_of_body req)

let string_response s = `String s |> respond'
let html_response h = `Html h |> respond'

let ymd_or_error y = match y with Some (path, meta) -> Logarion.of_file ("ymd/" ^ path) | None -> Ymd.blank_ymd ()

let webcfg = Configuration.of_filename "web.toml"
let lgrn = Logarion.Configuration.of_filename "logarion.toml"

let () =
  Random.self_init();;
  let (>>=) = Lwt.(>>=)
  and (>|=) = Lwt.(>|=) in
  let module L = Logarion in
  let ymd f = L.of_file f in
  let ret_param name req = return (param req name) in
  let option_load o = match o with Some f -> Some (Logarion.load_file f) | None -> None in
  let header_tpl = option_load Configuration.(webcfg.template.header) in
  let listing_tpl = option_load Configuration.(webcfg.template.listing) in
  let text_tpl = option_load Configuration.(webcfg.template.text) in
  let blog_url = match Configuration.(webcfg.url) with Some url -> url | None -> "" in
  let page_of_ymd = Html.of_ymd ~header_tpl ~text_tpl blog_url lgrn in
  let form_of_ymd = Html.form ~header_tpl blog_url lgrn in
  let list_of_ymds = Html.of_file_meta_pairs ~header_tpl ~listing_tpl blog_url lgrn in
  App.empty
  |> middleware @@ Middleware.static ~local_path:"./share/static" ~uri_prefix:"/static"
  |> post "/post"     (fun req -> ymd_of_req req >>= fun ymd -> L.to_file ymd >>= fun () -> html_response (page_of_ymd ymd))
  |> get "/edit/:ttl" (fun r   -> ret_param "ttl" r >>= ymdpath >|= ymd >|= form_of_ymd >>= html_response)
  |> get "/new"       (fun _   -> return (Ymd.blank_ymd ()) >|= form_of_ymd >>= html_response)
  |> get "/text/:ttl" (fun req -> ret_param "ttl" req >>= ymdpath >|= ymd >|= page_of_ymd >>= html_response)
  |> get "/!/:ttl"    (fun req -> ret_param "ttl" req >|= L.latest_file_meta_pair >|= ymd_or_error >|= page_of_ymd >>= html_response)
  |> get "/"          (fun _   -> return (L.file_meta_pairs ()) >|= list_of_ymds >>= html_response)
  |> App.run_command
