open Opium.Std

module Configuration = struct
  type template_set = {
      header : string;
      index : string;
      listing: string;
      text : string;
    }

  type t = {
      url : string;
      stylesheets : string list;
      templates : template_set;
    }

  let of_filename fn =
    let result = Toml.Parser.from_filename fn in
    match result with
    | `Error (str, loc) -> { url = ""; stylesheets = []; templates = { header = ""; index = ""; listing = ""; text = "" } }
    | `Ok tbl ->
       let str_of table_name key_name = match TomlLenses.(get tbl (key table_name |-- table |-- key key_name |-- string)) with
         Some v -> v | None -> "" in
       let strs_of table_name key_name = match TomlLenses.(get tbl (key table_name |-- table |-- key key_name |-- array |-- strings)) with
         Some v -> v | None -> [] in
       {
         url = str_of "general" "url";
         stylesheets = strs_of "general" "stylesheets";
         templates = {
             header = str_of "templates" "header";
             index = str_of "templates" "index";
             listing = str_of "templates" "listing";
             text = str_of "templates" "text";
           }
       }
end

let ymdpath title = return @@ "ymd/" ^ (Ymd.filename_of_title title)

let ymd_of_body_pairs pairs =
  let open Ymd in
  let open Lens.Infix in
  ListLabels.fold_left ~f:(fun a (k,vl) -> with_kv a (k, List.hd vl) ) ~init:blank_ymd pairs
  |> ((ymd_meta |-- meta_date |-- date_edited) ^= Some (Ptime_clock.now ()))

let ymd_of_req req =
  Lwt.map ymd_of_body_pairs (App.urlencoded_pairs_of_body req)

let string_response s = `String s |> respond'
let html_response h = `Html h |> respond'

let ymd_or_error y = match y with Some (path, meta) -> Logarion.of_file ("ymd/" ^ path) | None -> Ymd.blank_ymd

let webcfg = Configuration.of_filename "web.toml"
let lgrn = Logarion.Configuration.of_filename "logarion.toml"

let () =
  let (>>=) = Lwt.(>>=)
  and (>|=) = Lwt.(>|=) in
  let module L = Logarion in
  let ymd f = L.of_file f in
  let ret_param name req = return (param req name) in
  let header_tpl = Some (Logarion.load_file Configuration.(webcfg.templates.header)) in
  let listing_tpl = Some (Logarion.load_file Configuration.(webcfg.templates.listing)) in
  let text_tpl = Some (Logarion.load_file Configuration.(webcfg.templates.text)) in
  App.empty
  |> post "/post"     (fun req -> ymd_of_req req >>= fun ymd -> L.to_file ymd >>= fun () -> html_response (Html.of_ymd ~header_tpl ~text_tpl lgrn ymd))
  |> get "/edit/:ttl" (fun r   -> ret_param "ttl" r >>= ymdpath >|= ymd >|= Html.form ~header_tpl lgrn >>= html_response)
  |> get "/new"       (fun _   -> return Ymd.blank_ymd >|= Html.form ~header_tpl lgrn >>= html_response)
  |> get "/text/:ttl" (fun req -> ret_param "ttl" req >>= ymdpath >|= ymd >|= Html.of_ymd ~header_tpl ~text_tpl lgrn >>= html_response)
  |> get "/!/:ttl"    (fun req -> ret_param "ttl" req >|= L.latest_file_meta_pair >|= ymd_or_error >|= Html.of_ymd ~header_tpl ~text_tpl lgrn >>= html_response)
  |> get "/style.css" (fun _   -> return "ymd/style.css" >|= L.load_file >>= string_response)
  |> get "/"          (fun _   -> return (L.file_meta_pairs ()) >|= Html.of_file_meta_pairs ~header_tpl ~listing_tpl lgrn >>= html_response)
  |> App.run_command
