open Opium.Std

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

let () =
  let (>>=) = Lwt.(>>=)
  and (>|=) = Lwt.(>|=) in
  let module L = Logarion in
  let ymd f = L.of_file f in
  let ret_param name req = return (param req name) in
  App.empty
  |> post "/post"     (fun req -> ymd_of_req req >>= fun ymd -> L.to_file ymd >>= fun () -> html_response (Html.of_ymd ymd))
  |> get "/edit/:ttl" (fun r -> ret_param "ttl" r >>= ymdpath >|= ymd >|= Html.form   >>= html_response)
  |> get "/new"       (fun _   -> return Ymd.blank_ymd >|= Html.form   >>= html_response)
  |> get "/text/:ttl" (fun req -> ret_param "ttl" req >>= ymdpath >|= ymd >|= Html.of_ymd >>= html_response)
  |> get "/!/:ttl"    (fun req -> ret_param "ttl" req >|= L.latest_file_meta_pair >|= ymd_or_error >|= Html.of_ymd >>= html_response)
  |> get "/style.css" (fun _   -> return "ymd/style.css" >|= L.load_file >>= string_response)
  |> get "/"          (fun _   -> return (L.file_meta_pairs ()) >|= Html.of_file_meta_pairs >>= html_response)
  |> App.run_command
