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

let () =
  let (>>=) = Lwt.(>>=)
  and (>|=) = Lwt.(>|=) in
  let module L = Logarion in
  let ymd f = L.of_file f in
  App.empty
  |> post "/()/new"   (fun req -> ymd_of_req req >>= fun ymd -> L.to_file ymd >>= fun () -> html_response (Html.of_ymd ymd))
  |> get "/:ttl"      (fun req -> return (param req "ttl") >>= ymdpath >|= ymd >|= Html.of_ymd >>= html_response)
  |> get "/:ttl/edit" (fun req -> return (param req "ttl") >>= ymdpath >|= ymd >|= Html.form   >>= html_response)
  |> get "/style.css" (fun _   -> return ("ymd/style.css") >|= L.load_file >>= string_response)
  |> get "/()/new"    (fun _   -> return Ymd.blank_ymd     >|= Html.form   >>= html_response)
  |> get "/"          (fun req -> return (L.titled_files ()) >|= Html.of_titled_files >>= html_response)
  |> App.run_command
