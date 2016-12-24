let header logarion =
  let open Logarion.Configuration in
  "<title>" ^ logarion.title ^ "</title>"
  (* ^ "<subtitle>A subtitle.</subtitle>"
  ^ "<link href=\"http://example.org/feed/\" rel=\"self\" />"
  ^ "<link href=\"http://example.org/\" />" *)
  ^ "<id>urn:uuid:" ^ Logarion.Id.to_string logarion.id ^ "</id>"
  ^ "<updated>" ^ Ptime.(to_rfc3339 (Ptime_clock.now ())) ^ "</updated>"

let entry url logarion ymd =
  let open Ymd in
  let open Ymd.Author in
  "<entry>"
  ^ "<title>" ^ ymd.meta.title ^ "</title>"
  ^ "<link rel=\"alternate\" type=\"text/html\" href=\"" ^ url ^ "\"/>"
  ^ "<id>urn:uuid:" ^ Ymd.Id.to_string  ymd.meta.uuid ^ "</id>"
  ^ "<updated>" ^ Ymd.Date.(ymd.meta.date |> last |> rfc_string) ^ "</updated>"
  ^ "<author><name>" ^ ymd.meta.author.name ^ "</name><email>" ^ ymd.meta.author.email ^"</email></author>"
  ^ "<summary>" ^ ymd.meta.abstract ^ "</summary>"
  ^ "<content type=\"xhtml\"><div xmlns=\"http://www.w3.org/1999/xhtml\">"
  ^ Omd.to_html (Omd.of_string ymd.body)
  ^ "</div></content>"
  ^ "</entry>"

let feed url logarion articles =
  "<?xml version=\"1.0\" encoding=\"utf-8\"?><feed xmlns=\"http://www.w3.org/2005/Atom\">"
  ^ header logarion
  ^ List.fold_left (fun body ymd -> body ^ entry url logarion ymd) "" articles
  ^ "</feed>"
