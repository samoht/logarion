let esc = Xml_print.encode_unsafe_char

let header logarion url =
  let open Logarion.Configuration in
  "<title>" ^ logarion.title ^ "</title>"
  (* ^ "<subtitle>A subtitle.</subtitle>"*)
  ^ "<link rel=\"alternate\" type=\"text/html\" href=\"" ^ url ^ "\"/>"
  ^ "<link rel=\"self\" type=\"application/atom+xml\" href=\"" ^ url ^ "feed.atom\" />"
  ^ "<id>urn:uuid:" ^ Logarion.Id.to_string logarion.id ^ "</id>"
  ^ "<updated>" ^ Ptime.(to_rfc3339 (Ptime_clock.now ())) ^ "</updated>"

let opt_element tag_name content body =
  if content <> ""
  then body ^ "<" ^ tag_name ^ ">" ^ content ^ "</" ^ tag_name ^ ">"
  else body

let entry repo url logarion note =
  let open Logarion in
  let file = File.note note.Entry.path in
  let open Meta in
  let open Author in
  let u = "note/" ^ Entry.slug note in
  let meta = file.Note.meta in
  ("<entry>"
  ^ "<title>" ^ meta.title ^ "</title>"
  ^ "<id>urn:uuid:" ^ Meta.Id.to_string meta.uuid ^ "</id>"
  ^ "<link rel=\"alternate\" href=\"" ^ url ^ "/" ^ u ^ "\" />"
  ^ "<updated>" ^ Date.(meta.date |> last |> rfc_string) ^ "</updated>"
  ^ "<author>"
  |> opt_element "name"  @@ esc meta.author.name
  |> opt_element "email" @@ esc meta.author.email
   |> opt_element "summary" @@ esc meta.abstract)
  ^ "</author>"
  ^ "<content type=\"xhtml\"><div xmlns=\"http://www.w3.org/1999/xhtml\">"
  ^ (Omd.to_html @@ Omd.of_string @@ esc file.Note.body)
  ^ "</div></content>"
  ^ "</entry>"

let feed repo url logarion articles =
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<feed xmlns=\"http://www.w3.org/2005/Atom\">\n"
  ^ header logarion url
  ^ List.fold_left (fun feed note -> feed ^ "\n" ^ entry repo url logarion note) "" articles
  ^ "</feed>"
