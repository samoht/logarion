let esc = Xml_print.encode_unsafe_char

let header config url =
  let open Logarion.Meta in
  let open Logarion.Archive.Configuration in
  "<title>" ^ config.title ^ "</title>"
  (* ^ "<subtitle>A subtitle.</subtitle>"*)
  ^ "<link rel=\"alternate\" type=\"text/html\" href=\"" ^ url ^ "\"/>"
  ^ "<link rel=\"self\" type=\"application/atom+xml\" href=\"" ^ url ^ "feed.atom\" />"
  ^ "<id>urn:uuid:" ^ Id.to_string config.id ^ "</id>"
  ^ "<updated>" ^ Ptime.to_rfc3339 (Ptime_clock.now ()) ^ "</updated>"

let opt_element tag_name content body =
  if content <> ""
  then body ^ "<" ^ tag_name ^ ">" ^ content ^ "</" ^ tag_name ^ ">"
  else body

let entry config url node_fn note =
  let open Logarion in
  let meta = note.Note.meta in
  let u = "note/" ^ Meta.alias meta in
  let open Meta in
  let open Author in
  ("<entry>"
   ^ "<title>" ^ meta.title ^ "</title>"
   ^ "<id>urn:uuid:" ^ Meta.Id.to_string meta.uuid ^ "</id>"
   ^ "<link rel=\"alternate\" href=\"" ^ url ^ "/" ^ u ^ "\" />"
   ^ "<updated>" ^ Date.(meta.date |> last |> rfc_string) ^ "</updated>"
   ^ "<author>"
   |> opt_element "name"  @@ esc meta.author.name
   |> opt_element "email" @@ esc (Email.to_string meta.author.email)
   |> opt_element "summary" @@ esc meta.abstract)
  ^ "</author>"
  ^ Meta.StringSet.fold (fun elt a -> a ^ "<category term=\"" ^ elt ^ "\"/>") meta.topics ""
  ^ "<content type=\"xhtml\"><div xmlns=\"http://www.w3.org/1999/xhtml\">"
  ^ (Omd.to_html @@ Omd.of_string @@ esc note.Note.body)
  ^ "</div></content>"
  ^ "</entry>"

let feed config url note_fn articles =
  let fold_valid feed m = match note_fn m.Logarion.Meta.uuid with
    | Some note -> feed ^ "\n" ^ entry config url note_fn note
    | None -> feed
  in
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<feed xmlns=\"http://www.w3.org/2005/Atom\">\n"
  ^ header config url
  ^ List.fold_left fold_valid "" articles
  ^ "</feed>"
