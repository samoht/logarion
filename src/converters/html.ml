open Tyxml.Html
open Logarion

let to_string tyxml = Format.asprintf "%a" (Tyxml.Html.pp ()) tyxml

let head ?(style="/static/main.css") t =
  head (title (pcdata t)) [
         link ~rel:[`Stylesheet] ~href:style ();
         link ~rel:[`Alternate]  ~href:"/feed.atom" ~a:[a_mime_type "application/atom+xml"] ();
         meta ~a:[a_charset "utf-8"] ();
       ]

let logarion_header ?(header_tpl=None) blog_url title =
  match header_tpl with
  | Some (Template.Header s) -> Unsafe.data Template.(fold_header blog_url title s)
  | None   -> header [ h1 [ pcdata title] ]

let logarion_page ?(header_tpl=None) blog_url head_title header_title main = 
  html (head head_title) (body [ logarion_header ~header_tpl blog_url header_title; main ])

let logarion_note ?(note_tpl=None) ymd =
  match note_tpl with
  | Some (Template.Note s) -> Unsafe.data Template.(fold_note ymd s)
  | None ->
     let open Note in
     let open Meta in
     let ymd_body = Omd.to_html (Omd.of_string ymd.body) in
     article [
         details
           (summary [Unsafe.data ymd.meta.abstract])
           [time ~a:[a_datetime Date.(pretty_date @@ last ymd.meta.date)] []];
         Unsafe.data ymd_body;
       ]

let of_note ?(header_tpl=None) ?(note_tpl=None) blog_url lgrn ymd =
  logarion_page
    ~header_tpl
    blog_url
    (Note.title ymd ^ " by " ^ ymd.Note.meta.Meta.author.Meta.Author.name)
    Logarion.(lgrn.Archive.Configuration.title)
    (logarion_note ~note_tpl ymd)
  |> to_string

let article_link meta =
  let open Logarion in
  let u = "/note/" ^ Meta.alias meta in
  let d =
    let open Meta in
    Unsafe.data Note.(meta.Meta.title ^ (Meta.Date.pretty_date (meta.date |> Meta.Date.last)))
  in
  li [ a ~a:[ a_href (uri_of_string u) ] [ d ] ]

let of_entries ?(header_tpl=None) ?(list_tpl=None) ?(item_tpl=None) ?(from=0) ?(n=0) blog_url lgrn notes =
  let title = Logarion.(lgrn.Archive.Configuration.title) in
  logarion_page
    ~header_tpl
    blog_url
    title
    title
    (match list_tpl with
     | Some (Template.List s) -> Unsafe.data Template.(fold_list ~item_tpl ~from ~n notes s)
     | None -> (div [ h2 [pcdata "Articles"]; ul (List.map article_link notes); ]))
  |> to_string

let form ?(header_tpl=None) blog_url lgrn ymd =
  let article_form =
    let input_set title input = p [ label [ pcdata title; input ] ] in
    let either a b = if a <> "" then a else b in
    let open Note in
    let open Meta in
    let open Author in
    let auth = ymd.meta.author in
    let auth_name = either auth.name Logarion.(lgrn.Archive.Configuration.owner) in
    let auth_addr = either (Email.to_string auth.email) Logarion.(lgrn.Archive.Configuration.email) in
    [
      input ~a:[a_name "uuid"; a_value (Id.to_string ymd.meta.uuid); a_input_type `Hidden] ();
      input_set
        "Title"
        (input ~a:[a_name "title"; a_value ymd.meta.title; a_required ()] ());
      input_set
        "Author name"
        (input ~a:[a_name "name"; a_value auth_name] ()); 
      input_set
        "Author email"
        (input ~a:[a_name "email"; a_value auth_addr; a_input_type `Email] ());
      input_set
        "Topics"
        (input ~a:[a_name "topics"; a_value (stringset_csv ymd.meta.topics)] ());
      input_set
        "Categories"
        (input ~a:[a_name "categories"; a_value (CategorySet.to_csv ymd.meta.categories)] ());
      input_set
        "Keywords"
        (input ~a:[a_name "keywords"; a_value (stringset_csv ymd.meta.keywords)] ());
      input_set
        "Series"
        (input ~a:[a_name "series"; a_value (stringset_csv ymd.meta.series)] ());
      input_set
        "Abstract"
        (input ~a:[a_name "abstract"; a_value ymd.meta.abstract] ());
      input_set
        "Text"
        (textarea ~a:[a_name "body"] (pcdata ymd.body));
      p [ button ~a:[a_button_type `Submit] [pcdata "Submit"] ];
    ] in
  logarion_page
    ~header_tpl
    blog_url
    "Compose" "Article composition"
    (div [ form
             ~a:[a_method `Post; a_action (uri_of_string "/post.note"); a_accept_charset ["utf-8"];]
             [ fieldset ~legend:(legend [pcdata "Article"]) article_form ]
    ])
  |> to_string

let of_message ?(header_tpl=None) blog_url lgrn title message =
  logarion_page
    ~header_tpl
    blog_url
    title
    Logarion.(lgrn.Archive.Configuration.title)
    (div [pcdata message])
  |> to_string
