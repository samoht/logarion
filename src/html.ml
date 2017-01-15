open Tyxml.Html

let to_string tyxml = Format.asprintf "%a" (Tyxml.Html.pp ()) tyxml

let head ?(style="/static/main.css") t =
  head (title (pcdata t)) [
         link ~rel:[`Stylesheet] ~href:style ();
         meta ~a:[a_charset "utf-8"] ();
       ]

let logarion_header ?(header_tpl=None) blog_url title =
  match header_tpl with
  | Some (Template.Header s) -> Unsafe.data Template.(fold_header blog_url title s)
  | None   -> header [ h1 [ pcdata title] ]

let logarion_page ?(header_tpl=None) blog_url head_title header_title main = 
  html (head head_title)
       (body [ logarion_header ~header_tpl blog_url header_title; main ] )

let logarion_text ?(text_tpl=None) ymd =
  match text_tpl with
  | Some (Template.Text s) -> Unsafe.data Template.(fold_text ymd s)
  | None ->
     let ymd_body = Omd.to_html (Omd.of_string Ymd.(ymd.body)) in
     article [
         details
           (summary [Unsafe.data Ymd.(ymd.meta.Meta.abstract)])
           [time ~a:[a_datetime Ymd.(Date.(pretty_date @@ last ymd.meta.Meta.date))] []];
         Unsafe.data ymd_body;
       ]

let of_ymd ?(header_tpl=None) ?(text_tpl=None) blog_url lgrn ymd =
  logarion_page
    ~header_tpl
    blog_url
    Ymd.(Meta.(ymd.meta.title ^ " by " ^ ymd.meta.Meta.author.Author.name))
    Logarion.Configuration.(lgrn.title)
    (logarion_text ~text_tpl ymd)
  |> to_string

let article_link entry =
  let open Logarion.Entry in
  let u = "/text/" ^ slug entry in
  li [a ~a:[a_href (uri_of_string u)]
        [Unsafe.data (title entry ^ (Ymd.Date.pretty_date (entry |> date |> Ymd.Date.last))) ]
     ]

let of_entries ?(header_tpl=None) ?(listing_tpl=None) ?(entry_tpl=None) blog_url lgrn =
  let t = Logarion.Configuration.(lgrn.title) in
  logarion_page
    ~header_tpl
    blog_url
    t t
    (match listing_tpl with
     | Some (Template.Listing s) -> Unsafe.data Template.(fold_index ~entry_tpl lgrn s)
     | None ->
        let entries = Logarion.Archive.(of_repo lgrn.Logarion.Configuration.repository |> latest_listed) in
        (div [ h2 [pcdata "Articles"]; ul (List.map article_link entries); ]))
  |> to_string

let form ?(header_tpl=None) blog_url lgrn ymd =
  let article_form =
    let input_set title input = p [ label [ pcdata title; input ] ] in
    let either a b = if a <> "" then a else b in
    let open Ymd in
    let open Meta in
    let open Author in
    let auth = ymd.meta.author in
    let auth_name = either auth.name Logarion.Configuration.(lgrn.owner) in
    let auth_addr = either auth.email Logarion.Configuration.(lgrn.email) in
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
        (input ~a:[a_name "topics"; a_value (String.concat ", " ymd.meta.topics)] ());
      input_set
        "Categories"
        (input ~a:[a_name "categories"; a_value (CategorySet.to_csv ymd.meta.categories)] ());
      input_set
        "Keywords"
        (input ~a:[a_name "keywords"; a_value (String.concat ", " ymd.meta.keywords)] ());
      input_set
        "Series"
        (input ~a:[a_name "series"; a_value (String.concat ", " ymd.meta.series)] ());
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
             ~a:[a_method `Post; a_action (uri_of_string "/post"); a_accept_charset ["utf-8"];]
             [ fieldset ~legend:(legend [pcdata "Article"]) article_form ]
    ])
  |> to_string
