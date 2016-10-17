open Tyxml.Html

let to_string tyxml = Format.asprintf "%a" (Tyxml.Html.pp ()) tyxml

let head ?(style="/style.css") t =
  head (title (pcdata t)) [
         link ~rel:[`Stylesheet] ~href:"/style.css" ();
         meta ~a:[a_charset "utf-8"] ();
       ]

let logarion_header ?header_tpl:(tpl=None) title =
  match tpl with
  | Some s -> Unsafe.data Template.(of_string s |> fold_header title)
  | None   -> header [ h1 [ pcdata title] ]

let logarion_page head_title header_title main = 
  html (head head_title)
       (body [ logarion_header header_title; main ] )

let logarion_text ?text_tpl:(tpl=None) ymd =
  match tpl with
  | Some s -> Unsafe.data Template.(of_string s |> fold_text ymd)
  | None ->
     let ymd_body = Omd.to_html (Omd.of_string Ymd.(ymd.body)) in
     details
       (summary [Unsafe.data Ymd.(ymd.meta.abstract)])
       [time ~a:[a_datetime (Ymd.(rfc_string_of ymd.meta.date.published))] []];
     Unsafe.data ymd_body;
     footer [p []]

let of_ymd ?text_tpl:(tpl=None) lgrn ymd =
  let ymd_title = Ymd.(ymd.meta.title) in
  logarion_page
    (ymd_title ^ " by " ^ ymd.meta.author.name)
    ymd_title
    (logarion_text ~text_tpl:tpl ymd)
  |> to_string

let article_link (file, meta) =
  li [a ~a:[a_href (uri_of_string ("/text/" ^ Filename.chop_extension file))]
        [Unsafe.data Ymd.(meta.title)]
     ]

let of_file_meta_pairs ?listing_tpl:(tpl=None) lgrn file_meta_pairs =
  let t = Logarion.(lgrn.title) in
  logarion_page
    t t
    (div [ h2 [pcdata "Articles"];
           match tpl with
           | Some s -> Unsafe.data Template.(of_string s |> fold_index file_meta_pairs)
           | None -> ul (List.map article_link file_meta_pairs);
    ])
  |> to_string

let form lgrn ymd =
  let input_set title name value =
    p [ label [ span [pcdata title]; input ~a:[a_name name; a_value value] () ] ]
  in
  logarion_page
    "Compose" "Article composition"
    Ymd.(div [
         form
           ~a:[a_method `Post; a_action (uri_of_string "/post"); a_accept_charset ["utf-8"];]
           [
             fieldset
               ~legend:(legend [pcdata "Article"])
               [
                 input_set "Title" "title" ymd.meta.title;
                 input_set "Author name" "name" ymd.meta.author.name;
                 input_set "Author email" "email" ymd.meta.author.email;
                 input_set "Topics" "topics" (String.concat ", " ymd.meta.topics);
                 input_set "Categories" "categories" (String.concat ", " ymd.meta.categories);
                 input_set "Keywords" "keywords" (String.concat ", " ymd.meta.keywords);
                 input_set "Series" "series" (String.concat ", " ymd.meta.series);
                 input_set "Abstract" "abstract" ymd.meta.abstract;
                 p [
                     label [
                         span [pcdata"Text"];
                         textarea ~a:[a_name "body"] (pcdata ymd.body);
                       ];
                   ];
                 p [ button ~a:[a_button_type `Submit] [pcdata "Submit"] ];
               ]
           ]
    ])
  |> to_string
