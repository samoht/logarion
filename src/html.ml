open Tyxml.Html

let logarion_head ?(style="/style.css") t =
  head (title (pcdata t)) [link ~rel:[`Stylesheet] ~href:"/style.css" ()]
       
let html_of ymd =
  let ymd_title = Logarion.(ymd.meta.title) in
  let ymd_date = match Logarion.(ymd.meta.date.published) with
    | Some t -> Some t
    | None -> Logarion.(ymd.meta.date.edited) in
  let ymd_body = Omd.to_html (Omd.of_string Logarion.(ymd.body)) in
  let tyhtml =
    html (logarion_head ymd_title)
      (body [
           header [
               h1 [Unsafe.data ymd_title];
               details
                 (summary [Unsafe.data Logarion.(ymd.meta.abstract)])
                 [time ~a:[a_datetime (Logarion.(to_rfc ymd_date))] []];
             ];
           Unsafe.data ymd_body;
           footer [p []];
      ])
  in
  Format.asprintf "%a" (Tyxml.Html.pp ()) tyhtml

let html_of_titles titles =
  let link_item x = li [a ~a:[a_href ("/" ^ x)] [Unsafe.data x]] in
  let tyhtml =
    html (logarion_head "Homepage")
      (body [
           header [
               h1 [pcdata "Homepage"];
             ];
           div [
               h2 [pcdata "Articles"];
               ul (List.map link_item titles);
             ];
      ])
  in
  Format.asprintf "%a" (Tyxml.Html.pp ()) tyhtml

let html_of_form ymd =
  let tyhtml =
    html (logarion_head "Compose")
      (body [
           header [
               h1 [pcdata "Create new article"];
             ];
           div [
               form
                 ~a:[a_method `Post; a_action (uri_of_string "/()/new")]
                 [
                   fieldset
                     ~legend:(legend [pcdata "Create new article"])
                     [
                       p [
                           label [
                               span [pcdata "Title"];
                               input ~a:[a_name "title"] ()
                             ];
                         ];
                       p [
                           label [
                               span [pcdata "Author name"];
                               input ~a:[a_name "author_name"] ();
                             ];
                         ];
                       p [
                           label [
                               span [pcdata "Author email"];
                               input ~a:[a_name "author_email"] ();
                             ];
                         ];
                       p [
                           label [
                               span [pcdata "Topics"];
                               input ~a:[a_name "topics"] ();
                             ];
                         ];
                       p [
                           label [
                               span [pcdata "Categories"];
                               input ~a:[a_name "categories"] ();
                             ];
                         ];
                       p [
                           label [
                               span [pcdata "Keywords"];
                               input ~a:[a_name "keywords"] ();
                             ];
                         ];
                       p [
                           label [
                               span [pcdata "Series"];
                               input ~a:[a_name "series"] ();
                             ];
                         ];
                       p [
                           label [
                               span [pcdata "Abstract"];
                               input ~a:[a_name "abstract"] ();
                             ];
                         ];
                       p [
                           label [
                               span [pcdata"Text"];
                               textarea ~a:[a_name "text"] (pcdata "");
                             ];
                         ];
                       p [
                           button ~a:[a_button_type `Submit] [pcdata "Submit"];
                         ];
                     ]
                 ]
             ];
      ])
  in
  Format.asprintf "%a" (Tyxml.Html.pp ()) tyhtml
