open Lens
   
type name = string
type email = string

module Date = struct
  type t = {
      edited: Ptime.t option;
      published: Ptime.t option;
    } [@@deriving lens]

  let rfc_string date = match date with
      Some t -> Ptime.to_rfc3339 t | None -> "";;
  let of_string (rfc : string) = match Ptime.of_rfc3339 rfc with
      Ok (t,_,_) -> Some t | Error _ -> None;;

  let last date = match date.published with Some t -> Some t | None -> date.edited

  let pretty_date = function
    | Some t ->
       Ptime.to_date t |> fun (y, m, d) -> Printf.sprintf "%04d-%02d-%02d" y m d
    | None -> ""
end

module Id = struct
  type t = Uuidm.t
  let to_string = Uuidm.to_string
  let of_string = Uuidm.of_string
  let generate = Uuidm.v4_gen (Random.get_state ())
end

module Author = struct
  type t = {
      name: name;
      email: email;
    } [@@deriving lens]
  let of_string ~email name = { name; email }  
end

type meta = {
    title: string;
    author: Author.t;
    date: Date.t;
    categories: string list;
    topics: string list;
    keywords: string list;
    series: string list;
    abstract: string;
    uuid: Id.t
  } [@@deriving lens]

type ymd = {
    meta: meta;
    body: string;
  } [@@deriving lens]

let blank_meta ?(uuid=(Id.generate ())) () = {
    title = "";
    author = Author.({ name = ""; email = "" });
    date = Date.({ edited = None; published = None });
    categories = []; topics = []; keywords = []; series = [];
    abstract = "";
    uuid;
  }

let blank_ymd ?(uuid=(Id.generate ())) () = { meta = blank_meta ~uuid (); body = "" }

let filename_of_title t =
  let is_reserved = function
    | '!' | '*' | '\'' | '(' | ')' | ';' | ':' | '@' | '&' | '=' | '+' | '$'
      | ',' | '/' | '?' | '#' | '[' | ']' | ' ' | '\t' | '\x00' -> true
    | _ -> false in
  let drop h t = t in
  let dash h t = '-' :: t in
  let rec filter fn = function
    | [] -> []
    | head :: tail ->
       if is_reserved head
       then fn head (filter drop tail)
       else Char.lowercase_ascii head :: (filter dash tail) in
  Batteries.String.of_list @@ filter drop (Batteries.String.to_list t)

let filename ymd = filename_of_title ymd.meta.title
let trim_str v = v |> String.trim
let of_str y k v = Lens.Infix.(k ^= trim_str v) y

let with_meta_kv meta (k,v) =
  let list_of_csv = Re_str.(split (regexp " *, *")) in
  let of_str_list y k v = Lens.Infix.(k ^= list_of_csv (trim_str v)) y  in
  let open Lens.Infix in
  match k with
  | "title"     -> of_str meta (meta_title) v
  | "name"      -> of_str meta (meta_author |-- Author.name ) v
  | "email"     -> of_str meta (meta_author |-- Author.email) v
  | "abstract"  -> of_str meta meta_abstract v
  | "published" -> ((meta_date |-- Date.published) ^= Date.of_string v) meta
  | "edited"    -> ((meta_date |-- Date.edited   ) ^= Date.of_string v) meta
  | "topics"    -> of_str_list meta meta_topics v
  | "keywords"  -> of_str_list meta meta_keywords v
  | "categories"-> of_str_list meta meta_categories v
  | "series"    -> of_str_list meta meta_series v
  | "uuid"      ->
     (match Id.of_string v with Some id -> (meta_uuid ^= id) meta | None -> meta)
  | _ -> meta

let with_kv ymd (k,v) =
  let open Lens.Infix in
  match k with
  | "body" -> of_str ymd (ymd_body) v
  | _      -> { ymd with meta = with_meta_kv ymd.meta (k,v) }

let meta_pair_of_string line =
  let e = Re_str.(bounded_split (regexp ": *")) line 2 in
  if List.length e = 2
  then (Re_str.(replace_first (regexp "^[ -] ") "" (List.nth e 0)), List.nth e 1)
  else (Re_str.(replace_first (regexp "^[ -] ") "" line), "")

let meta_of_yaml yaml =
  let fields = List.map meta_pair_of_string (BatString.nsplit yaml "\n") in
  let open Lens.Infix in
  List.fold_left with_meta_kv (blank_meta ()) fields

let of_string s =
  let segments = Re_str.(split (regexp "^---$")) s in
  if List.length segments = 2 then
    let yaml_str = List.nth segments 0 in
    let md_str = List.nth segments 1 in
    let m = meta_of_yaml yaml_str in
    { meta = m; body = md_str }
  else
    { (blank_ymd ()) with body = "Error parsing file" }

let make ?(author_name="") ?(author_email="") ?(date_published=None) ?(date_edited=None)
         ?(abstract="") ?(topics=[]) ?(keywords=[]) ?(categories=[]) ?(series=[])
         title body =
  {
    meta = {
      title;
      topics;
      keywords;
      categories;
      series;
      abstract;
      uuid = Uuidm.v4_gen (Random.get_state ()) ();
      author = {
          Author.name = author_name;
          Author.email = author_email;
        };
      date = {
          Date.edited = date_edited;
          Date.published = date_published;
        }
    };
    body
  }

let to_string ymd =
  let buf = Buffer.create (String.length ymd.body + 256) in
  let buf_acc = Buffer.add_string buf in
  List.iter buf_acc [
              "---\n";
              "title: ";   ymd.meta.title;
              "\nauthors:";
              "\n- name: ";  ymd.meta.author.Author.name;
              "\n  email: "; ymd.meta.author.Author.email;
              "\ndate:";
              "\n  edited: ";    Date.(rfc_string ymd.meta.date.edited);
              "\n  published: "; Date.(rfc_string ymd.meta.date.published);
              "\ntopics: ";     String.concat ", " ymd.meta.topics;
              "\ncategories: "; String.concat ", " ymd.meta.categories;
              "\nkeywords: ";   String.concat ", " ymd.meta.keywords;
              "\nseries: ";     String.concat ", " ymd.meta.series;
              "\nabstract: ";   ymd.meta.abstract;
              "\nuuid: "; Uuidm.to_string ymd.meta.uuid;
              "\n---\n"; ymd.body;
            ];
  Buffer.contents buf
