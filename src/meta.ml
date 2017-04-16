open Lens
   
type name = string
type email = string

module Date = struct
  type t = {
      created: Ptime.t option;
      edited: Ptime.t option;
      published: Ptime.t option;
    } [@@deriving lens { submodule = true }]

  let rfc_string date = match date with Some t -> Ptime.to_rfc3339 t | None -> ""

  let of_string (rfc : string) = match Ptime.of_rfc3339 rfc with
      Ok (t,_,_) -> Some t | Error _ -> None

  let last (date : t) =
    let ds = [ date.created; date.edited; date.published ] in
    let open List in
    let ds' =
      fold_left (fun a d -> match d with Some d -> d :: a | None -> a) [] ds
      |> sort Ptime.compare
    in
    let len = List.length ds' in
    if len > 0 then Some (nth ds' (pred len))
    else None

  let compare a b = compare (last a) (last b)

  let pretty_date = function
    | Some t -> Ptime.to_date t |> fun (y, m, d) -> Printf.sprintf "%04d-%02d-%02d" y m d
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
    } [@@deriving lens { submodule = true } ]
  let of_string ~email name = { name; email }  
end

module Category = struct
  type t = Draft | Unlisted | Published | Custom of string

  let compare = Pervasives.compare

  let of_string = function
    | "draft" -> Draft
    | "unlisted" -> Unlisted
    | "published" -> Published
    | c -> Custom c

  let to_string = function
    | Draft -> "draft"
    | Unlisted -> "unlisted"
    | Published -> "published"
    | Custom c -> c
end

module CategorySet = struct
  include Set.Make(Category)
  let to_csv set =
    let f elt a =
      let s = Category.to_string elt in
      if a <> "" then a ^ ", " ^ s else s
    in
    fold f set ""
  let categorised categs cs = of_list categs |> (fun s -> subset s cs)
  let published = categorised [Category.Published]
  let listed cs = not @@ categorised [Category.Unlisted] cs
end

type t = {
    title: string;
    author: Author.t;
    date: Date.t;
    categories: CategorySet.t;
    topics: string list;
    keywords: string list;
    series: string list;
    abstract: string;
    uuid: Id.t
  } [@@deriving lens { submodule = true }]

let blank ?(uuid=(Id.generate ())) () = {
    title = "";
    author = Author.({ name = ""; email = "" });
    date = Date.({ created = None; edited = None; published = None });
    categories = CategorySet.empty;
    topics = [];
    keywords = [];
    series = [];
    abstract = "";
    uuid;
  }

let value_with_name (meta as m) = function
  | "title"    -> m.title
  | "abstract" -> m.abstract
  | "author_name"  -> m.author.Author.name
  | "author_email" -> m.author.Author.email
  | "date"          -> Date.(rfc_string @@ last m.date)
  | "date_created"  -> Date.(rfc_string m.date.created)
  | "date_edited"   -> Date.(rfc_string m.date.edited)
  | "date_published"-> Date.(rfc_string m.date.published)
  | "date_human"    -> Date.(pretty_date @@ last m.date)
  | "topics"     -> String.concat ", " m.topics;
  | "categories" -> CategorySet.to_csv m.categories;
  | "keywords" -> String.concat ", " m.keywords;
  | "series" -> String.concat ", " m.series;
  | "uuid" -> Id.to_string m.uuid
  | e -> invalid_arg e

let with_kv meta (k,v) =
  let list_of_csv = Re_str.(split (regexp " *, *")) in
  let open Infix in
  let of_str y k v = (k ^= String.trim v) y in
  let of_str_list y k v = (k ^= list_of_csv (String.trim v)) y in
  let open Lens in
  match k with
  | "title"     -> of_str meta title v
  | "author"    -> of_str meta (author |-- Author.Lens.name ) v
  | "name"      -> of_str meta (author |-- Author.Lens.name ) v
  | "email"     -> of_str meta (author |-- Author.Lens.email) v
  | "abstract"  -> of_str meta abstract v
  | "date"      -> ((date |-- Date.Lens.created)   ^= Date.of_string v) meta
  | "published" -> ((date |-- Date.Lens.published) ^= Date.of_string v) meta
  | "edited"    -> ((date |-- Date.Lens.edited   ) ^= Date.of_string v) meta
  | "topics"    -> of_str_list meta topics v
  | "keywords"  -> of_str_list meta keywords v
  | "categories"->
     let list = String.trim v |> list_of_csv in
     let list = List.map Category.of_string list in
     (categories ^= CategorySet.of_list list) meta
  | "series"    -> of_str_list meta series v
  | "uuid"      ->
     (match Id.of_string v with Some id -> (uuid ^= id) meta | None -> meta)
  | _ -> meta

let to_string (meta as m) =
  let has_len v = String.length v > 0 in
  let s field value = if has_len value then field ^ ": " ^ value ^ "\n" else "" in
  let a value =
    Author.(if has_len value.name || has_len value.email
            then "authors: " ^ value.name ^ " <" ^ value.email ^ ">\n" else "")
  in
  let d field value = match value with
    | Some d -> field ^ ": " ^ Date.rfc_string value ^ "\n" | None -> ""
  in
  let ss field values =
    if List.length values > 0
    then field ^ ": " ^ String.concat ", " values ^ "\n"
    else ""
  in
  let rows =
    [ s "title" m.title;
      a m.author;
      d "date" m.date.Date.created;
      d "edited" m.date.Date.edited;
      d "published" m.date.Date.published;
      ss "topics" m.topics;
      s "categories" (CategorySet.to_csv m.categories);
      ss "keywords" m.keywords;
      ss "series" m.series;
      s "abstract" m.abstract;
      s "uuid" (Uuidm.to_string m.uuid);
    ]
  in
  String.concat "" rows
