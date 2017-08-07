type name_t = string
type email_t = string

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
  let compare = Uuidm.compare
  let to_string = Uuidm.to_string
  let of_string = Uuidm.of_string
  let generate = Uuidm.v4_gen (Random.get_state ())
end

module Author = struct
  type t = {
      name: name_t;
      email: email_t;
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

module StringSet = Set.Make(String)

let stringset_csv set =
  let f elt a = if a <> "" then a ^ ", " ^ elt else elt in
  StringSet.fold f set ""

let string_alias t =
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

type t = {
    title: string;
    author: Author.t;
    date: Date.t;
    categories: CategorySet.t;
    topics: StringSet.t;
    keywords: StringSet.t;
    series: StringSet.t;
    abstract: string;
    uuid: Id.t;
    alias: string;
  } [@@deriving lens { submodule = true }]

let blank ?(uuid=(Id.generate ())) () = {
    title = "";
    author = Author.({ name = ""; email = "" });
    date = Date.({ created = None; edited = None; published = None });
    categories = CategorySet.empty;
    topics   = StringSet.empty;
    keywords = StringSet.empty;
    series   = StringSet.empty;
    abstract = "";
    uuid;
    alias = "";
  }

let listed    e = CategorySet.listed e.categories
let published e = CategorySet.published e.categories
let unique_topics ts x = StringSet.union ts x.topics

module AliasMap = Map.Make(String)
module IdMap = Map.Make(Id)

let alias meta = if meta.alias = "" then string_alias meta.title else meta.alias

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
  | "topics"     -> stringset_csv m.topics;
  | "categories" -> CategorySet.to_csv m.categories;
  | "keywords"   -> stringset_csv m.keywords;
  | "series"     -> stringset_csv m.series;
  | "uuid" -> Id.to_string m.uuid
  | "alias" -> alias m
  | e -> invalid_arg e

let with_kv meta (k,v) =
  let list_of_csv = Re_str.(split (regexp " *, *")) in
  let trim = String.trim in
  match k with
  | "title"     -> { meta with title = trim v }
  | "author"
  | "name"      -> { meta with author = Author.{ meta.author with name  = trim v }}
  | "email"     -> { meta with author = Author.{ meta.author with email = trim v }}
  | "abstract"  -> { meta with abstract = trim v }
  | "date"      -> { meta with date = Date.{ meta.date with created   = Date.of_string v }}
  | "published" -> { meta with date = Date.{ meta.date with published = Date.of_string v }}
  | "edited"    -> { meta with date = Date.{ meta.date with edited    = Date.of_string v }}
  | "topics"    -> { meta with topics   = trim v |> list_of_csv |> StringSet.of_list }
  | "keywords"  -> { meta with keywords = trim v |> list_of_csv |> StringSet.of_list }
  | "categories"->
     let categories = trim v |> list_of_csv |> List.map Category.of_string |> CategorySet.of_list in
     { meta with categories }
  | "series"    -> { meta with series = trim v |> list_of_csv |> StringSet.of_list }
  | "uuid"      -> (match Id.of_string v with Some id -> { meta with uuid = id } | None -> meta)
  | "alias"     -> { meta with alias = v }
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
      s "topics" (stringset_csv m.topics);
      s "categories" (CategorySet.to_csv m.categories);
      s "keywords" (stringset_csv m.keywords);
      s "series"   (stringset_csv m.series);
      s "abstract" m.abstract;
      s "uuid" (Uuidm.to_string m.uuid);
      s "alias" m.alias
    ]
  in
  String.concat "" rows
