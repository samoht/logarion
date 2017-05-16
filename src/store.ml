module type T = sig
  type t
  val to_list: ?order:('a -> 'a -> int) -> (Note.t -> 'a) -> t -> 'a list
  val note_with_id: t -> Meta.Id.t -> Note.t option
  val note_with_alias: t -> string -> Note.t option
  val with_note: t -> Note.t -> Note.t Lwt.t
end
