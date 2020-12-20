(** 
   Representation of static adventure data.

   This module represents the data stored in the adventure file, including
   the rooms and exits.  It handles loading of that data from JSON as well
   as querying the data.
*)

(** The abstract type of values representing adventures. *)
type t

(** The type of room identifiers. *)
type room_id = int 

(** The type of exit names. *)
type exit_name = string

(** Raised when an unknown room is encountered. *)
exception UnknownRoom of room_id

(** Raised when an unknown exit is encountered. *)
exception UnknownExit of exit_name

(** [from_json j] is the adventure that [j] represents. *)
val from_json : Yojson.Basic.t -> t

(** [start_room a] is the identifier of the starting room in adventure 
    [a]. *)
val start_room : t -> room_id

(** [room_ids a] is a set-like list of all of the room identifiers in 
    adventure [a]. *)
val room_ids : t -> room_id list

(** [room_name a r] is the initial message of room [r] in adventure [a]. 
    Raises [Not_found] if [r] is not a room identifier in [a]. *)
val room_name : t -> room_id -> string

(** [exits a r] is a set-like list of all exit names from room [r] in 
    adventure [a].
    Raises [UnknownRoom r] if [r] is not a room identifier in [a]. *)
val exits : t -> room_id -> exit_name list

(** [next_room a r e] is the room to which [e] exits from room [r] in
    adventure [a].  
    Raises [UnknownRoom r] if [r] is not a room identifier in [a].
    Raises [UnknownExit e] if [e] is not an exit from room [r] in [a]. *)
val next_room : t -> room_id -> exit_name -> room_id

(** [next_rooms a r] is a set-like list of all rooms to which there is an exit 
    from [r] in adventure [a]. 
    Raises [UnknownRoom r] if [r] is not a room identifier in [a].*)
val next_rooms : t -> room_id -> room_id list

(** [enemies a r] is a list of all the enemy ids in room [r] in adventure [a].
    Raises [UnknownRoom r] if [r] is not a room identifier in [a]. *)
val enemies : t -> room_id -> int list 

(** [diffculty a r] is the dificulty level of the enemy in room [r] in 
    adventure [a]. Raises [UnknownRoom r] if [r] is not a room identifier 
    in [a]. *)
val difficulty: t -> room_id -> int 

(** The type representing item types. The string is the item name. *)
type item =
  | FlatHp of string * int (** Recovers flat hp *)
  | PercentHp of string * float (** Recovers % hp *)
  | RevivalItem of string

(** The type of values representing complete item data for the shop. *)
type item_wrapper =
  {
    item : item;
    price : int
  }

(** [shop a r] is a list of all the items in the shop in room [r] in adventure 
    [a]. 
    Raises: [UnknownRoom r] if [r] is not a room identifier in [a] *)
val shop: t -> room_id -> item_wrapper list 

(** [rewards a r] is a list of all the rewards in room [r] in adventure [a].
    Raises [UnknownRoom r] if [r] is not a room identifier in [a]*)
val rewards: t -> room_id -> item list 

(** [item_matcher o] is the [item]-typed value represented by the JSON object 
    [o]. *)
val item_matcher : Yojson.Basic.t -> item

(** [item_string i] is a string representation of [i], without price. *)
val item_string : item -> string

(** [item_wrapper_string i] is a string representation of [i], with price. *)
val item_wrapper_string : item_wrapper -> string