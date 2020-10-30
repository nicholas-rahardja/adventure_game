(**
   Representation of static characters and moves data. 

   This module represents the data stored in a characters file. It handles 
   loading of that data from JSON as well as querying the data. 
*)

(** Representation type for a character. *)
type c 

(** Representation type for a character's moves. *)
type move

(** Representation type for a character's or a move's element. *)
type element = 
  | Water 
  | Fire 
  | Grass
  | Normal 

(** Representation type for a characters and moves JSON file. The lists for 
    characters and moves are association lists with IDs as keys and 
    characters/moves as values. *)
type t = {
  all_chars : (int * c) list;
  all_moves : (int * move) list
}

(** [from_json j] is the list of characters and moves that [j] represents.
    Requires: [j] is a valid characters JSON file representation. *)
val from_json : Yojson.Basic.t -> t

(* Character-related functions *)

(** [get_char t id] returns the character with ID [id] in [t]. *)
val get_char : t -> int -> c option

(** [get_char_name c] returns the name of [c]. *)
val get_char_name : c -> string

(** [get_char_desc c] returns the description of [c]. *)
val get_char_desc : c -> string

(** [get_moves c] is the list of moves that [c] has. *)
val get_moves : c -> move list

(** [get_char_atk c] is [c]'s base attack value. *)
val get_char_atk : c -> int 

(** [get_hp c] is [c]'s initial HP. *)
val get_hp : c -> int 

(** [get_char_element c] is the element of [c]. *)
val get_char_element : c -> element

(* Move-related functions *)

(** [get_move t id] is the move with ID [id] in [t]. *)
val get_move : t -> int -> move option

(** [get_move t id] is the [move]'s name. *)
val get_move_name : move -> string

(** [get_desc t move] is [move]'s description. *)
val get_move_desc : move -> string

(** [get_atk t move] is [move]'s base attack value. *)
val get_move_atk : move -> int

(** [get_scale t move] is [move]'s attack value multiplier. *)
val get_scale : move -> float

(** [get_char_element t move] is [move]'s element. *)
val get_move_element : move -> element

(* Attack-related functions *)

(** [get_effectiveness move e] is the attack multiplier, based on [move] and 
    [e]'s element *)
val get_effectiveness : move -> c -> float

(** [get_damage c e move] is the damage [c] inflicts on [e] with [move]. *)
val get_damage : c -> c -> move -> float 