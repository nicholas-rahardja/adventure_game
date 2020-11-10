(**
   Representation of single player game state.

   This module represents the player's state in the single player mode of the 
   game. It contains information about the player's characters, level, location, 
   and the list of visited locations. There are also functions that query the 
   aforementioned information, as well as change them.
*)

(** The abstract type of values representing the game state. *)
type t

(** The type of values representing the player's level. *)
type level = int

(** [init_state a clist] is the state at the start of the game in adventure [a] 
    with character list [clist]. The current room is the starting room of [a] 
    and the list of visited rooms is empty. *)
val init_state : Adventure.t -> Character.c list -> t

(** [get_chars t] is the set-like list of characters of the player with state 
    [t]. *)
val get_chars : t -> Character.c

(** [get_level t] is the level of the player with state [t]. *)
val get_level : t -> level

(** [get_room t] is the ID of the room the player with state [t] is currently 
    in. *)
val get_room : t -> Adventure.room_id

(** [get_visited t] is the set-like list of IDs of rooms that the player with
    state [t] has visited. *)
val get_visited : t -> Adventure.room_id list

(** [add_char t c] is the state [t] with character [c] added to the list of
    playable characters. *)
val add_char : t -> Character.c -> t

(** [remove_char t c] is the state [t] with character [c] removed from the list
    of playable characters *)
val remove_char : t -> Character.c -> t

(** [set_level t l] is the state [t] with the player's level changed to [l]. *)
val set_level : t -> level -> t

(** [incr_level t] increments [t]'s level by 1. *)
val incr_level : t -> t

(** The type representing the result of an attempted move. *)
type result =
  | Legal of t
  | Illegal

(** [move t r] returns [Legal t'] where [t'] is the state [t] but with the 
    player moved to room with ID [r] if the move is legal. If the move is not 
    legal, it returns [Illegal]. *)
val move : t -> Adventure.room_id -> result