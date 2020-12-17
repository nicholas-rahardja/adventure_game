(**
   Representation of single player game state.

   This module represents the player's state in the single player mode of the 
   game. It contains information about the player's characters and their order, 
   level, location, and the list of visited locations. There are also functions 
   that query the aforementioned information, as well as change them.
*)

(** The abstract type of values representing the game state. *)
type t

(** The type of values representing a character's experience points. *)
type xp = int

(** The type of values representing a character's level. The relation of level 
    [l] to experience [x] is [l] must be the largest integer where [l * l + l] 
    is less than [x]. *)
type level = int

(** [init_state a clist] is the state at the start of the game in adventure [a] 
    with character list [clist]. The current room is the starting room of [a] 
    and the list of visited rooms is empty. Each character has experience and 
    level 0. *)
val init_state : Adventure.t -> Character.c list -> t

(** [get_chars t] is the ordered list of characters for the player with state 
    [t]. *)
val get_chars : t -> Character.c list

(** [get_char n t] is the character in index [n] of [t]'s character list. 
    Raises: [Failure] if the list is too short.
    [Invalid_argument] if n is negative. *)
val get_char : int -> t -> Character.c

(** [get_xp n t] is the experience points of the the character with index [n] in
    the character list for the player with state [t]. 
    Raises: [Failure] if the list is too short.
    [Invalid_argument] if n is negative. *)
val get_xp : int -> t -> xp

(** [get_level n t] is the level of the character with index [n] in the 
    character list for the player with state [t]. 
    Raises: [Failure] if the list is too short.
    [Invalid_argument] if n is negative. *)
val get_level : int -> t -> level

(** [get_room t] is the ID of the room the player with state [t] is currently 
    in. *)
val get_room : t -> Adventure.room_id

(** [get_visited t] is the set-like list of IDs of rooms that the player with
    state [t] has visited. *)
val get_visited : t -> Adventure.room_id list

(** [add_chars c ~xp:e n t] adds character [c] with experience points [e] in 
    index [n] of the character list of the player with state [t]. If [n] is -1, 
    the character is appended to the end of the list. [e] is optional and will 
    default to 0. 
    Raises: [Failure] if [n] is less than -1 or more than 
    [List.length n] *)
val add_char : Character.c -> ?xp:xp -> int -> t -> t

(** [remove_char n t] is state [t] with the character in index [n] of the 
    character list removed. 
    Raises: [Failure] if [n] is not a valid index of the character 
    list. *)
val remove_char : int -> t -> t

(** [swap_chars n1 n2] swaps the characters at indices [n1] and [n2] of [t]'s 
    character list. 
    Raises: [Failure] if [n1] and [n2] are not valid indices of the
    character list. *)
val swap_chars : int -> int -> t -> t

(** [add_xp n e t] is the state [t] with the experience points of the character 
    at index [n] in the character list incremented by [e]. It is complemented
    by a boolean that is [true] if the addition results in a level increase. 
    Raises: [Failure] if [n] is not a valid index of the character 
    list. *)
val add_xp : int -> xp -> t -> t * bool

(** The type representing the result of an attempted move. *)
type result =
  | Legal of t
  | Illegal

(** [move t r] returns [Legal t'] where [t'] is the state [t] but with the 
    player moved to room with ID [r] if the move is legal. If the move is not 
    legal, it returns [Illegal]. *)
val move : t -> Adventure.room_id -> result