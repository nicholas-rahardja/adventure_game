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

(** The type of values representing gold, the in-game currency. The player must 
    never have negative gold. *)
type gold = int

(** [init_state a clist] is the state at the start of the game in adventure [a] 
    with character list [clist]. The current room is the starting room of [a] 
    and the list of visited rooms is empty. Each character has experience and 
    gold equal to 0. The inventory is empty. *)
val init_state : Adventure.t -> Character.c list -> t

(** [get_chars t] is the ordered list of characters for the player with state 
    [t]. *)
val get_chars : t -> Character.c list

(** [get_char n t] is the character in index [n] of [t]'s character list. 
    Raises: [Failure] if the list is too short.
    [Invalid_argument] if n is negative. *)
val get_char : int -> t -> Character.c

(** [get_xp n t] is the experience points of the character with index [n] in
    the character list for the player with state [t]. 
    Raises: [Failure] if the list is too short.
    [Invalid_argument] if n is negative. *)
val get_xp : int -> t -> xp

(** [get_char_with_xp n t] is a pair containing the character and its experience
    points at index [n] in
    the character list for the player with state [t]. 
    Raises: [Failure] if the list is too short.
    [Invalid_argument] if n is negative. *)
val get_char_with_xp : int -> t -> Character.c * xp

(** [get_char_with_xp_lst t] is same as [get_char_with_xp], but returns
    a list of pairs containing the player's current party of characters and 
    xp *)
val get_char_with_xp_lst : t -> (Character.c * xp) list


(** [get_level n t] is the level of the character with index [n] in the 
    character list for the player with state [t]. 
    Raises: [Failure] if the list is too short.
    [Invalid_argument] if n is negative. *)
val get_level : int -> t -> level

(** [xp_of_lvl lvl] is the total amount of xp to level from 0 to [lvl] *)
val xp_of_lvl: level -> xp

(** [get_room t] is the ID of the room the player with state [t] is currently 
    in. *)
val get_room : t -> Adventure.room_id

(** [get_visited t] is the set-like list of IDs of rooms that the player with
    state [t] has visited. *)
val get_visited : t -> Adventure.room_id list

(** [get_gold t] is the amount of gold the player with state [t] has. *)
val get_gold : t -> gold

(** [get_inventory t] is the (ordered) list of all the items that the player 
    with state [t] has. The order is used in [remove_inventory]. *)
val get_inventory : t -> Adventure.item list

(** [load_inventory items t] is the state of the game with the items replaced
    with [items] *)
val load_inventory: Adventure.item list -> t -> t

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

(** [add_xp n e t] is the state [t] with the experience points of the character 
    at index [n] in the character list incremented by [e]. It is complemented
    by a boolean that is [true] if the addition results in a level increase. 
    Raises: [Failure] if [n] is not a valid index of the character 
    list. *)
val add_xp : int -> xp -> t -> t * bool

(** [add_gold amt t] is state [t] with [amt] gold added. *)
val add_gold : gold -> t -> t

(** [sub_gold amt t] is state [t] with [amt] gold subtracted if [amt] is not 
    more than the amount in the player's account.
    Raises: [Failure] if the player has insufficient gold. *)
val sub_gold : gold -> t -> t

(** [add_inventory item t] is [t] with [item] prepended to the player's 
    inventory. *)
val add_inventory : Adventure.item -> t -> t

(** [remove_inventory i t] is [t] with the item at index [i] according to 
    [get_inventory] removed from the player's inventory.
    Raises: [Failure] if [i] is not a valid index of the player's inventory. *)
val remove_inventory : int -> t -> t

(** The type representing the result of an attempted move. *)
type result =
  | Legal of t
  | Illegal

(** [move t r] returns [Legal t'] where [t'] is the state [t] but with the 
    player moved to room with ID [r] if the move is legal. If the move is not 
    legal, it returns [Illegal]. *)
val move : t -> Adventure.room_id -> result

(** [save t path] saves state [t] as a JSON file to [path]. 
    Requires: [path] must be a valid, writable path. *)
val save : t -> string -> unit

(** [load adv c path] loads the JSON save file from [path] using adventure 
    information from [adv] and character information from [c]. 
    Requires: [path] must be a valid, readable path. *)
val load : Adventure.t -> Character.t -> string -> t