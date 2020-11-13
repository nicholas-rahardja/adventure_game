(** A module representing combat*)
(**[type c] represents one character, and their current hp. Must include Character.c in its representation *)
type c =
  {
    char_c: Character.c;
    char_name: string;
    char_moves: Character.move list;
    mutable cur_hp: int;
    mutable buffs : unit list; 
    mutable active: bool;
  }

(** [team] represents a team of characters. If a character is dead, then it is None. If a character is alive,
    it is Some c *)
type team = c list

(** [type t] represents the current state of combat. Should include a list of team1 and and team2. Should include
    a field containing a variant of whose turn it is. Includes the winner of the game, '1' for team 1, 
    '2' for team 2, or 'in_progress' if no winner yet *)
type t ={
  team1: team;
  team2: team;
  mutable winner: int;
}

exception Winner of int
exception UnknownMove
exception UnknownTarget

(** [proc k] is a random number generator. Returns true with a probability
    of k/100.
    Requires: 0 <= k <= 100 *)

val proc: int -> bool

(** [do_dmg c dmg] inflicts dmg amount of damage on character c. *)
val do_dmg: c -> float -> unit

(** [is_active c] returns true if a character's hp is above 0. *)
val is_active: c -> bool

(** [get_active team] returns a list with only the active characters *)
val get_active: team -> team

(** [select_enemy team] ask the user to pick an enemy from [team]
    to target. They will have to enter an int. 1 for the 1st target, 2 for the 
    2nd target, 3 for the 3rd target. If the pick an int that is out of range,
    the function will ask again
*)
val select_enemy : team -> c

(** [is_team_dead act_team] checks if [act_team] is all dead.
    Requires: [act_team] must be a team type that passed through the get_active
    function *)
val is_team_dead: team -> bool

(** [check_winner act_team inte] raises [Winner inte] exception if act_team 
    is dead. 
    function 
    requires: [team] is an active team*)
val check_winner: team -> int -> unit


(** PH function. Will be used to allow usage of items *)
val use_item: team -> unit

(** [is_move move_name move] checks if [move_name] matches [move] *)
val is_move: string -> Character.move -> bool


(** [select_move move_list] prints out the possible moves to use,
    and asks for the user to input an int. the function evaluates to the 
    move they chose. Will ask and repeat if the user does not select
    a valid move *)
val select_move: Character.move list -> Character.move


(** [use_move opp_team c] asks the user to attack with character [c].
    The possible targets are [opp_team]*)
val use_move: int -> c list -> c -> unit


(** [get_team n t] will return tuple of [(current_team, opposing team)]. 
    [current_team] is denotes that it is this team's turn to attack. 
    [opposing team] is the team being attacked *)
val get_team: int -> t -> team * team


(** [team_n_turn n t] carries out the functions of 1 turn. It allows team [n]
    one item usage, and then each character on the team will choose a move, and
    then choose a target to use it on.*)
val team_n_turn: int -> t -> unit


(** [start_t t] starts combat from the representation type t. the while loop
    only ends when a team has won *)
val start_t: t -> unit


(** [print_char_select t k] prints a message, to signify that the id k will 
      choose a character with that id in t.
*)
val print_char_select: Character.t -> int -> unit


(** [player_pick k lst] allows the user to pick k characters from a [lst],
    which is the list of character ids. Returns the int list they chose *)
val player_pick : int -> int list -> int list



(** [random_pick_char t k] allows users to pick 
    [k] character from [choices] number of choices. 
    Requires : [max_id] must be less than the number of possible character *)
val random_pick_char : Character.t -> int -> int -> int -> int list




(** [random_clst n t k max_id choices] gives the user a random list of characters
    containing [choices] number of characters. They are allowed to choose [k]
    characters, and then returns a Character.c list containing their choices
    Requires : [max_id] must be less than the number of possible character *)
val random_clst : int -> Character.t -> int -> int -> int -> Character.c list



(** [init] clst1 clst2 initializes a game state t with the given character lists *)
val init: Character.c list -> Character.c list -> t

(** [start] clst1 clst2 initializes a game state t with the given character lists,
    evaluates to a int for who wins combat. 1 for team 1 win, 2 for team 2 win.
    Once combat ends, it must allow the function [winner] to return the
    correct winner*)
val start: Character.c list -> Character.c list -> unit

(** [winner] returns the winner of combat. Is 0 if game is still going on *)
val winner: t -> int

(** [mult_start] initiates combat, with character/moves contained in [t]*)
val mult_start: Character.t -> unit


(* Sp combat functions *)

val start_mult: Character.c list -> Character.c list -> unit



