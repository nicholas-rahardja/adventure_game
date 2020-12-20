(** A module that initiates and handles combat, given a list of characters.*)

(** [move_cd] is an entry in the cooldown list. It has the [move] on cooldown,
    and the number of turns left to get off cooldown. *)
type move_cd = 
  {move: Character.move;
   turns_left: int;}

(** [type cd_lst] contains a list of moves of characters that are on cooldown.
    It includes the number of turns left until it is off cooldown. If 
    a move goes from on cooldown to off cooldown, then it should be removed
    from the cd list *)
type cd_lst = move_cd list

(** [type item] represents an item from the [Adventure] module *)
type item = Adventure.item

(**[type c] represents one character on a team.
   Must include Character.c in its representation.*)
type c =
  {
    char_c: Character.c;
    char_name: string;
    char_moves: Character.move list;
    atk: int;
    level: int;
    mutable cur_hp: int;
    mutable buffs : unit list; 
    mutable active: bool;
    mutable cooldown: cd_lst;
  }

(** [team] represents a team of characters.*)
type team = c list

(** [type t] represents the current state of combat. Should include a list of 
    team1 and and team2. Should include a field containing a variant of
     whose turn it is. Includes the winner of the game, '1' for team 1, 
    '2' for team 2, or 'in_progress' if no winner yet *)
type t ={
  team1: team;
  team2: team;
  mutable winner: int;
  mutable items: item list
}

exception Winner of int

(** [move_select] distinguishes between a valid move that the user can enter,
    , or if an input that does not lead to a valid move*)
type move_select =
  | Valid_m of Character.move
  | Invalid_m

(** [target_select] distinguishes between a valid target that the user can 
    enter, and an input that does not lead to a valid move*)
type target_select = 
  | Valid_tar of c
  | Invalid_tar

(* TODO *)
(** [item_select] distinguishes between a valid item that the user can use,
    and an input that does not lead to a valid item*)
type item_select = 
  | ValidItem of item
  | InvalidItem

(** [dmg_variation] is the variation % that all damage/healing is subjected to.
    Example: if dmg_variation = 5, then a damage of 100 can be randomly 
    increased to 105 or decreased to 95, maximum *)
val dmg_variation : int

(** [vary k percent] returns a value that deviates from [k] 
    by at most +- [percent]%.
    Requires: 0 <= percent <= 100*)
val vary : float -> int -> float

(** [proc k] is a random number generator. Returns true with a probability
    of k/100.
    Requires: 0 <= k <= 100 *)
val proc: int -> bool

(** [do_dmg c dmg] subtracts exactly [dmg] to character [c] health.
    Does not take into account  variation / buffs, etc. *)
val do_dmg: c -> int -> unit

(** [do_heal c heal] adds exactly [heal] to character [c] health. The health 
    after cannot exceed [c]'s maximum health.
    Does not take into account  variation / buffs, etc. *)
val do_heal: c -> int -> unit

(** [is_active c] returns true if a character's hp is above 0. *)
val is_active: c -> bool

(** [get_active team] returns a list with only the active characters *)
val get_active: team -> team

(** [select_enemy team] ask the user to pick an enemy from [team]
    to target. They will have to enter an int. 1 for the 1st target, 2 for the 
    2nd target, 3 for the 3rd target. If the pick an int that is out of range,
    the function will ask again.
*)
val select_enemy : team -> c

(** [target_input team input] checks if [input] is a valid target in [team]
    If it is, return [Valid_tar target], where target is the target chosen.
    Else, return [Invalid_tar]
    Requires: team must be an active_team, meaning no one is dead 
*)
val target_input : team -> string -> target_select

(** [is_team_dead act_team] checks if [act_team] is all dead.
    Requires: [act_team] must be a team type that passed through the get_active
    function *)
val is_team_dead: team -> bool

(** [check_winner act_team inte] raises [Winner inte] exception if act_team 
    is dead.  
    requires: [team] is an active team*)
val check_winner: team -> int -> unit

(** PH function. Will be used to allow usage of items *)
val use_item: team -> item list -> t ->  unit

(*print [is_move input move] checks if [input] matches the name of [move], 
    it disregards capitalization  *)
val is_move: string -> Character.move -> bool

(** [select_move move_list] prints out the possible moves to use,
    and asks for the user to input an int. the function evaluates to the 
    move they chose. Will ask and repeat if the user does not select
    a valid move *)
val select_move: Character.move list -> Character.move

(** [move_input move_lst input] checks if [input] is a valid move of [char_c].
    Returns: [Valid_m move] for a valid [move]. Else, it returns [Invalid_m].*)
val move_input: Character.move list -> string -> move_select 

(** [use_move opp_team c] asks the user to attack with character [c].
    The possible targets are [opp_team]*)
val use_move: int -> c list -> c -> unit

(** [get_team n t] will return tuple of [(current_team, opposing team)]. 
    [current_team] denotes that it is this team's turn to attack. 
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
      choose a character with that id in t.*)
val print_char_select: Character.t -> int -> unit

(** [player_pick k lst] allows the user to pick k characters from a [lst],
    which is the list of character ids. Returns the int list they chose 
    Requires: k > 0*)
val player_pick : int -> int list -> int list

(** [random_pick_char t k] allows users to pick 
    [k] character from [choices] number of choices. 
    Requires : [max_id] must be less than the number of possible character *)
val random_pick_char : Character.t -> int -> int -> int -> int list

(** [random_clst n t k max_id choices] gives the user a random list of 
    characters containing [choices] number of characters. They are allowed to 
    choose [k] characters, and then returns a Character.c list containing 
    their choices.
    Requires : [max_id] must be less than the number of possible character *)
val random_clst : int -> Character.t -> int -> int -> int -> Character.c list






(** [init clst1 clst2 items] initializes a game state t with the given 
    character and level pair lists, with [items] loaded *)
val init: (Character.c * int) list -> (Character.c * int) list -> 
  Adventure.item list -> t


(** [start clst1 clst2 items] initializes a game state t with the given 
    character and level pair lists, with the given [items]
    raises an exception for who wins combat. 
    [Winner 1] for team 1 win, [Winner 2] for team 2 win.
    Once combat ends, it must allow the function [winner] to return the
    correct winner*)
val start: (Character.c * int) list -> (Character.c * int) list -> 
  Adventure.item list -> unit

(** [winner] returns the winner of combat. Is 0 if game is still going on *)
val winner: t -> int

(** [mult_start] initiates combat, with character/moves contained in [t]*)
val mult_start: Character.t -> unit

(** [load_char (char, lvl)] loads [char] from Character into a [c] type record
    with level [lvl]*)
val load_char: Character.c * int -> c 

(** [is_on_cd cd] returns true if [cd] is on cd, which means its 
    "turns_left" field above 0. *)
val is_on_cd: move_cd -> bool

(** [add_cd move cd_lst] adds [move] to the cooldown list [cd_lst], with 
    the move's given cooldown*)
val add_cd: Character.move -> cd_lst -> cd_lst

(** [update_cd cd] updates the cooldown entry by decreasing 1 to
    its cooldown *)
val update_cd: move_cd -> move_cd

(** [update_cd_lst cd_lst] updates the cooldown list using [update_cd]. 
    If a move's cooldown reaches 0, then it is removed from the list.  *)
val update_cd_lst: cd_lst -> cd_lst

(** [add_cd_to_char c move] returns a cd_lst of character [c] with [move] 
    on cooldown*)
val add_cd_to_char:  c -> Character.move -> move_cd list

(** [move_on_cd cd_lst move] checks if [move] is on cooldown, looking at 
    [cd_lst] *)
val move_on_cd: move_cd list -> Character.move -> bool

(** [moves_off_cd move_lst cd_lst] returns a list of all moves from [move_lst] 
    that are off_cooldown in [cd_lst] *)
val moves_off_cd: Character.move list -> move_cd list -> Character.move list

(** [char_moves_off_cd c] returns a list of all moves that [c] has that 
    are off_cd *)
val char_moves_off_cd: c -> Character.move list

(** [update_cd_team team] updates all character's cooldowns on [team] *)
val update_cd_team: team -> unit

(** [set_teamlvl team lvl] returns a list of (Character.c, level) pair list
    with level [lvl] *)
val set_teamlvl: Character.c list -> int -> (Character.c * int) list
(* Sp combat functions *)

(** [start_sing clst1 clst2 items] is similar to [start], 
    but clst2 will be controlled
    by the computer, and [items] are loaded in.*)
val start_sing: (Character.c * int) list -> (Character.c * int) list -> 
  Adventure.item list -> int * item list

(** [rand_in_lst lst] returns a random element in lst *)
val rand_in_lst : 'a list -> 'a

(** [start_t_sing t] executes turns for the single player mode *)
val start_t_sing: t -> unit 

(**[smartness_of_c char] is the smartness of the character [char]*)
val smartness_of_c: c -> int 

(** [calc_dmg move atk target] is the damage done by [move] with [atk] to 
    [target]. Takes into account effectiveness *)
val calc_dmg: Character.move -> int -> c -> float 
