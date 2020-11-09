(** A module representing combat*)
(**[type c] represents one character, and their current hp. Must include Character.c in its representation *)
type c

(** [team] represents a team of characters. If a character is dead, then it is None. If a character is alive,
    it is Some c *)
type team = c list

(** [type t] represents the current state of combat. Should include a list of team1 and and team2. Should include
    a field containing a variant of whose turn it is. Includes the winner of the game, '1' for team 1, 
    '2' for team 2, or 'in_progress' if no winner yet *)
type t

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


