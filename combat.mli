(** A module representing combat*)

(**[type c] represents one character, and their current hp. Must include Character.c in its representation *)
type c

(** [c_lst] represents a team of characters. If a character is dead, then it is None. If a character is alive,
    it is Some c *)
type c_lst = c option list

(** [type t] represents the current state of combat. Should include a list of team1 and and team2. Should include
    a field containing a variant of whose turn it is. Includes the winner of the game, '1' for team 1, 
    '2' for team 2, or 'in_progress' if no winner yet
*)
type t

(** [is_dead c] returns a boolean of whether c is dead. A character is dead if it's current reaches 0 or less*)
val is_dead c

(** [team_dead c] returns a . A team is dead when all characters on the team is dead*) 
