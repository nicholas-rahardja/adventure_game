open Combat

val mult_start: Character.t -> unit

(** [start] clst1 clst2 initializes a game state t with the given character lists,
    evaluates to a int for who wins combat. 1 for team 1 win, 2 for team 2 win.
    Once combat ends, it must allow the function [winner] to return the
    correct winner*)
val start: Character.c list -> Character.c list -> unit