(** A module handing all buffs/debuffs, including status effects, and damage
    over time or healing over time*)
(*
(** Representation type for a buff *)
type buff

(** [process_end_of_turn buff target] processes [buff] at the end of turn 
    effects of [target]*)
val process_end_of_turn: Combat.c -> buff -> unit

(** [int_to_percent x] turns the integer to its appropriate percentage value 
    as a float
    Example: x= 10% turns into 0.1 *)
val int_to_percent: int -> float

(** [process_defender_buff defender move buff] returns a modifier to boost 
    or decrease the damage taken (if a negative number is returned) by [defender],
    due to the buff [buff]
    Example: returning 0.1 means the damage will be boosted by 10% *)
val process_defender_buff : Character.c -> Character.move -> buff -> float

(** [process_attacker_buff attacker move buff] returns a modifier to boost
    or decrease the damage done by [attacker] due to buff [buff] *)
val process_attacker_buff : Character.c -> Character.move -> buff -> float
*)