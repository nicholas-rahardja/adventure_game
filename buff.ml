(*
type buff = 
  | Dot of string * int * int
  | Hot of string * int * int
  | DmgDoneMod of string * int * int
  | DmgReceivedMod of string * int * int
  | ElementalDmgDoneMod of string * int * int
  | ElementalVulnerability of string * int * int

let process_end_of_turn target buff = 
  match buff with 
  | Dot (name, x) -> Combat.do_dmg target x
  | Hot (name, x) -> Combat.do_heal target x 
  | _ -> ()

let int_to_percent x = 
  let flt = float_of_int x in 
  flt /. 100.

let process_defender_buff defender move buff = 
  match buff with 
  | DmgReceivedMod (name, x) -> (* x = 10 means 10% more damage taken *)
    (* x = -10 means -10% damage taken *)
    int_to_percent x 
  | ElementalVulnerability (name, x) -> 
    if Character.get_effectiveness move defender > 1. then int_to_percent x else
      int_to_percent 0
  | _ -> 0.

let get_move_type move = failwith "TODO"

let get_char_type c = failwith "TODO"

let process_attacker_buff attacker move buff = 
  match buff with 
  | DmgDoneMod (name, x) -> int_to_percent x 
  | ElementalDmgDoneMod (name, x) ->
    if get_char_type attacker = (get_move_type move)
    then int_to_percent x else 0.
  | _ -> 0.

  *)