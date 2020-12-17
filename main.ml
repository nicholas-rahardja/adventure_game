open OUnit2

open Character


let j1 = Yojson.Basic.from_file "json/charmovetest.json"

let t1 = from_json j1
let adventure_t = 
  Adventure.from_json (Yojson.Basic.from_file "adventure_test.json")



let getextract_char id = match
    (get_char t1 id) with
| None -> failwith "character not found"
| Some x -> x

let michael = getextract_char 1
let gries = getextract_char 2 
let xenon = getextract_char 7
let mermaid = getextract_char 12
let fairy = getextract_char 5
let imp = getextract_char 3


let player_team = [michael; imp; mermaid]
let test_team2 = [fairy; fairy; fairy]

(* processes int list into char.c list *)

let char_list intlst = List.map (getextract_char) intlst

let print_int_line adv_t x = 
  Printf.printf "Room %d : Go to " x ;
  let room_name = (Adventure.room_name adv_t x) in 
  ANSITerminal.(print_string [blue] ( room_name ^ "\n"))


let print_rooms room_lst adv_t = List.iter (print_int_line adv_t) room_lst


let rec match_move t = 
  try begin
    match State.move t (read_int ()) with 
    | Illegal -> print_endline "enter a valid room exit"; match_move t
    | Legal x -> x

  end
  with 
  | _ -> print_endline "enter a valid room exit"; match_move t


(** [one_round state adv_t] carries out the combat in a single room, and then
    allows the player to choose the next exits. *)
let rec one_round (state : State.t) adv_t = 
  print_newline ();
  let cur_room = State.get_room state in 
  let enemies = Adventure.enemies adv_t cur_room in 
  if List.length enemies = 0 then print_endline "No enemies found\n" else
    (* Load player_team from State.t, and load enemies from Adventure.enemies *)
    (ANSITerminal.(print_string [red] "An enemy has attacked! \n\n");
     Combat.start_sing player_team (char_list enemies));
  print_endline "Where do you want to go next?";
  let x = Adventure.next_rooms adv_t cur_room in 
  print_rooms x adv_t;
  print_endline "Type in the int of the room:";
  let new_state = match_move state in 
  one_round new_state adv_t


let sing_player () = 
  ANSITerminal.( print_string [red] "Welcome Adventurer! \n ");
  print_endline "You must defeat the enemies and reclaim
this land!!";
  let game_state = State.init_state adventure_t player_team in 
  one_round game_state adventure_t



let mult_player () = Combat.mult_start t1

let rec go () = 
  print_endline "Type 1 for multiplayer,\n or Type 2 for single player ";
  let choice = read_int () in 
  print_newline ();
  if choice = 1 then mult_player ()
  else if choice = 2 then sing_player ()
  else go ()



let go_t = 
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Combat-based Game System!\n\n");
  go ()