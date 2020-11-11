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

let print_int_line x = print_newline (); print_int x

let print_rooms room_lst = List.iter (print_int_line) room_lst


let rec match_move t = match State.move t (read_int ()) with 
  | Illegal -> print_endline "enter a valid room exit"; match_move t
  | Legal x -> x


(** [one_round state adv_t] carries out the combat in a single room, and then
    allows the player to choose the next exits. *)
let rec one_round (state : State.t) adv_t= 
  let cur_room = State.get_room state in 
  let enemies = Adventure.enemies adv_t cur_room in 
  (* Load player_team from State.t, and load enemies from Adventure.enemies *)
  SinglePlayerCombat.start player_team (char_list enemies);
  print_endline "where do you want to go next?";
  print_endline "type in the int";
  let x = Adventure.next_rooms adv_t cur_room in 
  print_rooms x;
  print_newline ();
  let new_state = match_move state in 
  one_round new_state adv_t


let sing_player () = 
  let game_state = State.init_state adventure_t player_team in 
  one_round game_state adventure_t



let mult_player () = Combat.mult_start t1

let rec go () = 
  print_endline "Type 1 for multiplayer,\n or 2 for single player (single 
  player only testing for now";
  let choice = read_int () in 
  if choice = 1 then mult_player ()
  else if choice = 2 then sing_player ()
  else go ()



let go_t = 
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Combat-based Game System!\n");
  go ()