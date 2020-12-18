open OUnit2

open Character


let j1 = Yojson.Basic.from_file "json/charmovetest.json"

let t1 = from_json j1
let adventure_t = 
  Adventure.from_json (Yojson.Basic.from_file "adventure_test.json")

type player_choice = 
  | Yes
  | No
  | Invalid

let enemy_join_chance = 100

(** Exception when player doesnt want to buy an item *)
exception NoItemSelected
let counter = ref 1
(** [print_char_name c] prints:
    "Type n to remove [c]'s name" Where n is the number in the party.*)
let print_char c = 
  Printf.printf "Type %d to remove " !counter;
  let char_name = Character.get_char_name c in 
  ANSITerminal.(print_string [blue] char_name);
  print_newline ();
  incr counter

(** [print_replace team] prints the characters that can be replaced *)
let print_replace team = 
  print_endline "which character to do you want to replace? Type the int: ";
  fun () ->
    List.iter (print_char) team; print_newline ();
    counter := 1

let getextract_char id = 
  let opt = (get_char t1 id) in 
  Option.get opt


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

(* allows player to recruit an enemy *)
let player_input input = 
  match String.lowercase_ascii input with 
  | "yes" | "y" -> Yes
  | "no" | "n" -> No 
  | _ -> Invalid


let rec add_to_team_helper char state lvl = 
  let user_input = player_input (read_line ()) in 
  match user_input with 
  | Invalid -> 
    print_endline "Please type 'yes' or 'no'";
    add_to_team_helper char state  lvl
  | No -> 
    print_endline "You have decided to keep your current team"; state
  | Yes -> add_to_team_yes char state lvl


and add_to_team_yes char state lvl = 
  try begin
    let cur_party = State.get_chars state in 
    print_replace cur_party ();
    let choice = read_int () in 
    let index = choice - 1 in
    let state' = State.remove_char index state in 
    let total_xp = State.xp_of_lvl lvl in 
    let state'' = State.add_char char ~xp:total_xp ~-1 state' in 
    let char_name = (Character.get_char_name char) in 
    Printf.printf "A level %d %s has joined your team! \n" lvl char_name;
    state''
  end
  with _ -> print_endline "that is not a valid choice, please type a valid int";
    add_to_team_yes char state lvl



let add_to_team enemy_team state difficulty = 
  if Combat.proc enemy_join_chance then begin 
    let rand_choice = Combat.rand_in_lst enemy_team in 
    let char_name = Character.get_char_name rand_choice in 
    ANSITerminal.(print_string [red] char_name);
    Printf.printf " with level %d wants to join your team! \n" difficulty;
    Printf.printf 
      "Type 'y' to let him join your adventures! or type 'n' to decline \n" ;
    add_to_team_helper rand_choice state difficulty
  end
  else state


let rec match_move t = 
  try begin
    match State.move t (read_int ()) with 
    | Illegal -> print_endline "enter a valid room exit"; match_move t
    | Legal x -> x

  end
  with 
  | _ -> print_endline "enter a valid room exit"; match_move t

(** from lst = [x0,x1,...,xn], return [0,1,...,n] *)
let lst_index_maker lst = 
  let func index _ = index in 
  List.mapi func lst

(** adds xp, and prints out level up message if level up, returns new state *)
let add_xp_func n exp state = 
  let (state', b) = State.add_xp n exp state in 
  if b then begin 
    let char_name = State.get_char n state' |> Character.get_char_name in 
    let new_lvl = State.get_level n state' in 
    ANSITerminal.(print_string [blue] char_name);
    Printf.printf " has reached level %d! \n"  new_lvl;
    state'
  end
  else state'

(** Grants player gold based on enemy lvl *)
let add_gold_helper lvl state = 
  let amt = 10 * lvl in 
  Printf.printf "You have received %d gold! \n" amt;
  State.add_gold amt state

let init_combat cur_room enemies state adv_t = 
  let player_team = State.get_char_with_xp_lst state in 
  let difficulty = Adventure.difficulty adv_t cur_room in 
  let pairs = Combat.set_teamlvl enemies difficulty in 
  ANSITerminal.(print_string [red] "An enemy has attacked! \n\n");
  Combat.start_sing player_team pairs;
  ANSITerminal.(print_string [red] "You won! \n");
  let exp_gained = State.xp_of_lvl difficulty in 
  Printf.printf "Your characters gained %d experience \n" exp_gained;
  let state_ref = ref state in
  for i = 0 to List.length (player_team) - 1 do 
    state_ref := (add_xp_func i exp_gained !state_ref)
  done; 
  let state_after_gold = add_gold_helper difficulty !state_ref in
  let state_after_enemy = add_to_team enemies state_after_gold difficulty in
  state_after_enemy

let add_rewards item_lst state = 
  let state_ref = ref state in 
  List.iter (fun item -> 
      let item_name = Adventure.item_string item in 
      Printf.printf "You found %s! You put it in your inventory\n5" item_name;
      let new_state = State.add_inventory item !state_ref in 
      state_ref := new_state) item_lst ; !state_ref

let counter_shop = ref 1

let print_item item = 
  Printf.printf "Type %d to buy " !counter_shop;
  let item_name = Adventure.item_wrapper_string item in 
  ANSITerminal.(print_string [blue] item_name);
  print_newline ();
  incr counter_shop

let print_item_lst item_lst = 
  fun () ->
  List.iter (print_item) item_lst; print_newline ();
  counter_shop := 1

let check_buy_none input = 
  if input = "none" then raise NoItemSelected else ()

let rec ask_shop_item shop state = 
  try
    print_item_lst shop ();
    let input = read_line () in 
    check_buy_none input;
    let choice = int_of_string input - 1 in 
    let item_choice = List.nth shop choice in 
    if State.get_gold state < item_choice.price then begin
      print_endline "The shopkeeper says: No money, No buy!";
      ask_shop_item shop state end 
    else begin 
      let item_name = Adventure.item_string item_choice.item in 
      Printf.printf "You brought %s \n" item_name;
      State.sub_gold item_choice.price state
    end
  with e -> 
  match e with 
  |NoItemSelected -> print_endline "You decided not to buy anything"; state
  | _ -> print_endline "enter a valid int"; ask_shop_item shop state


let state_shop_helper shop state = 
  if List.length shop = 0 then state else begin
    print_endline "A shopkeeper greets you: ";
    ANSITerminal.(print_string [blue] "Welcome to my shop! Have a look around...\n");
    print_endline "Type the int of the item you want. You can only buy one.";
    print_endline "Or type 'none' to buy nothing";
    let new_state = ask_shop_item shop state in 
    print_endline "The shopkeeper says: Come again anytime!";
    new_state


  end


(** [one_round state adv_t] carries out the combat in a single room, and then
    allows the player to choose the next exits. *)
let rec one_round (state : State.t) adv_t = 
  print_newline ();
  let cur_room = State.get_room state in 
  let enemies = Adventure.enemies adv_t cur_room |> char_list in 
  let new_state = 
    if List.length enemies = 0 then (print_endline "No enemies found\n"; state) else 
      init_combat cur_room enemies state adv_t
      (* init_combat cur_room enemies state adv_t  end *)
  in
  let rewards = Adventure.rewards adv_t cur_room in 
  let state_after_rewards = add_rewards rewards new_state in
  let shop_items = Adventure.shop adv_t cur_room in 
  let state_after_shop = state_shop_helper shop_items state_after_rewards in 
  print_endline "Where do you want to go next?";
  let x = Adventure.next_rooms adv_t cur_room in 
  print_rooms x adv_t;
  print_endline "Type in the int of the room:";
  let new_state' = match_move state_after_shop in 
  one_round new_state' adv_t


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