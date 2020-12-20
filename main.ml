open Character

let j1 = Yojson.Basic.from_file "json/charmove_game.json"

let t1 = from_json j1
let adventure_t = 
  Adventure.from_json (Yojson.Basic.from_file "json/adventure_game.json")

type player_choice = 
  | Yes
  | No
  | Invalid

let enemy_join_chance = 50

let rooms_visited_count = ref 0

let starting_room_id = ref ~-9999

let ending_room_id = 8

let print_yellow_win_msg () = 
  ANSITerminal.(print_string 
                  [yellow] "Congratulations!!! You have completed the game! 
  Thank you for playing!\n")

let print_lost_msg () = print_endline "Please restart the game to play again!\n"

(** Exception when player doesnt want to buy an item *)
exception NoItemSelected

(** Exception when player has lost single player *)
exception SinglePlayerLost

(** Exception when player decides to not load a game file *)
exception QuitGameLoading

(** Exception when player has defeated the opponents in the last room *)
exception SingPlayerCompleted

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
let imp = getextract_char 3

let player_team = [michael; gries; imp]

(* processes int list into char.c list *)

let char_list intlst = List.map (getextract_char) intlst

let print_red str = 
  ANSITerminal.(print_string [red] str)

let print_int_line adv_t x = 
  Printf.printf "Room %d : Go to " x ;
  let room_name = (Adventure.room_name adv_t x) in 
  ANSITerminal.(print_string [blue] ( room_name ^ "\n"))

let print_rooms room_lst adv_t = 
  List.iter (print_int_line adv_t) room_lst

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
  if Combat.proc enemy_join_chance then (
    let rand_choice = Combat.rand_in_lst enemy_team in 
    print_newline ();
    let char_name = Character.get_char_name rand_choice in 
    print_red char_name;
    Printf.printf " with level %d wants to join your team! \n" difficulty;
    Printf.printf 
      "Type 'y' to let him join your adventures! or type 'n' to decline \n" ;
    add_to_team_helper rand_choice state difficulty
  )
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
  Printf.printf "You have received %d gold! \n\n" amt;
  State.add_gold amt state

let handle_winner integer = 
  if integer = 2 then begin 
    print_endline "Oh no! Your team has fallen! Better luck next time!";
    raise SinglePlayerLost
  end
  else ()

let init_combat cur_room enemies state adv_t = 
  let player_team = State.get_char_with_xp_lst state in 
  let difficulty = Adventure.difficulty adv_t cur_room in 
  let pairs = Combat.set_teamlvl enemies difficulty in 
  let items = State.get_inventory state in 
  print_red "An enemy has attacked! \n\n";
  let (winner, items) = Combat.start_sing player_team pairs items in 
  handle_winner winner;
  let state_update_items = State.load_inventory items state in 
  print_red "You won! \n";
  let exp_gained = State.xp_of_lvl difficulty in 
  Printf.printf "Your characters gained %d experience \n\n" exp_gained;
  let state_ref = ref state_update_items in
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
      Printf.printf "You found %s! You put it in your inventory\n" item_name;
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

let ask_shop_helper shop = 
  print_item_lst shop ();
  let input = read_line () in 
  check_buy_none input;
  let choice = int_of_string input - 1 in 
  List.nth shop choice 

let rec ask_shop_item shop state = 
  try
    let item_choice = ask_shop_helper shop in 
    if State.get_gold state < item_choice.price then (
      print_endline "You don't have enough gold!";
      print_endline "The shopkeeper says: No money, No buy!";
      ask_shop_item shop state )
    else begin 
      let item_name = Adventure.item_string item_choice.item in 
      Printf.printf "You brought ";
      ANSITerminal.(print_string [blue] item_name);
      print_newline ();
      let sub_gold_state = State.sub_gold item_choice.price state in 
      State.add_inventory item_choice.item sub_gold_state
    end
  with e -> 
  match e with 
  | NoItemSelected -> print_endline "You decided not to buy anything.\n"; state
  | _ -> print_endline "enter a valid int"; ask_shop_item shop state

let state_shop_helper shop state = 
  if List.length shop = 0 then state else begin
    print_endline "A shopkeeper greets you: ";
    ANSITerminal.(print_string [blue] "\nWelcome to my shop! Have a look \
                                       around...\n");
    print_endline "Type the int of the item you want. You can only buy one.";
    print_string "Or type ";
    print_red "'none'";
    print_endline " to buy nothing";
    print_endline "You currently have: ";
    let cur_gold = State.get_gold state |> string_of_int in
    ANSITerminal.(print_string [yellow] (cur_gold ^ " gold\n") );
    let new_state = ask_shop_item shop state in 
    print_endline "The shopkeeper says: Come again anytime!\n";
    new_state
  end

let rec ask_save_yes state = 
  print_endline "Type the name of the file you would like to save it as:";
  print_endline"For example, type 'save1' or 'Clarksons_adventure'";
  try 
    let input = read_line () in 
    let file_name =  input ^ ".json" in
    let path = "./" ^ file_name in 
    Printf.printf "Saved as %s \n\n" file_name;
    State.save state path
  with _ -> print_endline "That is not a valid file name, please try again";
    ask_save_yes state

(** asks the user to save progress *)
let rec ask_save state = 
  try
    print_string "Would you like to save your progress?";
    print_endline "Type 'yes' to save.\nType 'no' to not save" ;
    let input = read_line () in 
    match input with 
    | "yes" | "y" -> ask_save_yes state
    | "no" | "n" -> ()
    | _ -> failwith "not valid option"

  with _ -> 
    print_endline "\nplease type yes or no"; ask_save state 

let print_room room adv_t = 
  let name = Adventure.room_name adv_t room in 
  print_string "\nYou are currently at ";
  ANSITerminal.(print_string [green] name)

let first_room cur_room exp = 
  if !rooms_visited_count > 1 then exp ();
  if cur_room <> !starting_room_id  then incr rooms_visited_count 

(** [one_round state adv_t] carries out the combat in a single room, and then
    allows the player to choose the next exits. *)
let rec one_round (state : State.t) adv_t = 
  let cur_room = State.get_room state in 
  print_room cur_room adv_t;
  print_newline ();
  first_room cur_room (fun _ -> ask_save state);
  let enemies = Adventure.enemies adv_t cur_room |> char_list in 
  let new_state = 
    if List.length enemies = 0 then state else 
      init_combat cur_room enemies state adv_t
  in
  if cur_room = ending_room_id then raise SingPlayerCompleted;
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
  try
    print_red "Welcome Adventurer! \n ";
    print_endline "You must defeat the enemies and reclaim this land!! \n";
    let game_state = State.init_state adventure_t player_team in 
    starting_room_id := State.get_room game_state;
    one_round game_state adventure_t
  with e -> 
  match e with 
  | SinglePlayerLost -> print_lost_msg ()
  | SingPlayerCompleted -> print_yellow_win_msg ()
  | _ -> ()

let print_load_msg () = 
  print_endline "Type the name of your save file. 
For example: 'save1' or 'Clarksons_Adventures'\n";
  print_string "Or type ";
  print_red "'quit'"; 
  print_endline " to return to the main menu"

let rec load_game () = 
  print_load_msg ();
  try
    let input = read_line () in 
    if input = "quit" then raise QuitGameLoading 
    else (
      let path = "./" ^ input ^ ".json" in 
      let state = State.load adventure_t t1 path in 
      Printf.printf "Successfully loaded file %s\n" input;
      one_round state adventure_t
    )
  with e -> match e with 
    | QuitGameLoading -> raise QuitGameLoading
    | SinglePlayerLost -> print_lost_msg ()
    | SingPlayerCompleted -> print_yellow_win_msg ()
    | _ -> 
      print_endline "That file is not found. Please try again."; load_game ()


let mult_player () = Combat.mult_start t1

let rec go () = 
  try
    print_endline 
      "Type 1 for multiplayer,\nor Type 2 for single player story mode,
or Type 3 to load a saved game file ";
    let choice = read_int () in 
    print_newline ();
    match choice with 
    | 1 -> mult_player ()
    | 2 -> sing_player ()
    | 3 -> load_game ()
    | _ -> print_endline "Please enter 1,2, or 3"; go ()
  with QuitGameLoading -> go ()

let go_t = 
  print_red "\n\nWelcome to the 3110 Combat-based Game System!\n\n";
  go ()