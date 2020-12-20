open Printf
exception Winner of int


type move_cd = 
  {move: Character.move;
   turns_left : int;}

type cd_lst = move_cd list

type item = Adventure.item

exception RevivalItemException
exception NoItemSelected
exception LeftOverItems of item list

type c = {
  char_c: Character.c;
  char_name: string;
  char_moves: Character.move list;
  atk: int;
  level: int;
  mutable cur_hp: int;
  mutable buffs : unit list; 
  mutable active: bool;
  mutable cooldown: cd_lst;}

type char_t = Character.t

type team = c list

(**{team1 = [c1; c2]}; team2 = [c3; c4]; winner = 1; 
   items= [item1; item2; item_n]} represents the combat where there are 2 teams
   team 1 and team 2. Where team 1 has characters c1 and c2, and team 2 has 
   characters c3 and c4. Items are all the items held by team 1 (only valid in 
   single player mode). The winner of the game is team1. Note if winner is 0, 
   then no team has won yet.
   RI:  [winner] can only be 0,1,2*)
type t = {
  team1: team;
  team2: team;
  mutable winner: int;
  mutable items: Adventure.item list;
}

type move_select =
  | Valid_m of Character.move
  | Invalid_m

type target_select = 
  | Valid_tar of c
  | Invalid_tar

type item_select = 
  | ValidItem of item
  | InvalidItem

(* Constants *)
let max_char_per_team = 3
let max_char_id = 12
let max_selections = 7

let dmg_variation = 5
let health_mod = 1
let delay_time = 1
let multiplayer_base_lvl = 3

let revive_base_hp = 0.5

(* Single player constants *)
let dmg_mod = 2

exception WinnerSingPlayer of int * (item list)

let vary (k : float) percent = 
  let rand_int = Random.int (percent * 10 + 1) in
  let rand_float = float_of_int (rand_int) /. 10. in 
  let mult = rand_float /. 100. in 
  let pos_or_neg = 
    if Random.bool () then float_of_int ~-1
    else 1. in 
  let offset = k *.  (mult *. pos_or_neg) in
  k +. offset

let proc k =
  let rand_int = Random.int 100 + 1 in
  rand_int <= k

(** [blue_char_name c] prints [c]'s name in blue. *)
let blue_char_name c = ANSITerminal.(print_string [blue] c.char_name)

let do_dmg c (dmg : int) = 
  (blue_char_name c; 
   Printf.printf " has taken %d damage\n" dmg);
  let remaining_health = c.cur_hp - dmg in
  if remaining_health > 0 then begin
    blue_char_name c;
    Printf.printf " has %d health left\n" remaining_health;
    c.cur_hp <- remaining_health end else
    (blue_char_name c;
     Printf.printf " has fallen!\n";
     c.cur_hp <- 0;  c.active <- false);
  print_newline ()

let do_heal c heal =  
  (blue_char_name c; 
   Printf.printf " has healed by %d \n" heal);
  let added_health = c.cur_hp + heal in 
  let max_hp = health_mod * Character.get_char_hp_lvl c.char_c c.level in 
  let final_health = if added_health > max_hp then max_hp 
    else added_health in
  c.cur_hp <- final_health; 
  blue_char_name c; 
  Printf.printf " healed to %d hp \n" final_health

(** [getextract_char c] extracts the c from the c option*)
let getextract_char c = 
  Option.get c

let is_active c = c.active

let is_dead c = not c.active

let get_active team = 
  List.filter is_active team

let get_dead team = List.filter is_dead team

(** [target index team]. Returns the character at index [index].
    Requires: all characters in team must be active
    index must be < team.length *)
let target index (team : team) = 
  index - 1 |> List.nth team

let counter = ref 1

(** [print_char_name inte c] prints:
    "inte target is c_name" where c_name is the char_name of c*)
let print_char_name c= 
  Printf.printf "Target %d : " !counter;
  blue_char_name c;
  Printf.printf " ~ %i remaining health left \n" c.cur_hp;
  incr counter


(** [print_targets team] prints out the possible targets to choose *)
(* bol = true if targeting attack. false if target to use item on *)
let print_targets team = 
  fun () ->
  List.iter (print_char_name) team; print_newline ();
  counter := 1

(** [ask_user] asks the user to pick an int, and will evaluate to that int *)
let ask_user = read_int

(** DEBUG CODE *)
let print_cd cd = 
  let move_name = Character.get_move_name cd.move in
  Printf.printf " %s has %d turns remaining \n" move_name cd.turns_left

let is_on_cd cd = 
  cd.turns_left > 0

let add_cd move cd_lst = 
  let cd = Character.get_move_cd move in 
  let new_entry =  {move = move;
                    turns_left = cd;} in 
  new_entry :: cd_lst

let update_cd move_cd = 
  (* Printf.printf " current move cd is %d" move_cd.turns_left; *)
  let next_cd = move_cd.turns_left - 1 in 
  let new_cd = {move_cd with turns_left = next_cd} in 
  (* print_cd new_cd; *)
  new_cd

let update_cd_lst cd_lst = 
  let not_trimmed_lst = List.map update_cd cd_lst in 
  List.filter is_on_cd not_trimmed_lst

let update_cd_char c = 
  let new_lst = update_cd_lst c.cooldown in 
  c.cooldown <- new_lst

let update_cd_team team = 
  List.iter update_cd_char team

let add_cd_to_char c move = 
  add_cd move c.cooldown 

let reassign_char_cd c move = 
  let new_lst = add_cd_to_char c move in 
  print_int ((List.nth new_lst 0).turns_left);
  print_newline ();
  c.cooldown <- new_lst

let target_input team input = 
  try
    let input_int = int_of_string input in 
    let target = target input_int team in 
    Valid_tar target
  with _ -> Invalid_tar

let rec select_enemy team = 
  print_newline ();
  let act_team = get_active team in
  print_targets act_team  ();
  let input = read_line () in 
  let target = target_input act_team input in 
  match target with 
  | Valid_tar x -> x
  | Invalid_tar -> 
    print_endline "\n That is not a valid target, please choose again: \n"; 
    select_enemy team

let is_team_dead act_team = act_team = []

let check_winner act_team inte= 
  if is_team_dead act_team then raise (Winner inte) else ()


let print_item_use name c = 
  Printf.printf "You used %s on " name;
  blue_char_name c; print_newline ()

let flat_hp name value c = 
  print_item_use name c;
  do_heal c value

let percent_hp name percent c = 
  print_item_use name c;
  let max_hp = Character.get_char_hp_lvl c.char_c c.level * health_mod
               |> float_of_int in 
  let hp_restored = max_hp *. percent |> int_of_float in 
  do_heal c hp_restored

let revival_item name c = 
  percent_hp name revive_base_hp c; c.active <- true;
  let char_name = c.char_name in 
  Printf.printf 
    "%s has been revived, but they are too weak to attack this turn!" char_name


let match_item item c = 
  match item with
  | Adventure.RevivalItem name -> revival_item name c
  | Adventure.FlatHp (name, value) -> flat_hp name value c
  | Adventure.PercentHp (name, percent) -> percent_hp name percent c



let print_item n item = 
  let item_name = Adventure.item_string item in 
  Printf.printf "Type %d to use item %s \n" (n + 1) item_name

(* Helper to remove [n]th element from list [lst] *)
let rec remove n lst = 
  match lst with 
  | [] -> []
  | h :: t -> if n = 0 then t else h :: remove (n - 1) t

let print_item_choices items = 
  print_endline "You may use an item";
  print_string "Type the int to select your item. Or type ";
  ANSITerminal.(print_string [red] "0");
  print_endline " to not use any item \n";
  List.iteri (print_item) items

(* let item_input items input = 
   try 
   let integer = int_of_string input in 
   match integer with 
   | 0 -> 0
   | x when (x <= List.length items) && (x>0) -> x
   | _ -> InvalidItem *)


let rec revival_item_select_helper team = 
  let act_team = get_active team in 
  if 0 = List.compare_lengths team act_team then (
    print_endline "All your team is alive! You cannot use this a revival item";
    raise RevivalItemException) else begin
    let dead_team = get_dead team in 
    Printf.printf 
      "Please enter a target number of your team to use the item on: \n";
    print_targets dead_team ();
    let input = read_line () in 
    let target = target_input dead_team input in 
    match target with 
    | Invalid_tar -> 
      print_endline "that is not a valid target, please type again"; 
      revival_item_select_helper team
    | Valid_tar c -> c
  end

let no_item_exp input = 
  if input = 0 then raise NoItemSelected


(** PH function. Will be used to allow usage of items *)
let rec use_item team items t = 
  try
    if List.length items > 0 then begin 
      print_item_choices items;
      let input = read_int () in 
      no_item_exp input;
      let item_selected = List.nth items (input - 1) in 
      let char_selected = begin
        match item_selected with 
        | Adventure.RevivalItem _ -> 
          revival_item_select_helper team
        | _ -> print_endline "Please enter the integer to select which
        character to use this item on";
          select_enemy team
      end in 
      match_item item_selected char_selected;
      let new_item_lst = remove (input - 1) items in 
      t.items <- new_item_lst


    end
  with e -> match e with 
    | NoItemSelected -> print_endline "you decided not to use an item"
    | RevivalItemException -> use_item team items t
    | _ -> print_endline "that is not a valid item, please type again"; 
      use_item team items t

let is_move input move = 
  let move1 = Character.get_move_name move |>  String.lowercase_ascii in 
  let move2 = (input |>  String.lowercase_ascii) in 
  move1 = move2

let move_input (move_lst : Character.move list) input = 
  try 
    let correct_move = List.find (is_move input) move_lst in
    Valid_m correct_move
  with
    _ -> Invalid_m

let move_on_cd_helper move cd = 
  let move1_name = Character.get_move_name cd.move in
  let move2_name = Character.get_move_name move in
  move1_name = move2_name

let move_on_cd cd_lst move = 
  let lst = List.filter (move_on_cd_helper move) cd_lst in 
  List.length lst = 0

let moves_off_cd move_lst cd_lst = 
  List.filter (move_on_cd cd_lst) move_lst

let char_moves_off_cd c = 
  let char_moves = c.char_moves in 
  let cd_lst = c.cooldown in 
  moves_off_cd char_moves cd_lst

let rec select_move (move_list: Character.move list) =
  let print_move (m : Character.move) = 
    Printf.printf "Move option : '%s'\n" (Character.get_move_name m) in
  List.iter print_move move_list; 
  print_endline "\nPlease enter a move name to attack your enemy:"; 
  let move_chosen = read_line () in
  let result = move_input move_list move_chosen in 
  match result with 
  | Valid_m x -> x
  | Invalid_m -> 
    print_endline "\nthat is not a valid move, please choose agin: \n";
    select_move move_list

let calc_dmg move atk target = 
  let base = Character.get_move_atk move |> float_of_int in 
  let offset = float_of_int atk *. Character.get_scale move in 
  let dmg = base +. offset in 
  dmg *. Character.get_effectiveness move target.char_c

let vary_dmg attacker move target = 
  let atk = attacker.atk in 
  let fix_dmg = calc_dmg move atk target in 
  let vary_dmg = vary fix_dmg dmg_variation |> int_of_float in 
  do_dmg target (vary_dmg * dmg_mod)

let use_move n opp_team c = 
  let act_enemy = get_active opp_team in 
  check_winner act_enemy n;
  blue_char_name c;
  print_string " is attacking: \n\n";
  let act_moves = char_moves_off_cd c in 
  if List.length act_moves = 0 then 
    print_endline "All this character's moves are on cooldown, can't attack!\n"
  else begin
    let move = select_move act_moves in 
    Printf.printf 
      "Please enter a target number of the opposition team to attack next: \n";
    let target = select_enemy act_enemy in 
    reassign_char_cd c move;
    vary_dmg c move target
  end

let get_team n t = 
  match n with
  | 1 -> (t.team1, t.team2)
  | 2 -> (t.team2, t.team1)
  | _ -> failwith "None"

let switch_n n = 
  match n with 
  | 1 -> 2
  | 2 -> 1
  | _ -> failwith "switch_n error, should not happen"

let print_green str =
  ANSITerminal.(print_string [green] str)

let team_n_turn n t : unit = 
  let (fri_team, enemy_team) = get_team n t in
  update_cd_team fri_team;
  let act_friendly = get_active fri_team in 
  check_winner (get_active enemy_team) n;
  check_winner (act_friendly) (switch_n n);
  print_green ("\nIt is team " ^ (string_of_int n) ^ "'s turn to start!\n\n");
  List.iter (use_move n enemy_team) act_friendly

let start_t t = 
  try begin
    while (true) do
      team_n_turn 1 t;
      team_n_turn 2 t;
    done
  end
  with Winner inte -> 
    Printf.printf "Congratulation, team %d is the Winner!\n\n" inte;
    t.winner <- inte

let winner t = t.winner

let print_char_select t id = 
  let char_name = 
    Character.get_char t id |> getextract_char |> Character.get_char_name
  in 
  Printf.printf "Type %d to select " id;
  ANSITerminal.(print_string [blue] char_name);
  print_newline ()

let rec player_pick_helper k (lst : int list) (acc : int list) = 
  if k = 0 then acc else begin 
    Printf.printf "Please pick %d more characters\n" k;
    let choice = ask_user () in
    if List.mem choice lst then player_pick_helper (k - 1) lst (choice :: acc)
    else (
      print_endline "That is not a character in the list, please pick again!";
      player_pick_helper k lst acc) end

let player_pick k lst = player_pick_helper k lst []

let random_pick_char t k max_id choices =
  print_endline "Choose your team";
  let lst = ref [] in
  while ((List.length !lst) < choices) do
    let rand_id = Random.int max_id |> (+) 1 in 
    lst := List.sort_uniq compare (rand_id :: !lst)
  done;
  List.iter (print_char_select t) !lst;
  print_endline 
    "Please enter the character number to select the character: \n";
  player_pick k !lst

let random_clst n t k max_id choices = 
  Printf.printf "Hi Player %d, it is you're turn to build your team!\n" n;
  let player_choicelst = random_pick_char t k max_id choices in
  let get_char_from_id id = Character.get_char t id |> getextract_char in 
  List.map get_char_from_id player_choicelst 

let load_char (char, lvl) = {
  char_c= char;
  char_name = Character.get_char_name char;
  char_moves = Character.get_moves char;
  cur_hp = (Character.get_char_hp_lvl char lvl) * health_mod;
  atk = Character.get_char_atk_lvl char lvl; 
  buffs= []; 
  active= true; 
  cooldown = [];
  level = lvl
}

let init_team clst = 
  List.map load_char clst 

let init clst1 clst2 items = {
  team1 = init_team clst1;
  team2 = init_team clst2;
  winner = 0;
  items = items;
}

let start clst1 clst2 items = 
  let init = init clst1 clst2 items in 
  start_t init

let set_teamlvl team lvl = 
  let pair_creator char = (char, lvl) in 
  List.map pair_creator team

let init_from_player t= 
  let team1 = random_clst 1 t max_char_per_team max_char_id max_selections in
  let team2 = random_clst 2 t max_char_per_team max_char_id max_selections in 
  let team_pair1 = set_teamlvl team1 multiplayer_base_lvl in 
  let team_pair2 = set_teamlvl team2 multiplayer_base_lvl in 
  init team_pair1 team_pair2

let mult_start t = 
  init_from_player t [] |> start_t 

(* BEGIN SINGLE PLAYER COMBAT MODULE *)

let rand_in_lst lst =   
  let range = List.length lst  in 
  let index = Random.int range in
  List.nth lst index

let print_move (m : Character.move) = 
  Printf.printf "Move option : '%s'\n" (Character.get_move_name m)

let rand_move c = 
  let act_moves = char_moves_off_cd c in 
  rand_in_lst act_moves

let rand_target team = 
  rand_in_lst team

let enemy_select_target team = rand_target team

let best_dmg_move c (target : c) = 
  let act_move_lst = char_moves_off_cd c in 
  let dmg move = Character.get_damage c.char_c target.char_c move in
  let acc_func acc move = (move, dmg move) :: acc in
  (* Compares in reverse order *)
  let compare_tuple (_, a) (_, b) = compare b a in 
  let sorted_lst = List.fold_left acc_func [] act_move_lst 
                   |> List.sort compare_tuple in
  let (move, dmg) = List.hd sorted_lst in (target, move, dmg)

let best_action c targets = 
  let lst = List.map (best_dmg_move c) targets in 
  let compare_tri_tuple (_, _ , a) (_, _, b) = compare b a in 
  let sorted_lst = List.sort compare_tri_tuple lst in 
  let (target, move, _) = List.hd sorted_lst in (move, target)

let enemy_select_action c targets chance : (Character.move * c) = 
  if proc chance then best_action c targets
  else (rand_move c, rand_target targets)

(** [print_flush] simply prints a new line, while flushing all 
    previous prints in the queue *)
let print_flush () = print_endline ""

(** [smartness = c.atk / 2.5, which is rougly its level * 2 ] *)
let smartness_of_c c = 
  let result = c.atk / 5 * 2 in 
  if result > 100 then 100 else result

let enemy_use_move_sing n opp_team c = 
  let act_enemy = get_active opp_team in 
  check_winner act_enemy n;
  blue_char_name c;
  print_string " is attacking:";
  print_flush ();
  Unix.sleep delay_time;
  let act_moves = char_moves_off_cd c in 
  if List.length act_moves = 0 then
    print_endline "This character has no available moves, can't attack!"
  else begin
    let smartness = smartness_of_c c in 
    let (move, target) = enemy_select_action c act_enemy smartness in
    reassign_char_cd c move;
    Printf.printf "%s used " c.char_name;
    ANSITerminal.(print_string [red] (Character.get_move_name move));
    print_flush ();
    Unix.sleep delay_time;
    vary_dmg c move target
  end

let execute_turn n t : unit = 
  let (fri_team, enemy_team) = get_team n t in
  update_cd_team fri_team;
  let act_friendly = get_active fri_team in 
  check_winner (get_active enemy_team) n;
  check_winner act_friendly (switch_n n);
  if n = 1 then begin
    ANSITerminal.(print_string [green] "\nIt is your turn to attack: \n\n");
    use_item fri_team t.items t; 
    List.iter (use_move n enemy_team) act_friendly
  end
  else begin
    ANSITerminal.( print_string [red] "The enemy is attacking! \n\n");
    List.iter (enemy_use_move_sing n enemy_team) act_friendly
  end

let start_t_sing t = 
  try begin
    while (true) do
      execute_turn 1 t;
      execute_turn 2 t;
    done
  end
  with Winner inte -> 
    t.winner <- inte; 
    raise (WinnerSingPlayer (t.winner, t.items))

let start_sing clst1 clst2 items = 
  try 
    let init_t = init clst1 clst2 items in 
    let _ = start_t_sing init_t in
    ANSITerminal.(print_string [red] "Congratulation, you defeated the enemy!\n");
    raise (Failure "Should not reach here")
  with WinnerSingPlayer (winner, items)  -> (winner, items)