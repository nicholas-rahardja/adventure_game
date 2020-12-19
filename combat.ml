open Printf
exception Winner of int

type move_cd = 
  {move: Character.move;
   turns_left : int;}

type cd_lst = move_cd list

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

(* TODO *)
type item_select = 
  | Valid_item of unit
  | Invalid_item


(* Constants *)
let max_char_per_team = 3
let max_char_id = 12
let max_selections = 7

let dmg_variation = 5
let health_mod = 5
let delay_time = 1
let permanent = 9999999
let multiplayer_base_lvl = 10


(** [vary k percent] returns a value that deviates from [k] 
    by at most +- [percent]%.
    Requires: 0 <= percent <= 100*)
let vary (k : float) percent = 
  let rand_int = Random.int (percent * 10 + 1) in
  let rand_float = float_of_int (rand_int) /. 10. in 
  let mult = rand_float /. 100. in 
  let pos_or_neg = 
    if Random.bool () then float_of_int ~-1
    else 1. in 
  let offset = k *.  (mult *. pos_or_neg) in
  k +. offset

(** [proc k] is a random number generator. Returns true with a probability
    of k/100.
    Requires: 0 <= k <= 100 *)
let proc k =
  let rand_int = Random.int 100 + 1 in
  rand_int <= k

(** [blue_char_name c] prints [c]'s name in blue. *)
let blue_char_name c = ANSITerminal.(print_string [blue] c.char_name)


(** [do_dmg c dmg] inflicts [dmg] amount of damage on character c.
    Does not take into account damage variation, or buffs, etc. *)
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
  let max_hp = Character.get_char_hp_lvl c.char_c c.level in 
  let final_health = if added_health > max_hp then max_hp 
    else added_health in
  c.cur_hp <- final_health; 
  blue_char_name c; 
  Printf.printf " healed to %d hp" final_health


(** [getextract_char c] extracts the c from the c option*)
let getextract_char c = 
  Option.get c


(** [is_active c] returns true if a character's hp is above 0. *)
let is_active c = c.active

(** [get_active] returns a list with only the active characters *)
let get_active team = 
  List.filter is_active team

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
let print_targets team = 
  (* let counter = ref 0 in  *)
  Printf.printf "Please enter a target number of the opposition team to attack next: \n";
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

(* TODO *)
let get_move_cd move = 3

let add_cd move cd_lst = 
  let cd = get_move_cd move in 
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

(** Requires: team must be an active_team, meaning no one is dead *)
let target_input team input = 
  try
    let input_int = int_of_string input in 
    let target = target input_int team in 
    Valid_tar target
  with _ -> Invalid_tar

(** [select_enemy] ask the user to pick an enemy to target. They will have to 
    enter an int. 0 for the 1st target, 1 for the 2nd target, 2 for the 3rd 
    target. If the pick an int that is out of range, the function will ask again
*)
let rec select_enemy team = 
  print_newline ();
  let act_team = get_active team in
  print_targets act_team ();
  let input = read_line () in 
  let target = target_input act_team input in 
  match target with 
  | Valid_tar x -> x
  | Invalid_tar -> 
    print_endline "\n That is not a valid target, please choose again: \n"; 
    select_enemy team

(** [is_team_dead act_team] checks if [act_team] is all dead.
    Requires: act_team must be a team type that passed through the get_active
    function *)
let is_team_dead act_team = act_team = []

(** [check_winner act_team inte] raises [Winner inte] exception if act_team 
    is dead. 
    function *)
let check_winner act_team inte= 
  if is_team_dead act_team then raise (Winner inte) else ()

(** PH function. Will be used to allow usage of items *)
let use_item team = ()

(** [is_move input move] checks if the string [input] is the same
    as [move]'s name, it disregards capitalization *)
let is_move input move = 
  let move1 = Character.get_move_name move |>  String.lowercase_ascii in 
  let move2 = (input |>  String.lowercase_ascii) in 
  move1 = move2


(** Should only be using list of off-cd moves *)
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



(** [select_move move_list] prints out the possible moves to use,
    and asks for the user to input an int. the function evaluates to the 
    move they chose. Will ask and repeat if the user does not select
    a valid move *)
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

(** calculates the damage of move with atk [atk] to [target]. Takes into
    account effectiveness *)
let calc_dmg move atk target = 
  let base = Character.get_move_atk move |> float_of_int in 
  let offset = float_of_int atk *. Character.get_scale move in 
  let dmg = base +. offset in 
  dmg *. Character.get_effectiveness move target.char_c

let vary_dmg attacker move target = 
  let atk = attacker.atk in 
  let fix_dmg = calc_dmg move atk target in 
  let vary_dmg = vary fix_dmg dmg_variation |> int_of_float in 
  do_dmg target vary_dmg


(** [use_move opp_team c] asks the user to attack with character c.*)
let use_move n opp_team c = 
  let act_enemy = get_active opp_team in 
  check_winner act_enemy n;
  blue_char_name c;
  print_string " is attacking: \n\n";
  let act_moves = char_moves_off_cd c in 
  if List.length act_moves = 0 then 
    print_endline "All this character's moves are on cooldown, can't attack!"
  else begin
    let move = select_move act_moves in 
    let target = select_enemy act_enemy in 
    reassign_char_cd c move;
    vary_dmg c move target
  end

(** [get_team n t] will return tuple of [(current_team, opposing team)]. 
    [current_team] is denotes that it is this team's turn to attack. 
    [opposing team] is the team being attacked *)
let get_team n t = 
  match n with
  | 1 -> (t.team1, t.team2)
  | 2 -> (t.team2, t.team1)
  | _ -> failwith "None"

(** [team_n_turn n t] carries out the functions of 1 turn. It allows team [n]
    one item usage, and then each character on the team will choose a move, and
    then choose a target to use it on.*)
let team_n_turn n t : unit = 
  let (fri_team, enemy_team) = get_team n t in
  update_cd_team fri_team;
  let act_friendly = get_active fri_team in 
  check_winner (get_active enemy_team) n;
  Printf.printf "\n It is team %d's turn to start!\n\n" n;
  use_item fri_team; 
  List.iter (use_move n enemy_team) act_friendly


(** [start_t t] starts combat from the representation type t. the while loop
    only ends when a team has won *)
let start_t t = 
  try begin
    while (true) do
      team_n_turn 1 t;
      team_n_turn 2 t;
    done
  end
  with Winner inte -> Printf.printf "Congratulation, team %d is the Winner!\n\n" inte;
    t.winner <- inte

let winner t = t.winner


(** [print_char_select t k] prints a message, to signify that the id k will 
      choose a character with that id in t.
*)
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

(** [player_pick k lst] allows player to pick [k] characters
    requires k > 0*)
let player_pick k lst = player_pick_helper k lst []

(** [random_pick_char t k] allows users to pick k character from max_id choices*)
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


(** random_clst randomly chooses k characters from the list of all possible
    characters, and then returns a Character.c list containing their choices*)
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

(** [mult_start] initiates combat, with character/moves contained in [t]*)
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
  Printf.printf "Character %s is attacking: " c.char_name;
  print_flush ();
  Unix.sleep delay_time;
  let act_moves = char_moves_off_cd c in 
  if List.length act_moves = 0 then
    print_endline "This character has no available moves, can't attack!"
  else begin
    let smartness = smartness_of_c c in 
    let (move, target) = enemy_select_action c act_enemy smartness
    in
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
  if n = 1 then begin
    ANSITerminal.(print_string [red] "It is your turn to attack: \n");
    use_item fri_team; 
    List.iter (use_move n enemy_team) act_friendly
  end
  else begin
    ANSITerminal.( print_string [red] "The enemy is attacking! \n");
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
    t.winner <- inte

let start_sing clst1 clst2 items = 
  let init = init clst1 clst2 items in 
  start_t_sing init;
  ANSITerminal.(print_string 
                  [red] "Congratulation, you defeated the enemy!\n");