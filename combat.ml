open Printf
exception Winner of int
exception UnknownMove
exception UnknownTarget

type c = {
  char_c: Character.c;
  char_name: string;
  char_moves: Character.move list;
  mutable cur_hp: int;
  mutable buffs : unit list; 
  mutable active: bool;
}
type char_t = Character.t

type team = c list

type t = {
  team1: team;
  team2: team;
  mutable winner: int;
}


(* Constants *)
let max_char_per_team = 3
let max_char_id = 12
let max_selections = 7
let dmg_variation = 5
let health_mod = 5
let delay_time = 1
let smartness = 100


(** [vary k percent] returns a value that deviates from [k] 
    by at most +- [percent]%.
    Requires: 0 <= percent <= 100*)
(* TODO: FIX FORMULA *)
let vary (k : float) percent = 
  let rand_factor = Random.int (2 * percent + 1) - percent |> float_of_int in 
  (rand_factor /. 100. +. 1.) *. k

(** [proc k] is a random number generator. Returns true with a probability
    of k/100.
    Requires: 0 <= k <= 100 *)
let proc k =
  let rand_int = Random.int 100 + 1 in
  rand_int <= k

(** [do_dmg c dmg] inflicts dmg amount of damage on character c. *)
let do_dmg c (dmg : float) = 
  let dmg = vary dmg dmg_variation |> Float.round |> int_of_float in
  let blue_char_name () = ANSITerminal.(print_string [blue] c.char_name) in
  (blue_char_name (); 
   Printf.printf " has taken %d damage\n" dmg);
  let remaining_health = c.cur_hp - dmg in
  if remaining_health > 0 then begin
    blue_char_name ();
    Printf.printf " has %d health left\n" remaining_health;
    c.cur_hp <- remaining_health end else
    (blue_char_name ();
     Printf.printf " has fallen!\n";
     c.cur_hp <- 0;  c.active <- false);
  print_newline ()


(** [getextract_char c] extracts the c from the c option*)
let getextract_char c = match
    c with
| None -> failwith "character not found"
| Some x -> x


(** [is_active c] returns true if a character's hp is above 0. *)
let is_active c = c.active = true

(** [get_active] returns a list with only the active characters *)
let get_active team = 
  List.filter is_active team

(** [target index team]. Returns the character at index [index].
    Requires: all characters in team must be active
    index must be < team.length *)
let target index (team:team) = 
  index - 1 |> List.nth team

let counter = ref 1

(** [print_char_name inte c] prints:
    "inte target is c_name" where c_name is the char_name of c*)
let print_char_name c= 
  Printf.printf "Target %d : " !counter;
  ANSITerminal.(print_string [blue] c.char_name);
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
let ask_user = 

  read_int

(** [select_enemy] ask the user to pick an enemy to target. They will have to 
    enter an int. 0 for the 1st target, 1 for the 2nd target, 2 for the 3rd 
    target. If the pick an int that is out of range, the function will ask again
*)
let rec select_enemy team = 
  try begin
    print_newline ();
    let act_team = get_active team in
    print_targets act_team ();
    target (ask_user ()) act_team
  end
  with 
    _ -> print_endline "\n That is not a valid target, please choose again: \n"; 
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

(** *)
let is_move move_name move = 
  (Character.get_move_name move |>  String.lowercase_ascii) = 
  (move_name |>  String.lowercase_ascii)


(** [select_move move_list] prints out the possible moves to use,
    and asks for the user to input an int. the function evaluates to the 
    move they chose. Will ask and repeat if the user does not select
    a valid move *)
let rec select_move (move_list: Character.move list) =
  try begin
    let print_move (m : Character.move) = 
      Printf.printf "Move option : '%s'\n" (Character.get_move_name m) in
    List.iter print_move move_list; 
    print_endline "\nPlease enter a move name to attack your enemy:"; 
    let move_chosen = read_line () in
    List.find (is_move move_chosen) move_list
  end
  with _ -> 
    print_endline "\nthat is not a valid move, please choose agin: \n";
    select_move move_list


(** [use_move opp_team c] asks the user to attack with character c.*)
let use_move n opp_team c = 
  let act_enemy = get_active opp_team in 
  check_winner act_enemy n;
  let char_name = c.char_name in
  ANSITerminal.(print_string [blue] char_name);
  print_string " is attacking: \n\n";
  let move = select_move c.char_moves in 
  let target = select_enemy act_enemy in 
  Character.get_damage c.char_c target.char_c move  |>
  do_dmg target

(* let use_move opp_team c= 
   let move_num = select_move c.char_moves in 
   let move_id = List.nth c.char_moves move_num in 
   let move= Character.get_move_in move_id in 
   let target = select_enemy opp_team in 
   do_dmg  *)

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

(** requires k > 0*)
let player_pick k lst = player_pick_helper k lst []



(** [random_pick_char t k] allows users to pick k character from max_id choices*)
let random_pick_char t k max_id choices=
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

let mult_print n = 
  Printf.printf "Hi Player %d, it is you're turn to build your team!\n" n

let build_list k = List.init k (fun _ -> 0)




let process_char cchar = {
  char_c= cchar;
  char_name = Character.get_char_name cchar;
  char_moves = Character.get_moves cchar;
  cur_hp = Character.get_hp cchar;
  buffs= []; 
  active= true; }

let init_team clst = 
  List.map process_char clst



let init clst1 clst2 = {
  team1 = init_team clst1;
  team2 = init_team clst2;
  winner = 0
}

let start clst1 clst2 = 
  let init = init clst1 clst2 in 
  start_t init


let init_from_player t= 
  let team1 = random_clst 1 t max_char_per_team max_char_id max_selections in
  let team2 = random_clst 2 t max_char_per_team max_char_id max_selections in 
  init team1 team2 

(** [mult_start] initiates combat, with character/moves contained in [t]*)
let mult_start t = 
  init_from_player t |> start_t 




(* BEGIN SINGLE PLAYER COMBAT MODULE *)

let rand_in_lst lst =   
  let range = List.length lst  in 
  let index = Random.int range in
  List.nth lst index

let print_move (m : Character.move) = 
  Printf.printf "Move option : '%s'\n" (Character.get_move_name m)

let rand_move c = 
  List.iter print_move c.char_moves;
  rand_in_lst c.char_moves 

let rand_target team = 
  rand_in_lst team

let enemy_select_move c = 
  let x = rand_move c in 
  x

let enemy_select_target team = rand_target team


let best_dmg_move c (target : c) = 
  let move_lst = c.char_moves in 
  let dmg move = Character.get_damage c.char_c target.char_c move in
  let acc_func acc move = (move, dmg move) :: acc in
  (* Compares in reverse order *)
  let compare_tuple (_, a) (_, b) = compare b a in 
  let sorted_lst = List.fold_left acc_func [] move_lst 
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


let enemy_use_move_mult n opp_team c = 

  let act_enemy = get_active opp_team in 
  check_winner act_enemy n;
  Printf.printf "Character %s is attacking: \n" c.char_name;
  Unix.sleep delay_time;
  let (move, target) = enemy_select_action c act_enemy smartness
  (* Change later to a probability *) 
  in
  Printf.printf "%s used '%s'\n" c.char_name (Character.get_move_name move);
  Unix.sleep delay_time;
  Character.get_damage c.char_c target.char_c move  |>
  do_dmg target

let execute_turn n t : unit = 
  let (fri_team, enemy_team) = get_team n t in
  let act_friendly = get_active fri_team in 
  check_winner (get_active enemy_team) n;
  if n = 1 then begin
    ANSITerminal.( print_string [red] "It is your turn to attack: \n");
    use_item fri_team; 
    List.iter (use_move n enemy_team) act_friendly
  end
  else begin
    ANSITerminal.( print_string [red] "The enemy is attacking! \n");
    List.iter (enemy_use_move_mult n enemy_team) act_friendly
  end

let start_t_mult t = 
  try begin
    while (true) do
      execute_turn 1 t;
      execute_turn 2 t;
    done
  end
  with Winner inte -> 
    ANSITerminal.(print_string 
                    [red] "Congratulation, you defeated the enemy!\n");
    t.winner <- inte

let start_mult clst1 clst2 = 
  let init = init clst1 clst2 in 
  start_t_mult init