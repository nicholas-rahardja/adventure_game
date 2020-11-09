include Combat

(** Seconds for program to sleep before next action by enemy is used *)
let delay_time = 2

(** probability that enemy will use a smart move. Turn into a function
    later *)
let smartness = 100
(** Generates a random int between 0 and [bound]. Takes care of 0 case  *)

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

let enemy_use_move n opp_team c = 
  Unix.sleep delay_time;
  let act_enemy = get_active opp_team in 
  check_winner act_enemy n;
  Printf.printf "Character %s is attacking: \n" c.char_name;
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
  Printf.printf "\n It is team %d's turn to start!\n\n" n;
  if n = 1 then begin
    use_item fri_team; 
    List.iter (use_move n enemy_team) act_friendly
  end
  else begin
    print_endline "The enemy is attacking \n\n";
    List.iter (enemy_use_move n enemy_team) act_friendly
  end

let start_t t = 
  try begin
    while (true) do
      execute_turn 1 t;
      execute_turn 2 t;
    done
  end
  with Winner inte -> Printf.printf "Congratulation, team %d is the Winner!\n" inte;
    t.winner <- inte

let start clst1 clst2 = 
  let init = init clst1 clst2 in 
  start_t init
