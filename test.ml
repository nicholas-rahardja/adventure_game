(**We implemented both black box and white box approach to our testing strategy.
   In this sense, we used TDD to write black-box test cases on our functions 
   based on the function specification before coding the function. 
   We ensured that for each function we tested the boundary and corner cases 
   based on the specification. Next, after coding the function, we used 
   white-box testing to ensure that we have covered all of the paths through 
   the function. Specifically, our tests contained tests of many functions 
   in the State, Adventure, and Character modules, since they did not depend 
   on state, and had definitive return values.

   We did not do this to all the functions we wrote, since many of our functions 
   are helpers which we have indirectly tested through testing the main 
   functions where they are used. We also avoided testing functions that only 
   print out values to the screen, as they return units, so no real test cases 
   can be made for them. For this reason as well, we avoid testing many 
   functions in the Combat and Main module, because require input from the user,
   the outputs depend heavily on state, and some outputs are even random. 
   So, we manually tested functions by running our game and making sure 
   the features implemented worked properly.

   Overall we believe that our test cases demonstrate the correctness of our 
   system, as we have implemented both black box and white box testing to 
   our main functions which should have adequately represented the domain 
   of many possible inputs to the functions, and we made sure that the 
   functions returned the values that they were expected to.
*)

open OUnit2
open Yojson.Basic
open Character
open Combat

(* Load JSON files here for testing.
   Call [from_file f] here to turn [f] into a value of type [Yojson.Basic.t]. *)
let j1 = from_file "json/charmove.json"
let t1 = from_json j1

(* Comparison and printer functions. These are REIMPLEMENTED FROM SCRATCH. 
   Despite some functions having the same names, they are NOT the same as the 
   code provided in CS 3110 assignments. *)

let pp_list pp_elt lst =
  let rec print_elt = function
    | [] -> ""
    | h :: t -> pp_elt h ^ ", " ^ print_elt t
  in
  "[" ^ print_elt lst ^ "]"

let cmp_unordered_lists lst1 lst2 =
  List.sort compare lst1 = List.sort compare lst2

let str x = 
  x

let string_of_element = function
  | Normal -> "Normal"
  | Water -> "Water"
  | Fire -> "Fire"
  | Grass -> "Grass"

(* START: Character tests *)
let chars_test name json f p expected =
  name >:: (fun _ -> assert_equal expected 
               ((from_json json).all_chars |> List.map (fun (_, y) -> f y))
               ~cmp:cmp_unordered_lists ~printer:p)

let chars_str_test name json f expected =
  name >:: (fun _ -> assert_equal expected 
               ((from_json json).all_chars |> List.map (fun (_, y) -> f y))
               ~cmp:cmp_unordered_lists ~printer:(pp_list str))

let get_char_hp_lvl_test name char level expected = 
  name >:: (fun _ -> assert_equal expected (get_char_hp_lvl char level))

let get_char_atk_lvl_test name char level expected = 
  name >:: (fun _ -> assert_equal expected (get_char_atk_lvl char level))

let c_3 = Option.get (get_char t1 3) 
let c_12 = Option.get (get_char t1 12)

let char_tests = [
  chars_test "char id test" j1 get_char_id (pp_list string_of_int)
    [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 1001];
  chars_test "char moves test" j1 get_moves (pp_list (pp_list get_move_name))
    [
      [Option.get (get_move t1 1); Option.get (get_move t1 2)];
      [Option.get (get_move t1 16); Option.get (get_move t1 17)];
      [Option.get (get_move t1 3); Option.get (get_move t1 5)];
      [Option.get (get_move t1 1); Option.get (get_move t1 2)];
      [Option.get (get_move t1 6); Option.get (get_move t1 10)];
      [Option.get (get_move t1 1); Option.get (get_move t1 2)];
      [Option.get (get_move t1 18); Option.get (get_move t1 20)];
      [Option.get (get_move t1 18); Option.get (get_move t1 19)];
      [Option.get (get_move t1 25); Option.get (get_move t1 26)];
      [Option.get (get_move t1 1); Option.get (get_move t1 2)];
      [Option.get (get_move t1 1); Option.get (get_move t1 2)];
      [Option.get (get_move t1 6); Option.get (get_move t1 15)];
      [Option.get (get_move t1 1001); Option.get (get_move t1 1002)];
    ];
  chars_test "char atk test" j1 get_char_atk (pp_list string_of_int) 
    [10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 1000];
  chars_test "char hp test" j1 get_hp (pp_list string_of_int) 
    [250; 250; 250; 250; 250; 250; 250; 250; 250; 250; 250; 250; 1000];
  get_char_hp_lvl_test "hp of character 3 at level 5" c_3 5 350; 
  get_char_hp_lvl_test "hp of character 12 at level 2" c_12 2 290; 
  get_char_atk_lvl_test "atk of character 3 at level 1" c_3 1 13; 
  get_char_atk_lvl_test "atk of character 12 at level 8" c_3 8 34;
]

(** Testing the move functions *)
let move_getter_test_helper name f input expected = 
  name >:: (fun _ -> assert_equal expected (f input))

let get_effectiveness_tests_helper name expected move character = 
  name >:: (fun _ -> assert_equal expected (get_effectiveness move character) 
               ~printer:string_of_float)

let get_damange_helper name expected player enemy move = 
  name >:: (fun _ -> assert_equal expected (get_damage player enemy move) 
               ~printer:string_of_float)

let move_1 = Option.get (get_move t1 1) 
let move_2 = Option.get (get_move t1 2) 
let move_3 = Option.get (get_move t1 3) 
let move_5 = Option.get (get_move t1 5) 
let move_10 = Option.get (get_move t1 10)
let move_11 = Option.get (get_move t1 11)
let move_15 = Option.get (get_move t1 15)
let move_16 = Option.get (get_move t1 16) 
let move_17 = Option.get (get_move t1 17) 
let move_1001 = Option.get (get_move t1 1001)
let move_1002 = Option.get (get_move t1 1002)

let move_tests = [
  move_getter_test_helper "get name move 1" get_move_name move_1 "Rising Slash";
  move_getter_test_helper "get name move 16" get_move_name move_16 
    "magic missiles";
  move_getter_test_helper "get attack move 2" get_move_atk move_2 10;
  move_getter_test_helper "get scale of move 1" get_scale move_1 2.0;
  get_effectiveness_tests_helper 
    "effectivennes of normal move vs fire character" 1.0 move_2 c_3;
  get_effectiveness_tests_helper "grass move vs fire character" 0.5 move_11 c_3;
  get_damange_helper "grass move vs fire character with base attack 10"
    5. c_12 c_3 move_11;
]

open Adventure
let test_adventure = from_json (from_file "./json/adventure_test.json")

let room_ids_test_helper name a expected = 
  name >::(fun _ -> assert_equal expected (room_ids a) 
              ~printer:(pp_list string_of_int) ~cmp:cmp_unordered_lists) 

let start_room_test_helper name a expected = 
  name >::(fun _ -> assert_equal expected (start_room a)) 

let exits_test_helper name a r expected = 
  name >:: (fun _ -> assert_equal expected (exits a r))

let next_room_test_helper name a r e expected = 
  name >:: (fun _ -> assert_equal expected (next_room a r e))

let next_rooms_test_helper name a r expected = 
  name >:: (fun _ -> assert_equal expected (next_rooms a r))

let enemies_test_helper name a r expected = 
  name >::(fun _ -> assert_equal expected (enemies a r)) 

let shop_test_helper name a r expected =
  name >::(fun _ -> assert_equal expected (shop a r)
              ~cmp:cmp_unordered_lists ~printer:(pp_list item_wrapper_string)) 

let rewards_test_helper name a r expected =
  name >::(fun _ -> assert_equal expected (rewards a r)
              ~cmp:cmp_unordered_lists ~printer:(pp_list item_string)) 

let difficulty_test_helper name a r expected = 
  name >::(fun _ -> assert_equal expected (difficulty a r)) 

let map_test = [
  start_room_test_helper "start room of adventure test" test_adventure 1;
  room_ids_test_helper "room ids in adventure test" test_adventure 
    [1; 2; 3; 4; 5; 6; 7; 8];
  exits_test_helper "exits from room 2 in adventure test" test_adventure 2 
    ["Somerset Town"];
  next_room_test_helper "next from room 3 to room 4" test_adventure 3 
    "Black Forest" 4;
  next_rooms_test_helper "next rooms from room 2" test_adventure 2 [3];
  enemies_test_helper "enemy in room 1" test_adventure 1 [];
  enemies_test_helper "enemy in room 2" test_adventure 2 [1];
  shop_test_helper "shop in room 2" test_adventure 2 
    [
      {
        item = FlatHp ("PH", 5);
        price = 3
      };
    ];
  shop_test_helper "shop in room 3" test_adventure 3 
    [
      {
        item = PercentHp ("PH", 0.1);
        price = 10
      };
      {
        item = RevivalItem "PH";
        price = 8
      };
    ];
  difficulty_test_helper "diffculty of room 2" test_adventure 2 1;
]

(* START: State tests *)
open State
(* Helper functions *)
let get_char_list l =
  List.filter_map (fun x -> Character.get_char t1 x) l

let state_chars_test name input expected_output =
  name >:: (fun _ -> assert_equal expected_output (input |> get_chars) 
               ~printer:(pp_list (fun x -> get_char_name x)))

let state_get_char_test name state index expected_output =
  name >:: (fun _ -> assert_equal 
               (Character.get_char t1 expected_output |> Option.get) 
               (get_char index state) 
               ~printer:get_char_name)

let state_exn_test name input expected_output =
  name >:: (fun _ -> assert_raises expected_output input)

let state_int_test name input f expected_output =
  name >:: (fun _ -> assert_equal expected_output (input |> f)
               ~printer:string_of_int)

let state_inventory_test name input expected_output =
  name >:: (fun _ -> assert_equal expected_output (get_inventory input)
               ~printer:(pp_list item_string))

let state_visited_test name input expected_output =
  name >:: (fun _ -> assert_equal expected_output (input |> get_visited) 
               ~printer:(pp_list string_of_int) ~cmp:cmp_unordered_lists)

let state_add_xp_test name state index amt new_xp up =
  let (t', r) = add_xp index amt state in
  name >:: (fun _ -> assert_equal (new_xp, up) (get_xp index t', r) 
               ~printer:(fun (x, b) -> "(" ^ string_of_int x ^ ", " 
                                       ^ string_of_bool b ^ ")"))

let state_printer c s =
  "{\n" ^ "Characters: " 
  ^ pp_list (fun x -> Character.get_char_id x |> string_of_int) (get_chars s)
  ^ "\n" ^ "Current room: " ^ string_of_int (get_room s) ^ "\n"
  ^ "Visited: " ^ pp_list (string_of_int) (get_visited s) ^ "\n"
  ^ "Gold: " ^ string_of_int (get_gold s) ^ "\n"
  ^ "Inventory: " ^ pp_list (item_string) (get_inventory s)
  ^ "\n}"

let state_move_test name input room new_visited =
  match State.move input room with
  | Illegal -> name >:: (fun _ -> assert_equal [] new_visited)
  | Legal t' -> state_visited_test name t' new_visited

let state_save_test name state adv c path =
  save state path;
  name >:: (fun _ -> assert_equal (load adv c path) state 
               ~printer:(state_printer c))

(* States and character lists *)
let cl1 = get_char_list [1; 2; 3]
let cl2 = get_char_list [4; 1; 2; 2; 3]
let s0 = init_state test_adventure [] (* Empty chars *)
let s1 = init_state test_adventure cl1
let s2 = init_state test_adventure cl2 (* Duplicate chars *)
let s3 = (* Moving around *)
  let open State in 
  let mv r s = 
    match move s r with
    | Legal t' -> t'
    | Illegal -> failwith "Illegal move" in
  s1
  |> mv 2
  |> mv 3
  |> mv 4
  |> mv 3
(* Add char with XP *)
let s4 = s2 |> add_char (Character.get_char t1 2 |> Option.get) ~xp:50 3
let s5 = s1 |> add_gold 50 (* Added gold *)
let s6 = s5 |> sub_gold 20 (* Succesful gold removal *)


let state_tests = [
  state_chars_test "get_chars test" s1 cl1;
  state_chars_test "get_chars empty" s0 [];
  state_get_char_test "get_char test" s1 1 2;
  state_get_char_test "get_char test with dups 1" s2 2 2;
  state_get_char_test "get_char test with dups 2" s2 3 2;
  state_exn_test "get_char beyond index" (fun _ -> get_char 3 s1) 
    (Failure "nth");
  state_exn_test "get_char negative" (fun _ -> get_char ~-1 s1) 
    (Invalid_argument "List.nth");
  state_int_test "get_xp 0 test" s1 (get_xp 0) 0;
  state_int_test "get_xp test" s4 (get_xp 3) 50;
  state_exn_test "get_xp beyond index" (fun _ -> get_xp 3 s1) 
    (Failure "nth");
  state_exn_test "get_xp negative" (fun _ -> get_xp ~-1 s1) 
    (Invalid_argument "List.nth");
  state_int_test "get_level 0 test" s1 (get_level 0) 0;
  state_int_test "get_level test" s4 (get_level 3) 6;
  state_exn_test "get_level beyond index" (fun _ -> get_level 3 s1) 
    (Failure "nth");
  state_exn_test "get_level negative" (fun _ -> get_level ~-1 s1) 
    (Invalid_argument "List.nth");
  state_int_test "get_room test" s1 State.get_room 1;
  state_visited_test "get_visited test" s1 [1];
  state_visited_test "visited no dups test" s3 [1; 2; 3; 4];
  state_int_test "get_gold 0 test" s1 get_gold 0;
  state_int_test "get_gold, add_gold test" s5 get_gold 50;
  state_inventory_test "get_inventory 0 test" s1 [];
  state_chars_test "add_chars prepend" 
    (add_char (Character.get_char t1 11 |> Option.get) 0 s2)
    (get_char_list [11; 4; 1; 2; 2; 3]);
  state_chars_test "add_chars append using -1" 
    (add_char (Character.get_char t1 10 |> Option.get) ~-1 s2)
    (get_char_list [4; 1; 2; 2; 3; 10]);
  state_chars_test "add_chars append" 
    (add_char (Character.get_char t1 10 |> Option.get) 5 s2) 
    (get_char_list [4; 1; 2; 2; 3; 10]);
  state_chars_test "add_char middle" 
    (add_char (Character.get_char t1 9 |> Option.get) 3 s2) 
    (get_char_list [4; 1; 2; 9; 2; 3]);
  state_chars_test "add_char empty" 
    (add_char (Character.get_char t1 8 |> Option.get) 0 s0) (get_char_list [8]);
  state_chars_test "add_char empty using -1" 
    (add_char (Character.get_char t1 8 |> Option.get) ~-1 s0) 
    (get_char_list [8]);
  state_int_test "add_char with nonzero xp in char list with dups" s4 (get_xp 3)
    50;
  state_exn_test "add_char negative" 
    (fun _ -> add_char (Character.get_char t1 1 |> Option.get) ~-2 s1) 
    (Failure "Invalid index");
  state_exn_test "add_char beyond index" 
    (fun _ -> add_char (Character.get_char t1 1 |> Option.get) 4 s1) 
    (Failure "Invalid index");
  state_chars_test "remove_chars on first" (remove_char 0 s2)
    (get_char_list [1; 2; 2; 3]);
  state_chars_test "remove_chars on last" (remove_char 4 s2)
    (get_char_list [4; 1; 2; 2]);
  state_int_test "remove_chars in middle with dups" 
    (remove_char 4 s4) (get_xp 3) 50;
  state_exn_test "remove_char negative" (fun _ -> remove_char ~-1 s1) 
    (Failure "Invalid index");
  state_exn_test "remove_char beyond index" (fun _ -> remove_char 3 s1) 
    (Failure "Invalid index");
  state_exn_test "remove_char empty" (fun _ -> remove_char 0 s0) 
    (Failure "Invalid index");
  state_add_xp_test "add_xp to 0 xp" s1 0 25 25 true;
  state_add_xp_test "add_xp no level up" s4 3 1 51 false;
  state_add_xp_test "add_xp with level up" s4 3 1000 1050 true;
  state_exn_test "add_xp beyond index" (fun _ -> add_xp 3 100 s1) 
    (Failure "Invalid index");
  state_exn_test "add_xp negative index" (fun _ -> add_xp ~-1 100 s1) 
    (Failure "Invalid index");
  state_int_test "sub_gold successful test" s6 get_gold 30;
  state_exn_test "sub_gold insufficient test" (fun _ -> sub_gold 60 s5) 
    (Failure "Insufficient gold");
  state_move_test "legal move" s1 2 [2; 1];
  state_move_test "illegal move" s1 4 [];
  state_save_test "save test, minimal" s0 test_adventure t1 "./s0.json";
  state_save_test "save test, normal" s1 test_adventure t1 "./s1.json";
  state_save_test "save test, with dups" s2 test_adventure t1 "./s2.json";
  state_save_test "save test, with xp" s4 test_adventure t1 "./s4.json";
  state_save_test "save test, with gold" s5 test_adventure t1 "./s5.json";
]

(* STARTING combat tests *)

(* "Aqua Torrent" *)
let move_6 = Option.get (get_move t1 6) 
(* "Cyclone" *)
let move_7 = Option.get (get_move t1 7) 
(* "Icebolt" *)
let move_8 = Option.get (get_move t1 8) 

let move_set1 = [move_6; move_8; move_7]

let char_1 = Option.get (Character.get_char t1 1)
let char_2 = Option.get (Character.get_char t1 2)
let char_3 = Option.get (Character.get_char t1 3)
let char_4 = Option.get (Character.get_char t1 4)
let char_5 = Option.get (Character.get_char t1 5)
let char_6 = Option.get (Character.get_char t1 6)
let char_10 = Option.get (Character.get_char t1 10)
let char_11 = Option.get (Character.get_char t1 11)
let char_12 = Option.get (Character.get_char t1 12)
let char_1001 = Option.get (Character.get_char t1 1001)

let char_lst1 = [char_1, 10;char_2, 10; char_3, 10]
let char_lst2 = [char_3, 10;char_1001, 10]
let char_lst3 = []
let char_lst4 = [char_10, 10; char_11, 20; char_12, 20]

let empty_item_lst = []

let char_lst1_lst2_t =
  {
    team1 = 
      [
        { 
          char_c = char_1;
          char_name = "Brave Warrior Clarkson";
          char_moves = [move_1; move_2];
          cur_hp = 450;
          atk = 40;
          buffs = []; 
          active = true;
          cooldown = [];
          level = 10
        };
        {
          char_c = char_2;
          char_name = "Wise Sage Gries";
          char_moves = [move_16; move_17];
          cur_hp = 450;
          atk = 40;
          buffs = []; 
          active = true;
          cooldown = [];
          level = 10
        };
        {
          char_c = char_3;
          char_name = "Nether Imp";
          char_moves = [move_3; move_5];
          cur_hp = 450;
          atk = 40;
          buffs = []; 
          active = true;
          cooldown = [];
          level = 10
        }
      ];
    team2 =
      [
        { 
          char_c = char_3;
          char_name = "Nether Imp";
          char_moves = [move_3; move_5];
          cur_hp = 450;
          atk = 40;
          buffs = []; 
          active = true;
          cooldown = [];
          level = 10
        };
        {
          char_c = char_1001;
          char_name = "test char";
          char_moves = [move_1001; move_1002];
          cur_hp = 1200;
          atk = 1030;
          buffs = []; 
          active = true;
          cooldown = [];
          level = 10
        }
      ];
    winner = 0;
    items = empty_item_lst
  }

let char_lst3_lst4_t =
  {
    team1 = 
      [];
    team2 =
      [
        {
          char_c = char_10;
          char_name = "Alpha Wolf";
          char_moves = [move_1; move_2];
          cur_hp = 450;
          atk = 40;
          buffs = []; 
          active = true;
          cooldown = [];
          level = 10
        };
        {
          char_c = char_11;
          char_name = "Wolf";
          char_moves = [move_1; move_2];
          cur_hp = 650;
          atk = 70;
          buffs = []; 
          active = true;
          cooldown = [];
          level = 20
        };
        {
          char_c = char_12;
          char_name = "Mermaid";
          char_moves = [move_6; move_15];
          cur_hp = 650;
          atk = 70;
          buffs = []; 
          active = true;
          cooldown = [];
          level = 20
        }
      ];
    winner = 0;
    items = empty_item_lst
  }

let char_lst3_lst3_t =
  {
    team1 = [];
    team2 = [];
    winner = 0;
    items = empty_item_lst
  }

let combat_t1 = 
  let first_team = [char_1, 10; char_2 , 10; char_3 , 10] in 
  let sec_team = [char_4 , 10; char_5, 10; char_6, 10] in 
  init first_team sec_team empty_item_lst

let combat_t2 = 
  let first_team = [char_1001, 10] in 
  let sec_team = [char_4, 10; char_5, 10] in 
  init first_team sec_team empty_item_lst

(* Get a team object by using [init] like above, then extract the field *)
let team1 = combat_t1.team1 
let team2 = combat_t1.team2
let team3 = combat_t2.team1
let team4 = combat_t2.team2

(* Access each target in a team using List.nth *)
let team_target team nth = 
  List.nth team nth

let team1_first_target = team_target team1 0
let team1_first_target_hp = team1_first_target.cur_hp

let team2_first_target = team_target team2 0
let team2_first_target_hp = team2_first_target.cur_hp
let team2_first_target_half_hp = team2_first_target_hp / 2
let team2_first_target_rem_hp = 
  team2_first_target_hp - team2_first_target_half_hp

let team3_first_target = team_target team3 0
let team3_first_target_hp = team3_first_target.cur_hp

let team4_first_target = team_target team4 0
let team4_first_target_hp = team4_first_target.cur_hp

let c1 = 
  {
    char_c =  char_5;
    char_name = "Forest Fairy";
    char_moves = [move_6; move_10];
    atk = 100;
    cur_hp = 40;
    buffs = []; 
    active = true;
    cooldown = [];
    level = 10
  }

let c2 = 
  {
    char_c =  char_5;
    char_name = "Forest Fairy";
    char_moves = [move_6; move_10];
    atk = 100;
    cur_hp = 0;
    buffs = []; 
    active = false;
    cooldown = [];
    level = 10
  }

let c5_level_0 = 
  {
    char_c = char_5;
    char_name = "Forest Fairy";
    char_moves = [move_6; move_10];
    atk = 10;
    cur_hp = 250;
    buffs = []; 
    active = true;
    cooldown = [];
    level = 0;
  }

let c5_level10 = 
  {
    char_c = char_5; 
    char_name = "Forest Fairy";
    char_moves = [move_6; move_10];
    atk = 40;
    cur_hp = 450; 
    buffs = []; 
    active = true;
    cooldown = [];
    level = 10;
  }

let c12_level5 = 
  {
    char_c = c_12;
    char_name = "Mermaid";
    char_moves = [move_6; move_15];
    atk = 25; 
    cur_hp = 350; 
    buffs = []; 
    active = true;
    cooldown = [];
    level = 5;
  }

let assert_eq_help name result exp_output = 
  name >:: fun _ -> assert_equal exp_output result 

let combat_move_input_test name move_lst input exp_output = 
  let result = Combat.move_input move_lst input in
  assert_eq_help name result exp_output 

let combat_target_input_test name team input exp_output = 
  let result = Combat.target_input team input in
  assert_eq_help name result exp_output

let do_heal_test name c heal expected = 
  do_heal c heal; 
  name >:: fun _ -> 
    assert_equal expected c.cur_hp ~printer:(string_of_int)

let combat_init_test name clst1 clst2 expected = 
  let init = Combat.init clst1 clst2 empty_item_lst in
  name >:: fun _ -> assert_equal init expected 

let combat_vary_test name k percent = 
  let value = Combat.vary k percent in 
  let margin = k *. (float_of_int percent) /. 100.0 in
  if value < k then assert_eq_help name ((k -. value) <= margin) true
  else assert_eq_help name ((value -. k) <= margin) true

let combat_is_active_test name c expected = 
  assert_eq_help name (Combat.is_active c) expected

let combat_get_active_test name team expected = 
  assert_eq_help name (Combat.get_active team) expected

let is_team_dead_test name team expected = 
  let active_team = Combat.get_active team in 
  assert_eq_help name (Combat.is_team_dead active_team) expected

let get_team_test name n t expected = 
  let result = Combat.get_team n t in 
  assert_eq_help name result expected 

let is_move_test name move_name m expected = 
  let result = Combat.is_move move_name m in 
  assert_eq_help name result expected 

let load_char_test name character level expected = 
  let char_new = Combat.load_char (character, level) in
  assert_eq_help name char_new expected

let load_char_test name char_level expected = 
  assert_eq_help name (Combat.load_char char_level) expected 

let smartness_of_c_test name char expected = 
  assert_eq_help name (Combat.smartness_of_c char) expected 

let check_winner_test name team team_num expected = 
  assert_eq_help name (Combat.check_winner team team_num) expected

(* One of the teams ar*)
let combat_winner_test name t expected = 
  assert_eq_help name (t.winner) expected

(* Makes move_cd entry *)
let make_cd_entry move cd = {
  move = move;
  turns_left = cd;}

let update_cd_test name input expected = 
  let new_move_cd = Combat.update_cd input in
  assert_eq_help name new_move_cd expected

let update_cd_lst_test name input expected = 
  let new_lst = Combat.update_cd_lst input in
  assert_eq_help name new_lst expected

let update_cd_team_test name team expected = 
  Combat.update_cd_team team;
  assert_eq_help name team expected

let move_cd_1 = make_cd_entry move_1 10 
let move_cd_1_new = make_cd_entry move_1 9
let move_cd_1_8 = make_cd_entry move_1 8

let move_cd_2 = make_cd_entry move_2 0
let move_cd_2_new = make_cd_entry move_2 ~-1

let move_cd_3 = make_cd_entry move_3 ~-30
let move_cd_3_new = make_cd_entry move_3 ~-31

let move_cd_5 = make_cd_entry move_5 1
let move_cd_5_new = make_cd_entry move_5 0

let move_cd_15 = make_cd_entry move_15 0

let move_cd_1001 = make_cd_entry move_1001 1
let move_cd_1001_new = make_cd_entry move_1001 0

let move_cd_1002 = make_cd_entry move_1002 0

(* move6, 3 turns*)
let move_6_cd = make_cd_entry move_6 3
let update_move6 = update_cd move_6_cd

let cd_team_1 =
  [
    {
      char_c = char_10;
      char_name = "Alpha Wolf";
      char_moves = [move_1; move_2];
      cur_hp = 1750;
      atk = 60;
      buffs = []; 
      active = true;
      cooldown = [];
      level = 10
    };
    {
      char_c = char_12;
      char_name = "Mermaid";
      char_moves = [move_6; move_15];
      cur_hp = 2250;
      atk = 110;
      buffs = []; 
      active = true;
      cooldown = [];
      level = 10
    }
  ]

let cd_team_2 =
  [
    {
      char_c = char_10;
      char_name = "Alpha Wolf";
      char_moves = [move_1; move_2];
      cur_hp = 1750;
      atk = 60;
      buffs = []; 
      active = true;
      cooldown = [move_cd_1; move_cd_1_new];
      level = 10
    };
    {
      char_c = char_12;
      char_name = "Mermaid";
      char_moves = [move_6; move_15];
      cur_hp = 2250;
      atk = 110;
      buffs = []; 
      active = true;
      cooldown = [move_cd_15];
      level = 10
    }
  ]

let cd_team_2_new =
  [
    {
      char_c = char_10;
      char_name = "Alpha Wolf";
      char_moves = [move_1; move_2];
      cur_hp = 1750;
      atk = 60;
      buffs = []; 
      active = true;
      cooldown = [move_cd_1_new; move_cd_1_8];
      level = 10
    };
    {
      char_c = char_12;
      char_name = "Mermaid";
      char_moves = [move_6; move_15];
      cur_hp = 2250;
      atk = 110;
      buffs = []; 
      active = true;
      cooldown = [];
      level = 10
    }
  ]

let cd_team_3 =
  [
    {
      char_c = char_1001;
      char_name = "test char";
      char_moves = [move_1001; move_1002];
      cur_hp = 5500;
      atk = 1050;
      buffs = []; 
      active = true;
      cooldown = [move_cd_1001; move_cd_1002];
      level = 10
    };
    {
      char_c = char_3;
      char_name = "Nether Imp";
      char_moves = [move_3; move_5];
      cur_hp = 1750;
      atk = 60;
      buffs = []; 
      active = true;
      cooldown = [move_cd_3];
      level = 10
    };
  ]

let cd_team_3_new =
  [
    {
      char_c = char_1001;
      char_name = "test char";
      char_moves = [move_1001; move_1002];
      cur_hp = 5500;
      atk = 1050;
      buffs = []; 
      active = true;
      cooldown = [];
      level = 10
    };
    {
      char_c = char_3;
      char_name = "Nether Imp";
      char_moves = [move_3; move_5];
      cur_hp = 1750;
      atk = 60;
      buffs = []; 
      active = true;
      cooldown = [];
      level = 10
    };
  ]

let team_no_health =
  [
    {
      char_c = char_1001;
      char_name = "test char";
      char_moves = [move_1001; move_1002];
      cur_hp = 0;
      atk = 1050;
      buffs = []; 
      active = false;
      cooldown = [];
      level = 10
    };
    {
      char_c = char_3;
      char_name = "Nether Imp";
      char_moves = [move_3; move_5];
      cur_hp = 0;
      atk = 60;
      buffs = []; 
      active = false;
      cooldown = [];
      level = 10
    };
  ]

let combat_tests = [
  (* testing move_input *)
  combat_move_input_test "valid move 6" 
    move_set1 "Aqua torrent" (Valid_m move_6);
  combat_move_input_test "valid move 7" move_set1 "cyclone" (Valid_m move_7);
  combat_move_input_test "valid move 8" move_set1 "Icebolt" (Valid_m move_8);
  combat_move_input_test "invalid move, move not part of list" 
    move_set1 "Kick" (Invalid_m);
  combat_move_input_test "invalid move" move_set1 "asdfg" (Invalid_m);
  (* testing tar_input *)
  combat_target_input_test "valid target for team1" team1 "1" 
    (Valid_tar team1_first_target);
  combat_target_input_test "valid target for team2" team2 "1" 
    (Valid_tar team2_first_target);  
  combat_target_input_test "invalid target for team1" team1 "5" Invalid_tar; 
  combat_target_input_test "invalid target for team2" team2 "4" Invalid_tar;
  assert_eq_help "update cd, turns left" update_move6.turns_left 2;
  assert_eq_help "update cd, move" update_move6.move move_6;
  combat_vary_test "vary by 0 percent from 10" 10. 0;
  combat_vary_test "vary by 0 percent from 0" 0. 0;
  combat_vary_test "vary by 100 percent from 0" 0. 100;
  combat_vary_test "vary by 20 percent from 10" 10. 20;
  combat_vary_test "vary by 20 percent from 0" 0. 20;
  combat_winner_test "winner of an ongoing game is 0" combat_t1 0;
  combat_init_test "initialize a game state t for char list 1 & 2" 
    char_lst1 char_lst2 char_lst1_lst2_t;
  combat_init_test "initialize a game state t for char list 3 & 4" 
    char_lst3 char_lst4 char_lst3_lst4_t;
  combat_init_test "initialize a game state t for empty char lists 3 & 3" 
    char_lst3 char_lst3 char_lst3_lst3_t;
  update_cd_test "update move_cd_1 with positive cd = 10" 
    move_cd_1 move_cd_1_new;
  update_cd_test "update move_cd_2 with cd = 0" move_cd_2 move_cd_2_new;
  update_cd_test "update move_cd_3 with negative cd = -30" 
    move_cd_3 move_cd_3_new;
  update_cd_lst_test "update move_cd with positive cd's" 
    [move_cd_1; move_cd_1_new] [move_cd_1_new; move_cd_1_8];
  update_cd_lst_test "update empty move_cd list" [] [];
  update_cd_lst_test "update move_cd with cd = 0" [move_cd_2] [];
  update_cd_lst_test "update move_cd with cd = 1" [move_cd_5] [];
  update_cd_lst_test "update move_cd with negative cd's" 
    [move_cd_2_new; move_cd_3] [];
  update_cd_lst_test "update move_cd with combinations of cd's" 
    [move_cd_2; move_cd_1; move_cd_2_new] [move_cd_1_new];
  update_cd_team_test "emtpy team" [] [];
  update_cd_team_test "emtpy cooldown move list" cd_team_1 cd_team_1;
  update_cd_team_test "one char with positive cd and one with cd = 0" 
    cd_team_2 cd_team_2_new;
  update_cd_team_test "chars with negative cd, cd=1 and cd = 0" 
    cd_team_3 cd_team_3_new;
  combat_is_active_test "character with positive health" c1 true;
  combat_is_active_test "character with 0 health" c2 false;
  combat_get_active_test "team with all characters alive" cd_team_1 cd_team_1;
  combat_get_active_test "team with all characters dead" team_no_health [];
  combat_get_active_test "team with all characters dead" team_no_health [];
  is_team_dead_test "team with all characters dead" team_no_health true;
  is_team_dead_test "team with characters alive" cd_team_3 false;
  get_team_test "team 1 followed by team 2 in combat 1" 
    1 combat_t1 (team1, team2);
  get_team_test "team 2 followed by team 1 in combat 2" 
    2 combat_t1 (team2, team1);
  get_team_test "team 1 followed by team 2 in combat 2" 
    1 combat_t2 (team3, team4);
  get_team_test "team 2 followed by team 1 in combat 2" 
    2 combat_t2 (team4, team3);
  is_move_test "testing move with wrong name" "aqua" move_6 false; 
  is_move_test "testing empty string input" "" move_7 false; 
  is_move_test "testing move with all underscore letters" 
    "aqua torrent" move_6 true; 
  is_move_test "testing move 8 with exact name" "Icebolt" move_8 true;
  load_char_test "load character 5 at level 0" (char_5, 0) c5_level_0;  
  load_char_test "load character 5 at level 10" (char_5, 10) c5_level10;
  load_char_test "load character 12 at level 5" (char_12,5) c12_level5;
  smartness_of_c_test "smartness of character 5 at level 0" c5_level_0 4;
  smartness_of_c_test "smartness of character 12 at level 5" c12_level5 10;
  check_winner_test "non empty team" team1 1 ();
]

let suite =
  "test suite"  >::: List.flatten [
    char_tests;
    move_tests;
    map_test;
    state_tests;
    combat_tests;
  ]

let _ = run_test_tt_main suite

