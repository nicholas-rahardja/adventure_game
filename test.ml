open OUnit2
open Yojson.Basic
open Character


(* Load JSON files here for testing.
   Call [from_file f] here to turn [f] into a value of type [Yojson.Basic.t]. *)
let j1 = from_file "json/charmove.json"
let t1 = from_json j1

(* Copied from A2 test suite *)
(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(* Other comparison and printer functions *)
let cmp_unordered_lists lst1 lst2 =
  List.sort compare lst1 = List.sort compare lst2

let str x = 
  x

let string_of_element = function
  | Normal -> "Normal"
  | Water -> "Water"
  | Fire -> "Fire"
  | Grass -> "Grass"

let chars_test name json f p expected =
  name >:: (fun _ -> assert_equal expected 
               ((from_json json).all_chars |> List.map (fun (_, y) -> f y))
               ~cmp:cmp_unordered_lists ~printer:p)

let chars_str_test name json f expected =
  name >:: (fun _ -> assert_equal expected 
               ((from_json json).all_chars |> List.map (fun (_, y) -> f y))
               ~cmp:cmp_unordered_lists ~printer:(pp_list pp_string))

let char_tests = [
  chars_test "char id test" j1 get_char_id (pp_list string_of_int)
    [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 1001];
  chars_str_test "char name test" j1 get_char_name
    ["Brave Warrior Clarkson"; "Wise Sage Gries"; "Nether Imp"; "Harpy"; 
     "Forest Fairy"; "Mountain Thug"; "Holy Knight Xenon"; "Paladin"; 
     "Dark priestess"; "Alpha Wolf"; "Wolf"; "Mermaid"; "test char"];
  chars_str_test "char desc test" j1 get_char_desc
    ["PH"; "PH"; "PH"; "PH"; "PH"; "PH"; "PH"; "PH"; "PH"; "PH"; "PH"; "PH"; 
     "test character. DO NOT USE IN REAL GAME"];
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
  chars_test "char element test" j1 get_char_element (pp_list string_of_element) 
    [Normal; Normal; Fire; Normal; Grass; Normal; Normal; Normal; Normal; 
     Normal; Normal; Normal; Normal];
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
let move_16 = Option.get (get_move t1 16) 
let move_11 = Option.get (get_move t1 11)
let c_3 = Option.get (get_char t1 3) 
let c_12 = Option.get (get_char t1 12)

let move_tests = [
  move_getter_test_helper "get name move 1" get_move_name move_1 "Rising Slash";
  move_getter_test_helper "get name move 16" get_move_name move_16 
    "magic missiles";
  move_getter_test_helper "get description move 2" get_move_desc move_2 "PH";
  move_getter_test_helper "get attack move 2" get_move_atk move_2 10;
  move_getter_test_helper "get scale of move 1" get_scale move_1 2.0;
  move_getter_test_helper "get move element of move 16" get_move_element move_16
    Normal;
  get_effectiveness_tests_helper 
    "effectivennes of normal move vs fire character" 1.0 move_2 c_3;
  get_effectiveness_tests_helper "grass move vs fire character" 0.5 move_11 c_3;

  get_damange_helper "grass move vs fire character with base attack 10"
    5. c_12 c_3 move_11;
]

open Adventure
let test_adventure = from_json (from_file "adventure_test.json")
let room_ids_test_helper name a expected = name >::(fun _ -> 
    assert_equal expected (room_ids a) ~printer:(pp_list string_of_int) ~cmp:cmp_set_like_lists)  
let start_room_test_helper name a expected = name >::(fun _ -> 
    assert_equal expected (start_room a)) 
let message_test_helper name a r expected = name >:: (fun _ -> 
    assert_equal expected (message a r))
let exits_test_helper name a r expected = name >:: (fun _ -> 
    assert_equal expected (exits a r))
let next_room_test_helper name a r e expected = name >:: (fun _ -> 
    assert_equal expected (next_room a r e))
let next_rooms_test_helper name a r expected = name >:: (fun _ -> 
    assert_equal expected (next_rooms a r))
let enemies_test_helper name a r expected = name >::(fun _ -> 
    assert_equal expected (enemies a r)) 
let shop_test_helper name a r expected = name >::(fun _ -> 
    assert_equal expected (shop a r)) 
let rewards_test_helper name a r expected = name >::(fun _ -> 
    assert_equal expected (rewards a r)) 
let difficulty_test_helper name a r expected = name >::(fun _ -> 
    assert_equal expected (difficulty a r)) 

let map_test = [
  start_room_test_helper "start room of adventure test" test_adventure 1;
  room_ids_test_helper "room ids in adventure test" test_adventure [1;2;3;4;5;6;7;8];
  message_test_helper "message in room 1 of adventure test" test_adventure 1 
    "You are at Home Base";
  message_test_helper "message in room 3 of adventure test" test_adventure 3 
    "You are at Somerset Town";
  exits_test_helper "exits from room 2 in adventure test" test_adventure 2 
    ["Somerset Town"];
  next_room_test_helper "next from room 3 to room 4" test_adventure 3 
    "Black Forest" 4;
  next_rooms_test_helper "next rooms from room 2" test_adventure 2 [3];
  enemies_test_helper "enemy in room 1" test_adventure 1 [];
  enemies_test_helper "enemy in room 2" test_adventure 2 [1];
  shop_test_helper "shop in room 2" test_adventure 2 ["item 1"; "item 2"];
  rewards_test_helper "reward in room 3" test_adventure 3 ["reward 2"; "reward 3"];
  difficulty_test_helper "diffculty of room 2" test_adventure 2 1;
]

(* START: State tests *)

(* Helper functions *)
let get_char_list l =
  List.filter_map (fun x -> get_char t1 x) l

let state_chars_test name input expected_output =
  name >:: (fun _ -> assert_equal expected_output (input |> State.get_chars) 
               ~printer:(pp_list (fun x -> get_char_name x)))

let state_int_test name input f expected_output =
  name >:: (fun _ -> assert_equal expected_output (input |> f)
               ~printer:string_of_int)

let state_visited_test name input expected_output =
  name >:: (fun _ -> assert_equal expected_output (input |> State.get_visited) 
               ~printer:(pp_list string_of_int) ~cmp:cmp_set_like_lists)

let state_move_test name input room new_visited =
  match State.move input room with
  | Illegal -> name >:: (fun _ -> assert_equal [] new_visited)
  | Legal t' -> state_visited_test name t' new_visited

(* States and character lists *)
let cl1 = get_char_list [1; 2; 3]
let cl2 = get_char_list [4; 1; 2; 3]
let s1 = State.init_state test_adventure cl1
let s2 = State.set_chars s1 cl2
let s3 = 
  let open State in 
  let mv r s = 
    match move s r with
    | Legal t' -> t'
    | Illegal -> failwith "Illegal move" in
  s2
  |> mv 2
  |> mv 3
  |> mv 4
  |> mv 3


let state_tests = [
  state_chars_test "get_chars test" s1 cl1;
  state_int_test "get_level test" s1 State.get_level 1;
  state_int_test "get_room test" s1 State.get_room 1;
  state_visited_test "get_visited test" s1 [1];
  state_visited_test "visited no dups test" s3 [1; 2; 3; 4];
  state_chars_test "set new char list for s1" s2 (get_char_list [4; 1; 2; 3]);
  state_int_test "set_level test" (State.set_level s1 5) State.get_level 5;
  state_int_test  "incr_level test" (State.incr_level s1) State.get_level 2;
  state_move_test "legal move" s1 2 [2; 1];
  state_move_test "illegal move" s1 4 [];
]

let suite =
  "test suite"  >::: List.flatten [
    char_tests;
    move_tests;
    map_test;
    state_tests;
  ]

let _ = run_test_tt_main suite

