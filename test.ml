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

let suite =
  "test suite"  >::: List.flatten [
    char_tests;
    move_tests;
  ]

let _ = run_test_tt_main suite

