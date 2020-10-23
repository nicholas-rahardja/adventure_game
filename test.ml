open OUnit2
open Character

(* Call [from_file f] here to turn [f] into a value of type [Yojson.Basic.t]. *)
(* let example1 = from_file "directory/file.json" *)

let str x = 
  x

let string_of_element = function
  | Normal -> "Normal"
  | Water -> "Water"
  | Fire -> "Fire"
  | Grass -> "Grass"

(* Copied from A2 test suite *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

let char_str_test name f input expected =
  name >:: (fun _ -> assert_equal expected (f input) ~printer:str)

let char_int_test name f input expected =
  name >:: (fun _ -> assert_equal expected (f input) ~printer:string_of_int)

let char_element_test name f input expected =
  name >:: (fun _ -> assert_equal expected (f input) ~printer:string_of_element)

(* WARNING: the two functions below do not work with element tests. *)
let from_json_chars_test name json f expected =
  name >:: (fun _ -> assert_equal expected 
               ((from_json json).all_chars |> List.map f)
               ~cmp:cmp_set_like_lists)

let from_json_moves_test name json f expected =
  name >:: (fun _ -> assert_equal expected 
               ((from_json json).all_moves |> List.map f)
               ~cmp:cmp_set_like_lists)

let char_tests = [

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

let move_tests = [

]

let suite =
  "test suite"  >::: List.flatten [
    char_tests;
    move_tests;
  ]

let _ = run_test_tt_main suite

