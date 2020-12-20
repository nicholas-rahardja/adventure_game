open Yojson.Basic.Util

type element = 
  | Water 
  | Fire 
  | Grass
  | Normal 

type move = {
  id : int; 
  name : string; 
  description : string; 
  atk : int; 
  scale : float; 
  element : element ;
  cooldown : int;
}

type c = {
  id : int; 
  name : string; 
  description : string; 
  moves : move list; 
  atk : int; 
  hp : int; 
  element : element
}

type t = {
  all_chars : (int * c) list;
  all_moves : (int * move) list
}

(* First int is the id, 2nd int is the effectiveness *)
type buff = 
  | Dot of int * int 
  | Hot of int * int
  | DmgDoneMod of int * int
  | DmgReceivedMod of int * int
  | ElementalDmgDoneMod of int * int
  | ElementalVulnerability of int * int

(* Constants *)
let atk_per_lvl = 3
let hp_per_lvl = 20

(* Character-related functions *)

let get_char t id =
  List.assoc_opt id t.all_chars

let get_char_id c =
  c.id

let get_char_name c =
  c.name

let get_char_desc c =
  c.description

let get_moves c =
  c.moves

let get_char_atk c =
  c.atk

let get_hp c =
  c.hp 

let get_char_element c =
  c.element

(* Move-related functions *)

let get_move (t:t) (move_id: int) : move option = 
  List.assoc_opt move_id t.all_moves

(** [from_json j] first generates a "premature" [t] with only the list of moves
    and no characters in order to be used in get_move later. Then, it populates 
    the list of characters and uses get_move to convert the list of move IDs in
    the JSON into actual values of type [move].*)
let from_json j =
  let char_list = member "chars" j |> to_list in
  let moves_list = member "moves" j |> to_list in
  let to_move a =
    (member "id" a |> to_int,
     {
       cooldown = member "cooldown" a |> to_int;
       id = member "id" a |> to_int;
       name = member "name" a |> to_string;
       description = member "description" a |> to_string;
       atk = member "atk" a |> to_int;
       scale = member "scale" a |> to_float;
       element = match member "element" a |> to_string with
         | "normal" -> Normal
         | "water" -> Water
         | "fire" -> Fire
         | "grass" -> Grass
         | _ -> failwith "Cannot match element in from_json"
     }) in
  let premature_t = {
    all_chars = [];
    all_moves = List.map to_move moves_list
  } in
  let to_char a = 
    (member "id" a |> to_int,
     {
       id = member "id" a |> to_int;
       name = member "name" a |> to_string;
       description = member "description" a |> to_string;
       moves = member "moves" a 
               |> to_list 
               |> List.map to_int 
               |> List.map (get_move premature_t)
               |> List.map Option.get;
       atk = member "atk" a |> to_int;
       hp = member "hp" a |> to_int;
       element = match member "element" a |> to_string with
         | "normal" -> Normal
         | "water" -> Water
         | "fire" -> Fire
         | "grass" -> Grass
         | _ -> failwith "Cannot match element in from_json"
     })
  in
  {premature_t with all_chars = List.map to_char char_list}

let get_move_name (move:move) : string = 
  move.name 

let get_move_desc (move:move) : string = 
  move.description

let get_move_atk (move:move) : int = 
  move.atk

let get_scale (move:move) : float = 
  move.scale

let get_move_element (move:move) : element = 
  move.element 

let get_move_cd move = 
  move.cooldown


(* Attack-related functions *)

let get_effectiveness (move : move) (character :c) : float = 
  match (move.element, character.element) with 
  | (Fire, Fire) 
  | (Fire,Normal) 
  | (Water,Water) 
  | (Water,Normal) 
  | (Grass, Grass) 
  | (Grass,Normal) 
  | (Normal, _ ) -> 1.0
  | (Fire,Grass) 
  | (Water, Fire) 
  | (Grass, Water) -> 1.5 
  | _ -> 0.5 

let get_damage (player:c) (enemy:c) (move:move) : float = 
  let effectiveness = get_effectiveness move enemy in 
  float_of_int(move.atk) *. effectiveness 

let get_char_atk_lvl c lvl = 
  let offset = lvl * atk_per_lvl in 
  offset + get_char_atk c

let get_char_hp_lvl c lvl = 
  let offset = lvl * hp_per_lvl in 
  offset + get_hp c