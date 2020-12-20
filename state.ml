type level = int

type xp = int

type gold = int

(** AF: {chars = [(c1,1);(c2,2)]; current_room = r1; visited = [r2;r3]; map = m;
    gold = g; inventory = [item1;item2]} is the state of the game containing 
    characters c1 and c2, that have xp 1 and 2 respectively. The current room 
    the player is in is room r1, and the player has visited rooms r2 and r3. 
    The map m is currently in use, the player has g amounts of gold, and they
    have items item1 and item2.
    RI: [visited] must contain no duplicates. *)
type t = 
  {
    chars : (Character.c * xp) list;
    current_room : Adventure.room_id;
    visited : Adventure.room_id list;
    map : Adventure.t;
    gold : gold;
    inventory : Adventure.item list
  }

let rec init_clist clist exp =
  match clist with
  | [] -> []
  | h :: t -> (h, exp) :: (init_clist t exp)

let init_state a clist = 
  {
    chars = init_clist clist 0;
    current_room = Adventure.start_room a;
    visited = [Adventure.start_room a];
    map = a;
    gold = 0;
    inventory = []
  }

let rec extract_chars chars = 
  match chars with
  | [] -> []
  | (c, xp) :: t -> c :: extract_chars t

let get_chars t =
  extract_chars t.chars

let get_char n t =
  match List.nth t.chars n with
  | (c, xp) -> c

let get_xp n t =
  match List.nth t.chars n with
  | (c, xp) -> xp

let get_char_with_xp n t = 
  (get_char n t, get_xp n t)

let get_char_with_xp_lst t = 
  t.chars

let get_level n t =
  match List.nth t.chars n with
  | (c, xp) -> (-1. +. sqrt (1. +. 4. *. (float_of_int xp))) /. 2. 
               |> int_of_float

let xp_of_lvl lvl = 
  (lvl * lvl) + lvl

let get_room t =
  t.current_room

let get_visited t =
  t.visited

let get_gold t =
  t.gold

let get_inventory t =
  t.inventory

(** Precondition: [n] must be between -1 and [List.length clist], inclusive. *)
let rec add_helper c xp n clist =
  if n = -1 then clist @ [(c, xp)]
  else
    match clist with
    | [] -> [(c, xp)]
    | l  when n = 0 -> (c, xp) :: l
    | h :: t -> h :: add_helper c xp (n - 1) t

let add_char c ?(xp = 0) n t =
  let clist = t.chars in
  if n < -1 || n > List.length clist then failwith "Invalid index"
  else 
    {
      t with
      chars = add_helper c xp n clist
    }

(** Precondition: [n] must be between 0 and [List.length clist - 1], 
    inclusive. *)
let rec remove_helper n clist =
  match clist with
  | [] -> failwith "Not possible"
  | h :: t when n = 0 -> t
  | h :: t -> h :: remove_helper (n - 1) t

let remove_char n t =
  let clist = t.chars in
  if n < 0 || n >= List.length clist then failwith "Invalid index"
  else 
    {
      t with
      chars = remove_helper n clist
    }

(** Precondition: [n] must be between 0 and [List.length clist - 1], 
    inclusive. *)
let rec add_xp_helper n e clist =
  match clist with
  | [] -> failwith "Not possible"
  | (c, exp) :: t when n = 0 -> (c, exp + e) :: t
  | h :: t -> h :: add_xp_helper (n - 1) e t

let add_xp n e t =
  let clist = t.chars in
  if n < 0 || n >= List.length clist then failwith "Invalid index"
  else
    let t' =
      {
        t with
        chars = add_xp_helper n e clist
      } 
    in
    (t', get_level n t' > get_level n t)

let add_gold amt t =
  {
    t with
    gold = t.gold + amt
  }

let sub_gold amt t =
  let sub = t.gold - amt in
  if sub < 0 then failwith "Insufficient gold"
  else
    {
      t with
      gold = sub
    }

let add_inventory item t =
  {
    t with
    inventory = item :: t.inventory
  }

let rec remove_inventory_helper i = function
  | [] -> failwith "Precondition violated"
  | h :: t when i = 0 -> t
  | h :: t -> h :: remove_inventory_helper (i - 1) t

let remove_inventory i t =
  if i < 0 || i >= List.length t.inventory then failwith "Invalid index"
  else
    {
      t with
      inventory = remove_inventory_helper i t.inventory
    }

(** The type representing the result of an attempted move. *)
type result =
  | Legal of t
  | Illegal

let move t r =
  let exits = Adventure.next_rooms t.map t.current_room in
  match List.find_opt (fun x -> x = r) exits with
  | None -> Illegal
  | Some _ -> Legal 
                {t with 
                 current_room = r; 
                 visited = List.sort_uniq compare (r :: t.visited)
                }

open Adventure
open Yojson.Basic
open Yojson.Basic.Util

let char_to_json (c, xp) =
  `Assoc [("id", `Int (Character.get_char_id c)); ("xp", `Int xp)]

let item_json i =
  match i with
  | FlatHp (n, x) -> 
    `Assoc [
      ("name", `String n);
      ("item", `String "FlatHp");
      ("eff", `Int x)
    ]
  | PercentHp (n, f) -> 
    `Assoc 
      [
        ("name", `String n);
        ("item", `String "PercentHp");
        ("eff", `Float f)
      ]
  | RevivalItem n -> 
    `Assoc 
      [
        ("name", `String n);
        ("item", `String "RevivalItem");
      ] 

let to_json t =
  `Assoc
    [
      ("chars", `List (List.map char_to_json t.chars));
      ("current_room", `Int t.current_room);
      ("visited", `List (List.map (fun x -> `Int x) t.visited));
      ("gold", `Int t.gold);
      ("inventory", `List (List.map item_json t.inventory))
    ]

let save t path =
  t |> to_json |> to_file path

let char_from_json c o =
  (o |> member "id" |> to_int |> Character.get_char c |> Option.get, 
   o |> member "xp" |> to_int)

let load adv c path =
  let json = from_file path in
  {
    chars = json |> member "chars" |> to_list |> List.map (char_from_json c);
    current_room = json |> member "current_room" |> to_int;
    visited = json |> member "visited" |> to_list |> List.map to_int;
    map = adv;
    gold = json |> member "gold" |> to_int;
    inventory = json |> member "inventory" |> to_list |> List.map item_matcher
  }

let load_inventory items t = 
  {t with inventory = items}