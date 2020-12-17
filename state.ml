type level = int

type xp = int

type gold = int

(** Invariant: [visited] must contain no duplicates. *)
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

let get_level n t =
  match List.nth t.chars n with
  | (c, xp) -> (-1. +. sqrt (1. +. 4. *. (float_of_int xp))) /. 2. 
               |> int_of_float

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

let swap_chars n1 n2 t =
  if n1 < 0 || n2 < 0 then failwith "Invalid index"
  else
    let c1 = get_char n1 t in
    let exp1 = get_xp n1 t in
    let c2 = get_char n2 t in
    let exp2 = get_xp n2 t in
    t
    |> remove_char n1
    |> add_char c2 ~xp:exp2 n1
    |> remove_char n2
    |> add_char c1 ~xp:exp1 n2

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
