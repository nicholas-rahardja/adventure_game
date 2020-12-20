open Yojson.Basic.Util

type room_id = int 

type exit_name = string

exception UnknownRoom of room_id

exception UnknownExit of exit_name

type exit ={
  name:exit_name;
  room_id:int;
}

type item =
  | FlatHp of string * int 
  | PercentHp of string * float 
  | RevivalItem of string

type item_wrapper =
  {
    item : item;
    price : int
  }

type room ={
  id : room_id;
  name : string;
  initial_message : string;
  enemy_ids : int list;
  shop : item_wrapper list;
  rewards : item list;
  difficulty : int; 
  exits : exit list 
}

(**AF: {rooms = [room_1; room_2;...; room_n]; start_room = room_s} is the
   adventure with the starting room at room_ s and that has n number of rooms.
   RI: The same room can only appear once in the list.*)
type t ={
  rooms : room list;
  start_room: room_id;
}

let exit_of_json json = {
  name = json |> member "name" |> to_string;
  room_id = json |> member "room id" |> to_int;
}

let item_matcher o =
  match o |> member "item" |> to_string with
  | "FlatHp" -> 
    FlatHp (o |> member "name" |> to_string, o |> member "eff" |> to_int)
  | "PercentHp" -> 
    PercentHp (o |> member "name" |> to_string, 
               o |> member "eff" |> to_float)
  | "RevivalItem" -> RevivalItem (o |> member "name" |> to_string)
  | _ -> failwith "Invalid item"

let shop_of_json json =
  let raw_list = json |> member "shop" |> to_list in
  let process o =
    {
      item = item_matcher o;  
      price = o |> member "price" |> to_int;
    }
  in
  List.map process raw_list

let rewards_of_json json =
  let raw_list = json |> member "rewards" |> to_list in
  List.map item_matcher raw_list

let room_of_json json ={
  id = json |> member "id" |> to_int;
  name = json |> member "name" |> to_string;
  initial_message = json |>  member "initial message" |> to_string;
  enemy_ids = json |> member "enemy ids" |> to_list |> List.map to_int;
  shop = json |> shop_of_json;
  rewards = json |> rewards_of_json;
  difficulty = json |> member "difficulty" |> to_int;
  exits = json |> member "exits" |> to_list |> List.map exit_of_json;
}

let from_json json = {
  rooms = json |> member "rooms" |> to_list |> List.map room_of_json;
  start_room = json |> member "start room" |> to_int;
}

let start_room a = 
  a.start_room

let room_ids a = 
  a.rooms |> List.map (fun x -> x.id) |> List.sort_uniq compare

let get_room a r = 
  match (a.rooms |> List.filter (fun x -> x.id = r)) with 
  | [] -> raise (UnknownRoom r) 
  | h::t -> h 

let room_name a r =
  (get_room a r).name

let exits a r = 
  (get_room a r).exits |> List.map (fun (x:exit) -> x.name) 
  |> List.sort_uniq compare

let next_room a r e = 
  match ((get_room a r).exits |> List.filter (fun (x:exit) -> x.name = e)) with 
  | [] -> raise (UnknownExit e) 
  | h::t -> h.room_id 

let next_rooms a r = 
  (get_room a r).exits |> List.map (fun x -> x.room_id) 
  |> List.sort_uniq compare

let enemies a r = 
  (get_room a r).enemy_ids

let shop a r = 
  (get_room a r).shop

let rewards a r = 
  (get_room a r).rewards

let difficulty a r = 
  (get_room a r).difficulty

let item_string i =
  begin
    match i with
    | FlatHp (n, x) -> 
      n ^ ": " ^ string_of_int x ^ " " ^ n ^ " HP Medicine" 
    | PercentHp (n, f) -> 
      n ^ ": " ^ string_of_float (f *. 100.) ^ "% HP Medicine"
    | RevivalItem n -> n ^ ": " ^ "Revival Item"

  end

let item_wrapper_string i =
  item_string i.item ^ " [Price: " ^ string_of_int i.price ^ "]"