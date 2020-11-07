open Yojson.Basic.Util

type room_id = int 

type exit_name = string

exception UnknownRoom of room_id

exception UnknownExit of exit_name

type exit ={
  name:exit_name;
  room_id:int;
}

type room ={
  id : room_id;
  name : string;
  initial_message : string;
  enemy_ids : int list;
  shop : string list;
  rewards : string list;
  difficulty : int; 
  exits : exit list 
}

type t ={
  rooms : room list;
  start_room: room_id;
}

let exit_of_json json = {
  name = json |> member "name" |> to_string;
  room_id = json |> member "room id" |> to_int;
}

let room_of_json json ={
  id = json |> member "id" |> to_int;
  name = json |> member "name" |> to_string;
  initial_message = json |>  member "initial message" |> to_string;
  enemy_ids = json |> member "enemy ids" |> to_list |> List.map to_int;
  shop = json |> member "shop" |> to_list |> List.map to_string;
  rewards = json |> member "rewards" |> to_list |> List.map to_string;
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

let message a r = 
  (get_room a r).initial_message

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



