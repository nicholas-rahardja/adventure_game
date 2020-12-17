open OUnit2
open Character
open Combat

let j1 = Yojson.Basic.from_file "json/charmovetest.json"
let t1 = from_json j1

let getextract_char id = match
    (get_char t1 id) with
| None -> failwith "character not found"
| Some x -> x

let michael = getextract_char 1
let gries = getextract_char 2
let char3 = getextract_char 3
let char4 =getextract_char 4
let char5 =getextract_char 5
let char6 =getextract_char 6
let test_unit_god = getextract_char 1001


let test_team1 = [michael;char3;char5]
let test_team2 = [gries;char4;char6]

let move_1 = Option.get (get_move t1 1)

let move_2 = Option.get (get_move t1 3)

let move_3 = Option.get (get_move t1 3)

let move_cd1 = add_cd move_1 []

let update1 = update_cd_lst move_cd1

let update2 = update_cd_lst update1

let update3 = update_cd_lst update2
