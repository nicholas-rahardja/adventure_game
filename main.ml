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
let test_unit_god = getextract_char 1001


let test_team1 = [michael; gries]
let test_team2 = [test_unit_god; michael]


let go unit = Combat.start test_team1 test_team2

let go_t = go ()