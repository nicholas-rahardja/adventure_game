open OUnit2
open Character
open Combat
open SinglePlayerCombat

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


let go = SinglePlayerCombat.start test_team1 test_team2