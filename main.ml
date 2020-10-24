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
let xenon = getextract_char 7
let dark_priestess = getextract_char 9
let fairy = getextract_char 5
let imp = getextract_char 3


let test_team1 = [michael; xenon; fairy]
let test_team2 = [gries; dark_priestess; imp]


let go unit = Combat.start test_team1 test_team2

let go_t = go ()