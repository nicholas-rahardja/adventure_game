open OUnit2
open Character

let j1 = Yojson.Basic.from_file "json/charmovetest.json"
let t1 = from_json j1

let getextract_char id = match
    (get_char t1 id) with
| None -> failwith "character not found"
| Some x -> x

let michael = getextract_char 1
let gries = getextract_char 2 
let xenon = getextract_char 7
let mermaid = getextract_char 12
let fairy = getextract_char 5
let imp = getextract_char 3


let test_team1 = [michael; imp; mermaid]
let test_team2 = [fairy; fairy; fairy]


let sing_player () = SinglePlayerCombat.start test_team1 test_team2
let mult_player () = Combat.mult_start t1

let rec go () = 
  print_endline "Type 1 for multiplayer,\n or 2 for single player (single 
  player only testing for now";
  let choice = read_int () in 
  if choice = 1 then mult_player ()
  else if choice = 2 then sing_player ()
  else go ()



let go_t = 
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Combat-based Game System!\n");
  go ()