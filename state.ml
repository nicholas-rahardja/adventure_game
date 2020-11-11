type level = int

(** Invariant: [visited] must contain no duplicates. *)
type t = 
  {
    chars : Character.c list;
    lvl : level;
    current_room : Adventure.room_id;
    visited : Adventure.room_id list;
    map : Adventure.t
  }

let init_state a clist = 
  {
    chars = clist;
    lvl = 1;
    current_room = Adventure.start_room a;
    visited = [Adventure.start_room a];
    map = a
  }

let get_chars t =
  t.chars

let get_level t =
  t.lvl

let get_room t =
  t.current_room

let get_visited t =
  t.visited

let cmp_chars c1 c2 =
  let get_id = Character.get_char_id in
  get_id c1 - get_id c2

let set_chars t c =
  {t with chars = c}

let set_level t l =
  {t with lvl = l}

let incr_level t =
  set_level t (t.lvl + 1)

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
