open Card

type t =  Card.t list

let empty = []

let is_empty = 
  (=) []

let deal d = 
  match d with 
  |h :: t -> (Some h, t)
  | _ -> (None, empty)

let size = List.length

(**[array_generator k] generated a list from [k] to 52 *)
let rec array_generator k = 
  if k = 52 then [] 
  else k:: array_generator (k+1)




let shuffle d =
  let compare_help a b = 
    if fst a < fst b then -1 
    else if fst a = fst b then 0 
    else 1 
  in
  let shuffle_help d = 
    Random.self_init ();
    let nd = List.map (fun c -> (Random.bits (), c)) d in
    let sond = List.sort compare_help nd in
    List.map snd sond
  in 
  match d with 
  |[] -> (List.map Card.convert (array_generator 0)) |> shuffle_help
  |_ -> shuffle_help d





let rec mem deck card = 
  match deck with 
  | [] -> false 
  | h::t -> if h = card then true else mem t card


let rec remove deck card = 
  match deck with 
  | [] -> []
  | h::t -> if h = card then t else h::(remove t card)

let insert card deck = 
  card::deck


let to_list deck =  deck 