open Hand 
open Deck
open Card

type role = 
  |BigBlind
  |SmallBlind
  |Normal

type player_id = string

type player = {
  name : player_id;
  role : role;
  bid : int;
  hand : Hand.t;
}

type pot = {
  money : int;
  participants : player_id list
}

type t = {
  players : player list;
  cur_player : player_id;
  cur_bet : int;
  deck : Deck.t;
  pots : pot list;
  community : Card.t list;
}

let cur_player t = t.cur_player

let cur_bet t = t.cur_bet

let deck t = t.deck

let pots t = t.pots

let community t = t.community

let rec index id list x = match list with 
    [] -> failwith "not found"
  | h::t -> if h = id then x 
    else index id t x+1 

let player_id_list t = 
  List.map (fun x -> x.name) t.players

let next_player t = 
  let id_list = player_id_list t in
  let x = index t.cur_player id_list 0 in
  List.nth id_list (x+1 mod List.length id_list)

let fold id t = {

}
