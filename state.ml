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
  cur_bet : int;
  hand : Hand.t;
}

(* type pot = (player_id * int) list  *)

type t = {
  round : int;
  all_players : player list;
  players : player list;
  cur_player : player_id;
  cur_bet : int;
  deck : Deck.t;
  pots : int;
  community : Card.t list;
}

let standard_deck = 
  let rec helper k c = 
    if k = 0 then c 
    else let c' = c |> 
                  Deck.insert (Card.make_card HEART k) |> 
                  Deck.insert (Card.make_card DIAMOND k) |>
                  Deck.insert (Card.make_card CLUB k) |> 
                  Deck.insert (Card.make_card SPADE k) 
      in helper (k-1) c' 
  in helper 13 Deck.empty

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

let conclude t = {
  t with 
  round = 0; 
  players = t.all_players;
  cur_bet = 0;
  deck = standard_deck;
  pots = 0;
  community = [];
}

let state_checker t = 
  if (t.round = 5) then conclude t
  else if List.for_all (fun (x:player) -> x.cur_bet = t.cur_bet) t.players
  then 
    let card = t.deck |> shuffle |> deal in
    {
      t with 
      round = t.round + 1;
      cur_bet = 1;
      deck = snd card;
      community = 
        match fst card with 
          Some x -> x:: t.community
        | None -> failwith "no card anymore";
    }
  else t

let find_player t = List.find (fun x -> x.name = t.cur_player) t.players

exception BlindFold

exception NotEnoughMoney

exception CannotCheck

let bet t n = List.map (fun x -> if x.name = t.cur_player then 
                           (if x.bid - n < 0 then raise NotEnoughMoney 
                            else {x with bid = x.bid - n; cur_bet = n}) 
                         else x) 
    t.players


let fold t = if (t.round = 0 && (find_player t).role= SmallBlind 
                 && t.cur_bet = 0) 
  then raise BlindFold
  else state_checker {
      t with players = List.filter (fun x -> x.name <> t.cur_player) t.players;
             cur_player = next_player t;
    }

let call t = if (t.round = 0 && (find_player t).role= SmallBlind 
                 && t.cur_bet = 0) then state_checker {
    t with cur_player = next_player t;
           cur_bet = 1;
           players = bet t 1;
           pots = t.pots + 1;
  }
  else if (t.round = 0 && (find_player t).role= BigBlind 
           && t.cur_bet < 2) then state_checker {
      t with cur_player = next_player t; 
             cur_bet = 2;
             players = bet t 2;
             pots = t.pots + 2;
    }
  else state_checker {
      t with cur_player = next_player t;
             players = bet t t.cur_bet;
             pots = t.pots + t.cur_bet;
    }

let check t = if (t.cur_bet <> 0) then 
