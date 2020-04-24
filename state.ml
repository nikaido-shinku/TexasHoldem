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

exception EmptyPlayers

exception BlindFold

exception BlindCheck

exception NotEnoughMoney

exception CannotCheck

exception NoMoreCard

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


(** the default bids players start with *)
let initial_bid = 100

(** the base bet small blind has to pay at the start of a game *)
let smallBlindInit = 1

(** the base bet big blind has to pay at the start of a game *)
let bigBlindInit = 2

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

let bet t n = { t with players = List.map 
                           (fun x -> if x.name = t.cur_player then 
                               (if x.bid - (n-x.cur_bet) < 0 then 
                                  raise NotEnoughMoney 
                                else {x with bid = x.bid - (n-x.cur_bet); 
                                             cur_bet = n}) 
                             else x) 
                           t.players;
                       cur_player = next_player t;
                       cur_bet = max t.cur_bet n;
                       pots = t.pots + n - (find_player t).cur_bet;
              }


let fold t = if (t.round = 0 && (find_player t).role= SmallBlind 
                 && t.cur_bet = 0) 
  then raise BlindFold
  else state_checker {
      t with players = List.filter (fun x -> x.name <> t.cur_player) t.players;
             cur_player = next_player t;
    }

let call t = if (t.round = 0 && (find_player t).role= SmallBlind 
                 && t.cur_bet = 0) then state_checker (bet t smallBlindInit)
  else if (t.round = 0 && (find_player t).role= BigBlind 
           && t.cur_bet < 2) then state_checker (bet t bigBlindInit)
  else state_checker (bet t t.cur_bet)

let check t = if (t.cur_bet <> 0) then raise CannotCheck
  else if (t.round = 0 && (find_player t).role= SmallBlind 
           && t.cur_bet = 0) then raise BlindCheck
  else {
    t with cur_player = next_player t
  }

let raise x t = failwith "unimplemented"

let exit t = failwith "unimplemented"

(**[initial_playerlist nl] initialize the player's states at the start of the 
   round given the names [nl] *)
let rec initial_playerlist nl deck pl= 
  match nl with 
  |[] -> pl
  | h :: t -> let (card_1, deck_1) = Deck.deal deck in 
    let (card_2, deck_2) = Deck.deal deck_1 in 
    let temp_player =
      {
        name = h;
        role = (match pl with 
            | [] -> SmallBlind
            | h :: [] -> BigBlind
            | _ -> Normal);
        bid = initial_bid;
        cur_bet = 0;
        hand = Hand.empty |> Hand.insert 
                 (match card_1 with Some x -> x 
                                  | None -> Stdlib.raise NoMoreCard) |> 
               Hand.insert 
                 (match card_2 with Some x -> x 
                                  | None -> Stdlib.raise NoMoreCard);
      } in 
    initial_playerlist t deck_2 (temp_player::pl)

let init_state str = 
  let nl = str |>
           String.split_on_char ' '
           |> List.filter ((<>) "") in 
  if (nl = []) then Stdlib.raise EmptyPlayers 
  else let playerlist = initial_playerlist nl standard_deck [] in
    {
      round = 0;
      all_players = playerlist;
      players = playerlist;
      cur_player = List.hd nl ;
      cur_bet = 0;
      deck = standard_deck ;
      pots = 0;
      community = [];
    }


(** [string_of_role r] is the string representation of role [r]*)
let string_of_role = function
  |BigBlind -> "BigBlind"
  |SmallBlind -> "SmallBlind"
  |Normal -> "Normal"

let string_of_cur_pot st = string_of_int st.pots

let string_of st = 
  let player_decr = 
    List.fold_left (fun str pl ->
        str 
        ^ pl.name
        ^ " Role: " ^(string_of_role pl.role)
        ^ " Hand: " ^ (Hand.string_of pl.hand)
        ^ " Bids: " ^ (string_of_int pl.bid) 
        ^ " Betted: " ^ (string_of_int pl.cur_bet)
        ^ "\n"
      ) "" st.players 
  in 
  "Current players info: \n \n" ^ player_decr
  ^ "\n The current player is: " ^ st.cur_player
  ^ "\n The current bet is: " ^ (string_of_int st.cur_bet)
  ^ "\n The current community cards are: " ^ (
    List.fold_left (fun str c ->
        str ^ " " ^ Card.string_of c )
      "" st.community
  )
  ^ "\n The amount in the pots is: " ^ (string_of_cur_pot st)
  ^ "\n we are at round :" ^ (string_of_int st.round)
