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
  action : bool;
}

type t = {
  round : int;
  all_players : player list;
  players : player list;
  cur_player : player_id;
  cur_bet : int;
  max_bet : int;
  deck : Deck.t;
  pots : (player_id * int) list;
  community : Card.t list;
}

exception EmptyPlayers

exception BlindFold

exception BlindCheck

exception BlindRaise

exception NotEnoughMoney

exception CannotCheck

exception NoMoreCard

exception ExceedBet of int

exception TimeToQuit

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

(** [index id list x] is the index of [id] in [list] plus [x]
    failwith: "not found" if id is not in the list *)
let rec index id list x = match list with 
    [] -> failwith "not found"
  | h::t -> if h = id then x 
    else index id t x+1 

(** [player_id_list t] return currently playing players' id list of a state [t]. 
*)
let player_id_list t = 
  List.map (fun x -> x.name) t.players

(** [next_player t] is the [player_id] of the player playing next from state [t]
    . *)
let next_player t = 
  let id_list = player_id_list t in
  let x = index t.cur_player id_list 0 in
  List.nth id_list ((x+1) mod List.length id_list)

(** [find_player t] is the [player] of the player that is making an action in 
    state [t]. *)
let find_player t = List.find (fun x -> x.name = t.cur_player) t.players

(** [action_ok t] is whether all players in [t] have already taken action*)
let action_ok t = List.for_all (fun x -> x.action) t.players

(** [reset_action pl] resets all the actions for players in [pl]*)
let reset_action pl = List.map (fun x -> {x with action = false}) pl

(**  [update_max_bet t] is the maximum current bet a player can have in a game. 
*)
let update_max_bet players = 
  let temp_player = 
    (players |> 
     List.sort (fun x y -> y.bid + y.cur_bet - x.bid - x.cur_bet) |> 
     (fun x -> List.nth x 2)) in
  (temp_player.cur_bet + temp_player.bid)






(** [conclude t] conclude a game, given the final state is [t] and start a 
    new game. *)
let conclude t = 
  let a_players = t.all_players in 
  let winners = fst (Hand.highest_hand t.community 
                       (List.map (fun x -> x.hand) t.players)) in
  let number_of_winner = List.length winners in 
  let update_player p = {
    p with 
    bid = p.bid + (t.pots)/number_of_winner;
  } in 
  let rec change_p_in_list ind ls = 
    match ls with 
    |[] -> []
    |h::t ->  if (ind = 0) then (update_player h) :: t else 
        h:: (change_p_in_list (ind-1) t) in 
  let rec updated_player ap win = match win with 
    |[] -> ap
    |h:: t -> updated_player (change_p_in_list h ap) t in 
  let clear_hand p = 
    {
      p with 
      cur_bet = 0;
      action = false;
      hand = Hand.empty;
    } in 
  let new_all_players_w_ch = 
    List.map clear_hand (updated_player a_players winners) in 
  print_endline (string_of_int (List.length new_all_players_w_ch));
  let rec update_bs_blind  acc ls = 
    match ls , acc with 
    |[] , _ -> acc
    |h:: t, [] -> update_bs_blind [{h with role = SmallBlind}] t
    |h::t, [x]  -> update_bs_blind  ({h with role = BigBlind} :: acc) t
    |h :: t, _ ->update_bs_blind ({h with role = Normal} :: acc) t
  in
  let update pl = 
    let sb =
      match pl with 
      |[] -> failwith "pl is empty"
      | a ->List.hd pl in 
    pl |> List.tl |> List.rev |> List.cons sb |> List.rev 
    |> update_bs_blind [] |> List.rev
  in 
  let new_all_p_w_role = 
    (* let tail =  List.nth new_all_players_w_ch 
        ((List.length new_all_players_w_ch)-1) in 
       match tail.role with 
       | SmallBlind -> update new_all_players_w_ch  
       | _ -> *)
    update new_all_players_w_ch 
  in 
  {
    t with 
    round = 0; 
    all_players = new_all_p_w_role ;
    players = new_all_p_w_role;
    cur_bet = 0;
    deck = standard_deck;
    pots = [];
    community = [];
    max_bet = update_max_bet new_all_p_w_role;
  }





let state_checker t = 
  if (List.length t.players = 0) then raise TimeToQuit
  else if ( List.length t.players = 1) then conclude t
  else if (List.for_all (fun (x:player) -> x.cur_bet = t.cur_bet) t.players
           && action_ok t)
  then 
    if (t.round = 3 ) then conclude t
    else if (t.round <> 0 ) then
      let card = t.deck |> shuffle |> deal in
      {
        t with 
        round = t.round + 1;
        players = reset_action t.players;
        (* cur_bet = 1; *)
        deck = snd card;
        community = 
          match fst card with 
            Some x -> x:: t.community
          | None -> failwith "no card anymore";

      }
    else  let (card1, deck1) = t.deck |> shuffle |> deal in
      let (card2, deck2) = deck1 |> deal in 
      let (card3,deck3) = deck2 |> deal in 
      {
        t with 
        round = t.round + 1;

        players = reset_action t.players;
        (* cur_bet = 1; *)
        deck = deck3;
        community = 
          match  card1 , card2 , card3 with 
          |  Some x1 , Some x2, Some x3 -> x3::x2 ::x1 :: t.community
          | _ -> failwith "no card anymore";
      }
  else t

(** [update_pots t pots n] is the new pots from old pots [pots] after the 
    current player in state [t] changes their current bet to [n]. *)
let rec update_pots t pots n = 
  let rec helper t pots n acc= 
    match pots with 
    |[] -> (t.cur_player, n) :: acc
    | hd::tl -> if (fst hd = t.cur_player) 
      then acc @ ((fst hd, n)::tl) 
      else helper t tl n (hd::acc) in
  helper t pots n []

(** [bet t n] steps the current state [t] after the current player changes 
    their current bet to n. 
    Raises: NotEnoughMoney when the current player doesn't have enough money for
     changing their current bet to n. *)
let bet t n = 
  (* Printf.printf "%s%d" "the value of n is: " n ;  *)
  { t with players = List.map 
               (fun x -> if x.name = t.cur_player then 
                   {x with 
                    bid = x.bid - (n-x.cur_bet); 
                    cur_bet = n;
                    action = true;
                   }
                 else x) 
               t.players;
           all_players = List.map 
               (fun x -> if x.name = t.cur_player then 
                   {x with bid = x.bid - (n-x.cur_bet); 
                           cur_bet = n}
                 else x) 
               t.all_players;
           cur_player = next_player t;
           cur_bet = max t.cur_bet n;
           pots = update_pots t t.pots n;
  }


let fold t = if (t.round = 0 && (find_player t).role= SmallBlind 
                 && t.cur_bet = 0) 
  then raise BlindFold
  else if (t.round = 0 && (find_player t).role= BigBlind 
           && t.cur_bet = smallBlindInit) 
  then raise BlindFold
  else let new_players = 
         List.filter (fun x -> x.name <> t.cur_player) t.players in
    state_checker {
      t with 
      players = new_players;
      cur_player = next_player t;
      max_bet = update_max_bet new_players;
    }

let call t = if (t.round = 0 && (find_player t).role= SmallBlind 
                 && t.cur_bet = 0) then state_checker (bet t smallBlindInit)
  else if (t.round = 0 && (find_player t).role= BigBlind 
           && t.cur_bet < 2) then state_checker (bet t bigBlindInit)
  else let temp_player = find_player t in 
    if (temp_player.bid < t.cur_bet - temp_player.cur_bet) then 
      (bet t (temp_player.bid + temp_player.cur_bet))
    else state_checker (bet t t.cur_bet)

let check t = if (t.cur_bet <> (find_player t).cur_bet) then raise CannotCheck
  else if (t.round = 0 && (find_player t).role= SmallBlind 
           && t.cur_bet = 0) then raise BlindCheck
  else if (t.round = 0 && (find_player t).role= BigBlind 
           && t.cur_bet = smallBlindInit) then raise BlindCheck
  else state_checker (bet t t.cur_bet)

let raise x t = if (t.round = 0 && (find_player t).role= SmallBlind 
                    && t.cur_bet = 0) then raise BlindRaise
  else if (t.round = 0 && (find_player t).role= BigBlind 
           && t.cur_bet = smallBlindInit) then raise BlindRaise
  else if ((find_player t).bid < x) then raise NotEnoughMoney
  else if (t.max_bet < (x + (find_player t).cur_bet)) then 
    raise (ExceedBet (t.max_bet - (find_player t).cur_bet))
  else state_checker (bet t (t.cur_bet + x))

let exit t = 
  let new_players = List.filter (fun x -> x.name <> t.cur_player) t.players in
  state_checker {
    t with 
    all_players = List.filter (fun x -> x.name <> t.cur_player) 
        t.all_players;
    players = new_players ;
    cur_player = next_player t;
    max_bet = update_max_bet new_players;
  }

(**[initial_playerlist nl] initialize the player's states at the start of the 
   round given the names [nl] and the [deck] after dealing the cards*)
let rec initial_playerlist nl deck pl= 
  match nl with 
  |[] -> pl , deck
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
        action = false;
      } in 
    initial_playerlist t deck_2 (temp_player::pl)

let init_state str = 
  let nl = str |>
           String.split_on_char ' '
           |> List.filter ((<>) "") in 
  if (nl = []) then Stdlib.raise EmptyPlayers 
  else let (playerlist , deck) = 
         initial_playerlist nl (Deck.shuffle standard_deck) [] 
    in
    let new_players = List.rev playerlist 
    in
    {
      round = 0;
      all_players = new_players;
      players = new_players;
      cur_player = List.hd nl;
      cur_bet = 0;
      deck = deck ;
      community = [];
      pots = [];
      max_bet = initial_bid;
    }

(** [string_of_role r] is the string representation of role [r]*)
let string_of_role = function
  |BigBlind -> "BigBlind"
  |SmallBlind -> "SmallBlind"
  |Normal -> "Normal"

let string_of_cur_pot st = List.fold_left 
    (fun acc (s, i) -> acc ^ "\n" ^ (s ^ ": " ^ (string_of_int i))) "" st.pots 

let string_of st = 
  let player_decr = 
    List.fold_left (fun str pl ->
        str 
        ^ pl.name
        ^ " Role: " ^(string_of_role pl.role)
        ^ " Bids: " ^ (string_of_int pl.bid) 
        ^ " Betted: " ^ (string_of_int pl.cur_bet)
        ^ "\n"
      ) "" st.players 
  in 
  "Current players info: \n \n" ^ player_decr
  ^ "\n The current player is: " ^ st.cur_player
  ^ "\n Hand: " ^ (Hand.string_of (find_player st).hand)
  ^ "\n The current bet is: " ^ (string_of_int st.cur_bet)
  ^ "\n The current community cards are: " ^ (
    List.fold_left (fun str c ->
        str ^ " " ^ Card.string_of c )
      "" st.community
  )
  ^ "\n The amount in the pots is: " ^ (string_of_cur_pot st)
  ^ "\n we are at round :" ^ (string_of_int st.round)
