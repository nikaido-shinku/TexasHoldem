
open Card
open Hand


let heart_king = Card.make_card HEART 13
let heart_queen = Card.make_card HEART 12
let heart_jack = Card.make_card HEART 11
let heart_10 = Card.make_card HEART 10
let heart_9 = Card.make_card HEART 9
let heart_8 = Card.make_card HEART 8
let heart_7 = Card.make_card HEART 7
let heart_6 = Card.make_card HEART 6
let heart_5 = Card.make_card HEART 5
let heart_4 = Card.make_card HEART 4
let heart_3 = Card.make_card HEART 3
let heart_2 = Card.make_card HEART 2
let heart_A = Card.make_card HEART 1


let club_king = Card.make_card CLUB 13
let club_queen = Card.make_card CLUB 12
let club_jack = Card.make_card CLUB 11
let club_10 = Card.make_card CLUB 10
let club_9 = Card.make_card CLUB 9
let club_8 = Card.make_card CLUB 8
let club_7 = Card.make_card CLUB 7
let club_6 = Card.make_card CLUB 6
let club_5 = Card.make_card CLUB 5
let club_4 = Card.make_card CLUB 4
let club_3 = Card.make_card CLUB 3
let club_2 = Card.make_card CLUB 2
let club_A = Card.make_card CLUB 1


let diamond_king = Card.make_card DIAMOND 13
let diamond_queen = Card.make_card DIAMOND 12
let diamond_jack = Card.make_card DIAMOND 11
let diamond_10 = Card.make_card DIAMOND 10
let diamond_9 = Card.make_card DIAMOND 9
let diamond_8 = Card.make_card DIAMOND 8
let diamond_7 = Card.make_card DIAMOND 7
let diamond_6 = Card.make_card DIAMOND 6
let diamond_5 = Card.make_card DIAMOND 5
let diamond_4 = Card.make_card DIAMOND 4
let diamond_3 = Card.make_card DIAMOND 3
let diamond_2 = Card.make_card DIAMOND 2
let diamond_A = Card.make_card DIAMOND 1


let spade_king = Card.make_card SPADE 13
let spade_queen = Card.make_card SPADE 12
let spade_jack = Card.make_card SPADE 11
let spade_10 = Card.make_card SPADE 10
let spade_9 = Card.make_card SPADE 9
let spade_8 = Card.make_card SPADE 8
let spade_7 = Card.make_card SPADE 7
let spade_6 = Card.make_card SPADE 6
let spade_5 = Card.make_card SPADE 5
let spade_4 = Card.make_card SPADE 4
let spade_3 = Card.make_card SPADE 3
let spade_2 = Card.make_card SPADE 2
let spade_A = Card.make_card SPADE 1




let random_suite i = 
  if i = 0 then HEART
  else if i = 1 then SPADE
  else if i = 2 then CLUB
  else DIAMOND

let random_suite_seed_1 = 
  Random.int 4

let random_suite_seed_2 = 
  let x = Random.int 4 in 
  if x = random_suite_seed_1 then
    (x+2) mod 4
  else x 

let random_suite_anchor_1 = 
  random_suite random_suite_seed_1

let random_suite_anchor_2 = 
  random_suite random_suite_seed_2

let random_suite_list_non_flush =
  random_suite (Random.int 4)::
  random_suite (Random.int 4)::
  random_suite (Random.int 4):: 
  [random_suite_anchor_1; random_suite_anchor_2]


let random_shift_num = 
  50


let step_rank i = 
  if i = 13 then 1
  else i+1

let rec multi_step i k = 
  if k = 0 then i
  else multi_step (step_rank i) (k-1)


let examples_royal = 
  [Hand.empty
   |> Hand.insert heart_jack
   |> Hand.insert heart_king
   |> Hand.insert heart_queen
   |> Hand.insert heart_10
   |> Hand.insert heart_A;

   Hand.empty
   |> Hand.insert spade_jack
   |> Hand.insert spade_king
   |> Hand.insert spade_queen
   |> Hand.insert spade_10
   |> Hand.insert spade_A;

   Hand.empty
   |> Hand.insert diamond_jack
   |> Hand.insert diamond_king
   |> Hand.insert diamond_queen
   |> Hand.insert diamond_10
   |> Hand.insert diamond_A;

   Hand.empty
   |> Hand.insert club_jack
   |> Hand.insert club_king
   |> Hand.insert club_queen
   |> Hand.insert club_10
   |> Hand.insert club_A;
  ]


let rec make_hands acc count f = 
  if count = 0 then acc else 
    make_hands (f :: acc) (count-1) f


let random_straight_flush = 
  let result = 
    let init =   (Random.int 13 + 1) in 
    let this_suit = random_suite (Random.int 4) in 
    Hand.empty
    |> Hand.insert (Card.make_card this_suit init)
    |> Hand.insert (Card.make_card this_suit (multi_step init 1))
    |> Hand.insert (Card.make_card this_suit (multi_step init 2))
    |> Hand.insert (Card.make_card this_suit (multi_step init 3))
    |> Hand.insert (Card.make_card this_suit (multi_step init 4))
  in 
  (* print_string (Hand.string_of result); *)
  result 



let example_straight_flush count seed =
  Random.init seed;
  make_hands [] count random_straight_flush


let random_straight = 
  let init =   (Random.int 13 + 1) in 
  let result = 
    Hand.empty
    |> Hand.insert (Card.make_card 
                      (random_suite (Random.int 4)) init)
    |> Hand.insert (Card.make_card 
                      (random_suite (Random.int 4)) (multi_step init 1))
    |> Hand.insert (Card.make_card 
                      (random_suite (Random.int 4)) (multi_step init 2))
    |> Hand.insert (Card.make_card 
                      (random_suite (Random.int 4)) (multi_step init 3))
    |> Hand.insert (Card.make_card 
                      (random_suite (Random.int 4)) (multi_step init 4))
  in 
  (* print_string (Hand.string_of result) ; *)
  result 

let example_straight count seed = 
  Random.init seed;
  make_hands [] count random_straight



let random_flush = 
  let suit = 
    random_suite (Random.int 4) in 
  let init = Hand.empty in 
  let result = 
    let random_rank_list = 
      let rec helper acc count = 
        if count = 0 then acc
        else let r =   (Random.int 13 + 1) in 
          if List.mem r acc then helper acc count 
          else helper (r::acc) (count-1) in 
      helper [] 5 in 
    List.fold_left (fun h r -> Hand.insert (Card.make_card suit r) h) 
      (init) (random_rank_list)
  in 
  (* print_string (Hand.string_of result) ; *)
  result 



let example_flush count seed= 
  Random.init seed;
  make_hands [] count random_flush


let random_four = 
  let r =  (Random.int 13 + 1) in 
  let result = 
    Hand.empty
    |> Hand.insert (Card.make_card 
                      HEART r)
    |> Hand.insert (Card.make_card 
                      DIAMOND r)
    |> Hand.insert (Card.make_card 
                      SPADE r)
    |> Hand.insert (Card.make_card 
                      CLUB r)
  in 
  let new_r = (r +  random_shift_num) mod 13 + 1 in 
  Hand.insert (Card.make_card 
                 (random_suite (Random.int 4)) new_r) result 



let example_four count seed = 
  Random.init seed;
  make_hands [] count random_four


let random_full = 
  let r1 =  (Random.int 13 + 1) in 
  let r2 = (r1 +  random_shift_num) mod 13 + 1 in 
  let rec helper acc count = 
    if count = 0 then acc
    else let r =   (Random.int 4) in 
      if List.mem r acc then helper acc count 
      else helper (r::acc) (count-1) in 
  let suit_seed_1 = 
    helper [] 3 in 
  let suit_seed_2 = 
    helper [] 2 in 
  let clist =
    List.map (fun s -> Card.make_card (random_suite s) r1) suit_seed_1
    @ List.map (fun s -> Card.make_card (random_suite s) r2) suit_seed_2 in 
  List.fold_left (fun h c -> Hand.insert c h) 
    (Hand.empty) (clist)




let example_full count seed = 
  Random.init seed;
  make_hands [] count random_full


let random_three = 
  let rec helper acc count = 
    if count = 0 then acc
    else let r =   (Random.int 13) + 1 in 
      if List.mem r acc then helper acc count 
      else helper (r::acc) (count-1) in 
  let r =(Random.int 13) + 1 in 
  let rlist = 
    List.filter ((<>)r) (helper [r] 2) in

  let clist = 
    List.map (fun r -> Card.make_card (random_suite (Random.int 4)) r) rlist
    @   [Card.make_card HEART r; Card.make_card SPADE r; Card.make_card CLUB  r;
        ]in 
  List.fold_left (fun h c -> Hand.insert c h) 
    (Hand.empty) (clist)

let example_three count seed = 
  Random.init seed;
  make_hands [] count random_three



let random_two_pair = 
  let r1 =(Random.int 13) + 1 in
  let r2 =  (r1 + random_shift_num) mod 13 + 1 in 
  let r3 = (r1 + random_shift_num - 2) mod 13 + 1 in
  let clist = [Card.make_card HEART r1; 
               Card.make_card SPADE r1; 
               Card.make_card CLUB  r2;
               Card.make_card HEART r2;
               Card.make_card CLUB r3
              ]in 
  List.fold_left (fun h c -> Hand.insert c h) 
    (Hand.empty) (clist)

let example_two_pair count seed = 
  Random.init seed;
  make_hands [] count random_two_pair




let random_pair = 
  let rec helper acc count = 
    if count = 0 then acc
    else let r =   (Random.int 13) + 1 in 
      if List.mem r acc then helper acc count 
      else helper (r::acc) (count-1) in 
  let r =(Random.int 13) + 1 in 
  let rlist = 
    List.filter ((<>)r) (helper [r] 3) in

  let clist = 
    List.map (fun r -> Card.make_card (random_suite (Random.int 4)) r) rlist
    @   [Card.make_card HEART r; Card.make_card SPADE r;
        ]in 
  List.fold_left (fun h c -> Hand.insert c h) 
    (Hand.empty) (clist)


let example_pair count seed = 
  Random.init seed;
  make_hands [] count random_pair
