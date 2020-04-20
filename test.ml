open OUnit2
open Card


let spade_9 = Card.make_card SPADE 9
let heart_king = Card.make_card HEART 13
let club_queen = Card.make_card CLUB 12
let diamond_9 = Card.make_card DIAMOND 9

(** [make_card_comparison name card1 card2 expected_output]
    constructs an OUnit test that examines the quality of [expected_output]
    with [compare card1 card2]*)
let make_card_comparison
    (name : string)
    (card1 : Card.t)
    (card2 : Card.t)
    (expected_output : int)= 
  name >:: (fun _ ->
      assert_equal expected_output (Card.compare card1 card2))


let test_card = [
  make_card_comparison "test for card comparison between king and queen"
    heart_king
    club_queen
    1;
  make_card_comparison "test for card comparison between queen and number"
    club_queen
    spade_9
    1;
  make_card_comparison "test for card comparison between same number"
    spade_9
    spade_9
    0;
  make_card_comparison "test for card comparison between 
  same number different suite"
    spade_9
    diamond_9
    0;
]

let tests = List.flatten [test_card]  

let suite = "search test suite" >::: tests

let _ = run_test_tt_main suite