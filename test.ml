open OUnit2
open Card
open Hand
open Deck
open Testexamples

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


(** [ test_hand_cateogry name expected_output input]
    constructs an OUnit test that examines the quality of [expected_output]
    with [category_of input]*)
let test_hand_category
    (name: string)
    (expected_output : category)
    (input : Hand.t) = 
  name >:: (fun _ ->
      assert_equal expected_output (Hand.category_of input))





let test_hand = List.flatten [
    List.map (test_hand_category "random royal" RoyalFlush)
      examples_royal;

    [test_hand_category "TEST TEST" StraightFlush
       (Hand.empty
        |> Hand.insert (Card.make_card HEART 5)
        |> Hand.insert (Card.make_card HEART 4)
        |> Hand.insert (Card.make_card HEART 3)
        |> Hand.insert (Card.make_card HEART 2)
        |> Hand.insert (Card.make_card HEART 1));
     test_hand_category "TEST TEST" StraightFlush
       (Hand.empty
        |> Hand.insert (Card.make_card HEART 12)
        |> Hand.insert (Card.make_card HEART 11)
        |> Hand.insert (Card.make_card HEART 10)
        |> Hand.insert (Card.make_card HEART 9)
        |> Hand.insert (Card.make_card HEART 8));
    ];
    List.map (test_hand_category "random straightflush" StraightFlush)
      (example_straight_flush 100 100);
    List.map (test_hand_category "random straight" Straight)
      (example_straight 100 100);
    List.map (test_hand_category "random flush" Flush)
      (example_flush 100 100);
    List.map (test_hand_category "random four" FourOfAKind)
      (example_four 100 100);
    List.map (test_hand_category "random full" FullHouse)
      (example_full 100 100);
    List.map (test_hand_category "random three" ThreeOfAKind)
      (example_three 100 100);
    List.map (test_hand_category "random two p" TwoPair)
      (example_two_pair 100 100);
    List.map (test_hand_category "random pair" Pair)
      (example_pair 100 100);
  ] 


let tests = List.flatten [test_card;test_hand]  

let suite = "search test suite" >::: tests

let _ = run_test_tt_main suite