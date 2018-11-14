module BlackJack where
import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)

{-
3.1 A0 evaluation of size

size hand2
    = size (Add (Card (Numeric 2) Hearts)
        (Add (Card Jack Spades) Empty))
    = 1 + size (Add (Card Jack Spades) Empty)
    = 1 + 1 + size Empty
    = 1 + 1 + 0
    = 2
-}

-- A1
-- | An empty hand (of size 0)
empty :: Hand
empty = Empty

-- A2
-- | Returns the value of a hand (ace = 11)
valueHand :: Hand -> Integer
valueHand Empty = 0
valueHand (Add card hand) = valueCard card + valueHand hand

-- | Returns the value of a card (ace = 11)
valueCard :: Card -> Integer
valueCard card = valueRank (rank card)

-- | Returns the value of a rank (ace = 11)
valueRank :: Rank -> Integer
valueRank Ace           = 11
valueRank (Numeric n)   = n
valueRank _             = 10 -- Not a number or ace => jack, queen or king

-- | Returns the value of a hand, replacing the value of aces (ace = n)
valueAceValue :: Hand -> Integer -> Integer
valueAceValue hand value = valueHand hand -(aces * 11) + aces * value
    where
        aces = countAces hand

-- | Returns the number of aces in a hand
countAces :: Hand -> Integer
countAces Empty = 0
countAces (Add card hand)
    | rank card == Ace = 1 + countAces hand
    | otherwise        = countAces hand

-- A3
-- | True iff the hand is bust (aces = 11 or 1  if needed)
gameOver :: Hand -> Bool
gameOver hand = valueHand hand > 21 &&
                valueAceValue hand 1 > 21

-- A4
-- | Returns the winners (aces = 11 or 1 if needed)
winner :: Hand -> Hand -> Player
winner guest bank
    | valueHandBest guest > valueHandBest bank = Guest
    | otherwise = Bank

-- | Returns the best possible value of a hand (ace = 11 or 1  if needed)
valueHandBest :: Hand -> Integer
valueHandBest hand
    | valueHand hand > 21 = valueAceValue hand 1
    | otherwise           = valueHand hand

-- B1

-- | Given two hands, put the first one on top of the other
-- Seen as two lists, a <+ b = [a0..an] <+ [b0..bn] = [a0..an,b0..bn]
(<+) :: Hand -> Hand -> Hand
(<+) Empty hand              = hand
(<+) hand Empty              = hand
(<+) (Add card hand1) hand2  = (Add card (hand1 <+ hand2))

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc a b c =
    a <+ (b <+ c) == (a <+ b) <+ c

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf a b =
    size a + size b == size (a <+ b)

-- B2

-- | Returns a full deck of 52 cards (typed as a hand)
fullDeck :: Hand
fullDeck = foldr (<+) empty ([fullSuit Spades,
                              fullSuit Hearts,
                              fullSuit Clubs,
                              fullSuit Diamonds])

prop_fullDeck_size :: Bool
prop_fullDeck_size = size fullDeck == 52

prop_fullDeck_value :: Bool
prop_fullDeck_value = valueHand fullDeck ==
    4 * suitTotalValue

-- | Helper function, returning a full given suit of 13 cards
fullSuit :: Suit -> Hand
fullSuit suit = foldr (<+) empty (
                [(Add (Card (Numeric n) suit) Empty) | n <- [2..10]] ++
                [(Add (Card Jack suit) Empty),
                (Add (Card Queen suit) Empty),
                (Add (Card King suit) Empty),
                (Add (Card Ace suit) Empty)])

prop_fullSuit_size :: Bool
prop_fullSuit_size = size (fullSuit Spades) == 13

prop_fullSuit_value :: Bool
prop_fullSuit_value = valueHand (fullSuit Hearts) ==
    suitTotalValue

suitTotalValue = 11 + (10 * 4) + sum [9,8 .. 2]

-- B3

-- | Given a deck and a hand, draws one card from the deck and puts it into
-- the hand, returning (new_deck, new_hand)
-- returns error if the deck is empty
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty hand = error "draw: deck is empty"
draw deck hand = (Empty, Empty) -- Todo

-- check that the total value is the same before and after drawing
prop_draw_valueDiff :: Hand -> Hand -> Bool
prop_draw_valueDiff deck hand =
     valueHand deck + valueHand hand == valueHand newDeck + valueHand newHand
     where
        (newDeck, newHand) = draw deck hand

-- B4

-- | Generates a hand for the bank
--playBank :: Hand -> Hand
--playBank deck =
--    drawBank deck Empty

-- | Helper function for playing the bank
--drawBank :: Hand -> Hand -> Hand
-- deck hand = newHand -- TODO

-- Tests that the bank always draws while having a value 15 or less
-- Thus the greatest possible value is 15 + 10 and the smallest is 16
--prop_playBank :: Hand -> Bool
--prop_playBank deck =
--    val <= 25 && val >= 16
--    where val = valueHand playBank

-- B5

-- | Given a StdGen and a hand of cards, shuffle the cards
--shuffle :: StdGen -> Hand -> Hand
--shuffle gen hand = Empty -- TODO



------------------------------Tests------------------------------------------

-- Some examples for basic testing

card10      = Card (Numeric 10) Hearts
card9       = Card (Numeric 9) Hearts
card2       = Card (Numeric 2) Hearts
cardAce     = Card Ace Hearts
cardJack    = Card Jack Hearts

hand10      = Add card10 Empty
hand12      = Add card2 Empty
hand32or12  = Add cardAce (Add cardAce (Add card10 Empty))
hand22or2   = Add cardAce (Add cardAce Empty)
hand21      = Add cardAce (Add cardJack Empty)
hand22      = Add card10 (Add card10 (Add card2 Empty)) -- *
hand32or22  = Add cardAce (Add cardJack (Add card9 (Add card2 Empty))) -- *

-- * should always bust

-- | Tests that a hand of n aces counts n aces
propCountAces :: Integer -> Bool
propCountAces n =
    countAces(handAces (abs n)) == abs n

-- | Helper function for generating a hand of size n of only ace of hearts
handAces :: Integer -> Hand
handAces n  | n <= 0 = Empty
            | otherwise = Add (Card Ace Hearts) (handAces (n-1))
