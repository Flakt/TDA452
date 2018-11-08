import Test.QuickCheck

-- Example of custom type(class)
data Suit = Spades
          | Hearts
          | Diamonds
          | Clubs
            deriving (Show,Eq)

data Colour = Black
            | Red
              deriving (Show)

-- Function using a self-defined type
colour :: Suit -> Colour
colour Spades = Black
colour Clubs = Black
colour c = Red

data Rank = Numeric Int
          | Jack
          | Queen
          | King
          | Ace
            deriving (Show,Eq,Ord) -- Allows types to be equaled and compared

-- Alternative to Rank type
{-
data Rank' = N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | Jack | Queen | King | Ace
             deriving (Show)
-}

all_ranks = [Numeric n | n <- [2..10]] ++ [Jack, Queen, King, Ace]

-- Comparing types in a list using Pattern matching
rankBeats :: Rank -> Rank -> Bool
rankBeats r1 r2 = r1 > r2

-- Combining data types to a new data type, rank and suit returns the rank/suit
-- of the card
data Card = Card { rank :: Rank, suit :: Suit}
            deriving (Show)

-- Creating a card
example_card_1 = Card Ace Hearts
example_card_2 = Card {rank = King, suit = Spades}
{-
-- Extracting Rank from Card through Pattern matching
rank :: Card -> Rank
rank (Card r s) = r

-- Same for suit
suit :: Card -> Suit
suit (Card r s) = s
-}

cardBeats :: Card -> Card -> Bool
cardBeats (Card r1 s1) (Card r2 s2) = s1 == s2 && rankBeats r1 r2

-- Alternative implementation of the function above
cardBeats' :: Card -> Card -> Bool
cardBeats' card1 card2 = suit card1 == suit card2
                       && rankBeats(rank card1) (rank card2)

-- Add Card Hand recursivly adds a card to the existing hand
data Hand = Empty | Add Card Hand
            deriving (Show)

example_hand_0 = Empty
example_hand_1 = Add example_card_1 example_hand_0
example_hand_2 = Add example_card_2 example_hand_1

handBeats :: Hand -> Card -> Bool
handBeats Empty           beat = False
handBeats (Add card hand) beat = cardBeats card beat || handBeats hand beat

-- Return the card that beat the given card
betterCards :: Hand -> Card -> Hand
betterCards Empty beat = Empty
betterCards (Add card hand) beat
            | cardBeats card beat = Add card (betterCards hand beat)
            | otherwise           = betterCards hand beat

-- Chooses a card from the hand to beat a given card, gives the lowest card
-- in hand if the hand can't beat the given card
chooseCard :: Card -> Hand -> Card
chooseCard beat hand
    | handBeats hand beat       = lowestCard (betterCards hand beat)
    | haveSuit hand (suit beat) = lowestCard (sameSuit hand (suit beat))
    | otherwise                 = lowestCard hand

-- Find (one of) the lowest card in a hand
lowestCard :: Hand -> Card
lowestCard (Add card Empty) = card
lowestCard (Add card hand) | rank card < rank low = card
                           | otherwise            = low
    where low = lowestCard hand

-- Return a hand containing only the cards of the given suit
sameSuit :: Hand -> Suit -> Hand
sameSuit hand s = betterCards hand (Card (Numeric 1) s) -- A hack

-- Does the hand contaiin a card of the given suit?
haveSuit :: Hand -> Suit -> Bool
haveSuit Empty s = False
haveSuit (Add card hand) s = suit card == s || haveSuit hand s
