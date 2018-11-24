module BlackJack where
import Cards
import RunGame
import System.Random

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
-- | Returns the value of a hand (ace = 11 or 1)

value :: Hand -> Integer
value hand
    | valueInitial hand > 21 = valueWithValueOfAce hand
    | otherwise           = valueInitial hand

valueInitial :: Hand -> Integer
valueInitial Empty = 0
valueInitial (Add card hand) = valueCard card + valueInitial hand

-- | Returns the value of a card (ace = 11)
valueCard :: Card -> Integer
valueCard card = valueRank (rank card)

-- | Returns the value of a rank (ace = 11)
valueRank :: Rank -> Integer
valueRank Ace           = 11
valueRank (Numeric n)   = n
valueRank _             = 10 -- Not a number or ace => jack, queen or king

-- | Returns the value of a hand, replacing the value of aces (ace = 1)
-- each ace is worth 10 less
valueWithValueOfAce :: Hand -> Integer
valueWithValueOfAce hand = valueInitial hand -(aces * 10)
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
gameOver hand = value hand > 21 &&
                valueWithValueOfAce hand > 21

-- A4
-- | Returns the winners (aces = 11 or 1 if needed)
-- a player wins iff they have more than the bank  and if they are not bust or
-- the player is not bust and the bank is
winner :: Hand -> Hand -> Player
winner guest bank
    | value guest > value bank &&
      not (gameOver guest) = Guest
    | not (gameOver guest) && gameOver bank = Guest
    | otherwise = Bank

-- B1

-- | Given two hands, put the first one on top of the other
-- Seen as two lists, a <+ b = [a0..an] <+ [b0..bn] = [a0..an,b0..bn]
(<+) :: Hand -> Hand -> Hand
(<+) Empty hand              = hand
(<+) hand Empty              = hand
(<+) (Add card hand1) hand2  = Add card (hand1 <+ hand2)

-- B2

-- | Returns a full deck of 52 cards (typed as a hand)
fullDeck :: Hand
fullDeck = foldr (<+) empty [fullSuit Spades,
                              fullSuit Hearts,
                              fullSuit Clubs,
                              fullSuit Diamonds]

-- | Helper function, returning a full given suit of 13 cards
fullSuit :: Suit -> Hand
fullSuit suit = foldr (<+) empty (
                [Add (Card (Numeric n) suit) Empty | n <- [2..10]] ++
                [Add (Card Jack suit) Empty,
                Add (Card Queen suit) Empty,
                Add (Card King suit) Empty,
                Add (Card Ace suit) Empty])

-- B3

-- | Given a deck and a hand, draws one card from the deck and puts it into
-- the hand, returning (new_deck, new_hand)
-- returns error if the deck is empty
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty hand = error "draw: deck is empty"
draw (Add card deck) hand = 
    (deck, Add card Empty <+ hand)

-- B4

-- | Generates a hand for the bank
playBank :: Hand -> Hand
playBank deck =
    drawBank deck Empty

-- | Helper function for playing the bank
drawBank :: Hand -> Hand -> Hand
drawBank deck hand 
    | value hand >= 16 = hand    
    | otherwise = drawBank newDeck newHand
    where 
        (newDeck, newHand) = draw deck hand


-- B5

-- | Given a StdGen and a hand of cards, shuffle the cards
-- The method is to draw a random card from the source and
-- append it to the hand until the source is empty
shuffle :: StdGen -> Hand -> Hand
shuffle gen hand = shuffle' gen hand Empty

-- | Helper function that recursively empties the source
shuffle' :: StdGen -> Hand -> Hand -> Hand
shuffle' gen Empty hand   = hand
shuffle' gen source hand  =
    shuffle' gen2 source2 hand2
    where
        (gen2, source2, hand2) = drawRandom gen source hand

-- | Draws a random card from a deck
drawRandom :: StdGen -> Hand -> Hand -> (StdGen, Hand, Hand)
drawRandom gen deck hand = 
    (gen2, newDeck, newHand)
    where 
        (newDeck, newHand) = drawNth deck hand randomInt
        (randomInt, gen2) = randomR (0, size deck + (-1)) gen

-- | Draws the nth card from a deck and return (newDeck, newHand)
drawNth :: Hand -> Hand -> Integer -> (Hand, Hand)
drawNth deck hand 0     = draw deck hand    
drawNth Empty hand n    = draw Empty hand
drawNth (Add card deck) hand n 
    | n > 0     = (Add card newDeck, newCard)
    | otherwise = error "drawNth: drawing from a negative index" 
    where 
        (newDeck, newCard) = drawNth deck hand (n + (-1))

------------------------------Tests------------------------------------------

-- | Tests that a hand of n aces counts n aces
prop_countAces :: Integer -> Bool
prop_countAces n =
    countAces(handAces (abs n)) == abs n

-- | Helper function for generating a hand of size n of only ace of hearts
handAces :: Integer -> Hand
handAces n  | n <= 0 = Empty
            | otherwise = Add (Card Ace Hearts) (handAces (n-1))


-- Tests that the bank always draws while having a value 15 or less
-- Thus the greatest possible value is 15 + 10 and the smallest is 16
prop_playBank :: Hand -> Bool
prop_playBank deck =
    value deck < 16 ||  -- guard against deck which the bank can't play with  
    (val <= 25 && val >= 16)
    where val = value (playBank deck)

-- check that the total value is the same before and after drawing
prop_draw_valueDiff :: Hand -> Hand -> Bool
prop_draw_valueDiff deck hand =
    size deck == 0 || -- guard against errors when using QuickCheck
    valueInitial deck + valueInitial hand == valueInitial newDeck + valueInitial hand
    where
        (newDeck, hand) = draw deck hand

-- check that the size is changed appropriately
prop_draw_size :: Hand -> Hand -> Bool
prop_draw_size deck hand = 
    size deck == 0 || -- guard against errors when using QuickCheck
    ((size newDeck + 1) == size deck) &&
    (size hand == (size hand + 1))
    where (newDeck, hand) = draw deck hand 

-- check that the size of a suit is 13
prop_fullSuit_size :: Bool
prop_fullSuit_size = size (fullSuit Spades) == 13

-- check that the total value of a suit is some constant
prop_fullSuit_value :: Bool
prop_fullSuit_value = valueInitial (fullSuit Hearts) ==
    suitTotalValue

suitTotalValue :: Integer
suitTotalValue = 11 + (10 * 4) + sum [9,8 .. 2]

-- check <+ associativity
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc a b c =
    a <+ (b <+ c) == (a <+ b) <+ c

-- check <+ retains size correctly
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf a b =
    size a + size b == size (a <+ b)

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards gen card hand =
    card `belongsTo` hand == card `belongsTo` shuffle gen hand

belongsTo :: Card -> Hand -> Bool
card `belongsTo` Empty = False
card `belongsTo` (Add card' hand) = card == card' || card `belongsTo` hand

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle gen deck = size deck == size (shuffle gen deck)

implementation :: Interface
implementation = Interface
  { iEmpty    = empty
  , iFullDeck = fullDeck
  , iValue    = value
  , iGameOver = gameOver
  , iWinner   = winner 
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffle
  }

main :: IO ()
main = runGame implementation