module BlackJack where
import Cards
import RunGame
import Test.QuickCheck

{-
3.1 A0

size hand2 
    = size (Add (Card (Numeric 2) Hearts) 
        (Add (Card Jack Spades) Empty))
    = 1 + size (Add (Card Jack Spades) Empty)
    = 1 + 1 + size Empty 
    = 1 + 1 + 0
    = 2
-}

-- Returns the winners (aces = 11 or 1 if needed)
winner :: Hand -> Hand -> Player
winner guest bank 
    | valueHandBest guest > valueHandBest bank = Guest
    | otherwise = Bank

-- Returns the best possible value of a hand (ace = 11 or 1  if needed)
valueHandBest :: Hand -> Integer
valueHandBest hand 
    | valueHand hand > 21 = valueAceValue hand 1
    | otherwise           = valueHand hand            

-- Returns the value of a hand (ace = 11)
valueHand :: Hand -> Integer
valueHand Empty = 0
valueHand (Add card hand) = valueCard card + valueHand hand

-- Returns the value of a card (ace = 11)
valueCard :: Card -> Integer
valueCard card = valueRank (rank card)

-- Returns the value of a rank (ace = 11)
valueRank :: Rank -> Integer
valueRank Jack       = 10
valueRank Queen      = 10
valueRank King       = 10
valueRank Ace        = 11
valueRank (Numeric n)  = n

-- Returns the value of a hand, replacing the value of aces (ace = n) 
valueAceValue :: Hand -> Integer -> Integer
valueAceValue hand value = valueHand hand -(aces * 11) + aces * value 
    where 
        aces = countAces hand 

-- Returns the number of aces in a hand
countAces :: Hand -> Integer
countAces Empty = 0
countAces (Add card hand) 
        | rank card == Ace = 1 + countAces hand
        | otherwise        = countAces hand

-- Helper function for generating a hand of size n of only ace of hearts
handAces :: Integer -> Hand
handAces n  | n <= 0 = Empty
            | otherwise = Add (Card Ace Hearts) (handAces (n-1))    

-- Not used in A

-- Returns true if the hand is bust (aces = 11 or 1  if needed)
gameOver :: Hand -> Bool
gameOver hand = valueHand hand > 21 &&
                valueAceValue hand 1 > 21

-- An empty hand (of size 0)
empty :: Hand
empty = Empty

------------------------------Tests------------------------------------------

card10 = Card (Numeric 10) Hearts
card2 = Card (Numeric 2) Hearts
cardAce = Card Ace Hearts
cardJack = Card Jack Hearts

hand10 = Add card10 Empty       
hand12 = Add card2 Empty
hand22 = Add card10 (Add card10 (Add card2 Empty)) -- should always bust
hand32or12 = Add cardAce (Add cardAce (Add card10 Empty))
hand22or2 = Add cardAce (Add cardAce Empty)
hand21 = Add cardAce (Add cardJack Empty)

-- Tests that a hand of n aces counts n aces
propCountAces :: Integer -> Bool
propCountAces n =
    countAces(handAces (abs n)) == abs n
