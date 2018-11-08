module BlackJack where
import Cards
import RunGame


-- An empty hand (of size 0)
empty :: Hand
empty = Empty

-- Returns the value of a rank
valueRank :: Rank -> Integer
-- valueRank rank | rank `elem` [Jack,Queen,King] = 10 -- alternative implementation
valueRank Jack       = 10
valueRank Queen      = 10
valueRank King       = 10
valueRank Ace        = 11
valueRank Numeric n  = n

-- Returns the value of a suit (for tie breakers)
-- could also be done by having Suit derive from Ord
valueSuit :: Suit -> Integer
valueSuit Heart     = 4
valueSuit Spades    = 3
valueSuit Diamonds  = 2
valueSuit Clubs     = 1

-- Returns the value of a hand (without smart aces)
value :: Hand -> Integer
value Empty = 0
value (Add card hand) = valueOf (rank card) + value hand

-- Returns the value of a hand where the value of aces is replaced value 
valueAceValue :: Hand -> Integer -> Integer
valueAceValue hand value = value hand -(aces * 11) + aces * value 
    where 
        aces = countAces hand 

-- Returns the number of aces in a hand
countAces :: Hand -> Integer
countAces Empty = 0
countAces (Add card hand) 
        | rank card == Ace = 1 + countAces hand
        | otherwise        = countAces hand

-- Tests that a hand of n aces counts n aces
propCountAces :: Integer -> Bool
propCountAces n =
        countAces(hand n) == n
        where 
            hand n 
                | n <= 0 = Empty
                | otherwise = Add Card Ace Hearts hand (n-1)

-- Returns true if the hand is bust (with smart aces)
gameOver :: Hand -> Bool

-- Returns the winners (1st parameter is player's hand)
winner :: Hand -> Hand -> Player
