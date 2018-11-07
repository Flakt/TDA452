module BlackJack where
import Cards
import RunGame


-- An empty hand (of size 0)
empty :: Hand
empty = Empty

-- Returns the value of a rank
valueRank :: Rank -> Integer
valueRank rank 
        | rank == Jack  = 10
        | rank == Queen = 10
        | rank == King  = 10
        | rank == Ace   = 11
        | rank == Numeric n = n

-- Returns the value of a suit (for tie breakers)
valueSuit :: Suit -> Integer
valueSuit suit
        | suit == Heart     = 4
        | suit == Spades    = 3
        | suit == Diamonds  = 2
        | suit == Clubs     = 1

-- Returns the value of a hand (without smart aces)
value :: Hand -> Integer
value Empty = 0
value (Add card hand) = valueOf (rank card) + value hand

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
