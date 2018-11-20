module TestHands where
import BlackJack
import Cards

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