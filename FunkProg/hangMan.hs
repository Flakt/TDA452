import System.Random(randomRIO)
import Data.List(nub)

main = do word <- getRandomWord
          play word ""

getRandomWord :: IO String
getRandomWord = do text <- readFile "/usr/share/dict/words"
                   let ls = lines text
                   ix <- randomRIO (0,length ls-1)
                   return (ls !! ix)

play :: String -> String -> IO ()
play word guesses =
  do putStrLn [if l `elem` guesses then l else '_'  | l <- word]
     putStrLn ("Number of guesses left: "++show guessesLeft)
     if and [l `elem` guesses | l <- word]
       then putStrLn "You won!"
       else if guessesLeft <= 0
            then do putStrLn "You lost! Game over!"
                    putStrLn ("The word was: "++word)
            else do putStrLn "Enter your guess:"
                    s <- getLine
                    play word (s++guesses)
  where
    guessesLeft = length word + 4 - length guesses
