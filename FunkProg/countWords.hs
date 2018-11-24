
module Main where --This can be omitted, since the default module name is Main

import Data.List(group,sort)

main :: IO ()
main = interact wordCounts

-- | Count words in a text and produce nicely formatted output
wordCounts :: String -> String
wordCounts = unlines
           . map (\(n,w)->w++": "++show n)
           . reverse
           . sort
           . map (\ws-> (length ws,head ws))
           . group
           . sort
           . words
