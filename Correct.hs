{-
 - Weekly Coding Challenge 14: Phoning it in
 - Spell check
 -
 - Maps the Levenshtein distance to every word in the dictionary, using
 - a crazy-optimized algorithm found on the interwebs.
 - Sorts by the distance.
 - Takes only the first one if an exact match was found (spelled correctly),
 - otherwise shows the top 3.
 -}
import System.Environment
import Data.List (sortOn)

-- From https://wiki.haskell.org/Edit_distance
dist :: Eq a => [a] -> [a] -> Int
dist a b
    = last (if lab == 0 then mainDiag
      else if lab > 0 then lowers !! (lab - 1)
           else{- < 0 -}   uppers !! (-1 - lab))
    where mainDiag = oneDiag a b (head uppers) (-1 : head lowers)
          uppers = eachDiag a b (mainDiag : uppers) -- upper diagonals
          lowers = eachDiag b a (mainDiag : lowers) -- lower diagonals
          eachDiag a [] diags = []
          eachDiag a (bch:bs) (lastDiag:diags) = oneDiag a bs nextDiag lastDiag : eachDiag a bs diags
              where nextDiag = head (tail diags)
          oneDiag a b diagAbove diagBelow = thisdiag
              where doDiag [] b nw n w = []
                    doDiag a [] nw n w = []
                    doDiag (ach:as) (bch:bs) nw n w = me : (doDiag as bs me (tail n) (tail w))
                        where me = if ach == bch then nw else 1 + min3 (head w) nw (head n)
                    firstelt = 1 + head diagBelow
                    thisdiag = firstelt : doDiag a b firstelt diagAbove (tail diagBelow)
          lab = length a - length b
          min3 x y z = if x < y then x else min y z

correct :: Eq a => [a] -> [[a]] -> [[a]]
correct input dictionary = take numToTake (map fst sorted)
    where numToTake = if ((== 0) . snd . head) sorted then 1 else 3
          sorted = sortOn snd editDistances
          editDistances = zip dictionary (map (dist input) dictionary)

main :: IO ()
main = do
    [file,input] <- getArgs
    text         <- readFile file
    putStrLn $ show $ correct input (words text)
