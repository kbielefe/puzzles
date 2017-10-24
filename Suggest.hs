{-
 - Coding challenge 14: Phoning it in
 - Creates up to 3 suggestions based on an input suggestion and a corpus.
 -
 - Normalizes the corpus by replacing all non-word characters with a space.
 - Runs a sliding window on the corpus, creating a list of [(first, second)]
 - Keeps all members of the list where first == the input
 - Groups those into lists of [[second, second, second], [another, another]]
 - Sorts that by the negative length of the list, so longest comes first
 - takes the first 3
 -}
import System.Environment (getArgs)
import Data.List (sortOn, sort, group)
import Data.Char (toLower)

sliding :: [a] -> [(a, a)]
sliding [] = []
sliding xs = zip xs (drop 1 xs)

-- Now this is a proper Haskell type signature ;-)
suggest :: (Ord a) => a -> [a] -> [a]
suggest input corpus =  take 3 sortedByFrequency
    where sortedByFrequency = map head $ sortOn (negate . length) groups
          groups = group . sort $ followers
          followers = map snd $ filter ((== input) . fst) (sliding corpus)

nonwordToSpace :: Char -> Char
nonwordToSpace char = if char `elem` "abcdefghijklmnopqrstuvwxyz'-" then char else ' '

normalize :: String -> String
normalize = map (nonwordToSpace . toLower)

main :: IO ()
main = do
    [file,input] <- getArgs
    text         <- readFile file
    putStrLn $ show $ suggest (normalize input) (words $ normalize text)
