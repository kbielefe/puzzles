import Data.Maybe
import Data.List

sliding :: Int -> [b] -> [[b]]
sliding 0 _  = []
sliding _ [] = []
sliding size list@(x:xs)
  | length list >= size = take size list : sliding size xs
  | otherwise = []

fromRomanDigit :: Char -> Int
fromRomanDigit roman = case roman of
  'M' -> 1000
  'D' -> 500
  'C' -> 100
  'L' -> 50
  'X' -> 10
  'V' -> 5
  'I' -> 1

inversion :: [Int] -> Bool
inversion (x:y:xs) = x < y

subtractive :: [Int] -> Int
subtractive digits = sum $ map head $ filter inversion $ sliding 2 digits

romanToInt :: String -> Int
romanToInt roman = 
  let digits = map fromRomanDigit roman
  in sum digits - (subtractive digits) * 2
                      
toRomanDigit = [
  (1000, "M"),
  (900,  "CM"),
  (500,  "D"),
  (400,  "CD"),
  (100,  "C"),
  (90,   "XC"),
  (50,   "L"),
  (40,   "XL"),
  (10,   "X"),
  (9,    "IX"),
  (5,    "V"),
  (4,    "IV"),
  (1,    "I")]

intToRoman :: Int -> String
intToRoman int | int <= 0 = ""
intToRoman int = 
  let (decimal, roman) = fromJust $ find (\x -> fst x <= int) toRomanDigit
  in roman ++ intToRoman (int - decimal)

main = print $ intToRoman 2014
