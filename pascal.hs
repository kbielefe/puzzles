sliding :: [a] -> [[a]]
sliding xs =
  if length xs < 2 then []
  else let (x:y:tail) = xs
       in [x, y] : sliding (y:tail)

pascal :: [Integer] -> [Integer]
pascal prev =  1 : map (\[x,y] -> x + y) (sliding prev) ++ [1]
