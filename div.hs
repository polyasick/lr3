module Main where

-- деление одного целого числа на другое, возвращает целое и остаток
intDiv :: Integer->Integer->(Integer, Integer)
intDiv x y 
     | abs y > abs x = (0,x)
     | x > 0 && y > 0 = let (q, r) = intDiv (x-y) y
                        in (1 + q, r)
     | x > 0 && y < 0 = let (q, r) = intDiv (x-(negate y)) (negate y)
                        in (negate (q + 1), r)
     | y == 0 = error "div 0"
     | otherwise = let (q, r) = intDiv ((negate x)-(negate y)) (negate y)
                   in (q + 1, negate r)

main :: IO ()
main = undefined