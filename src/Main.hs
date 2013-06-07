module Main (main) where

main = print $ sum' 10000 0

inc :: Int -> Int
inc n = n + 1

sum' :: Int -> Int -> Int
sum' 0 acc = acc
sum' n acc = sum' (n-1) (acc+1)
