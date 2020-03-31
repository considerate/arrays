{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module Main where

import qualified Data.Arrays.Array as Array
import qualified Data.Arrays.Fin   as Fin
import           Data.Arrays.List
import           Data.Arrays.Nat

arr :: Array.Array '[N6, N3, N2] Int
arr = Array.generate $ \(Cons a (Cons b (Cons c Nil))) ->
  Fin.toInt a + Fin.toInt b + Fin.toInt c

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  print arr
  print (Array.sum arr)
