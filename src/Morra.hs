{-# LANGUAGE GADTs #-}
module Morra where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import System.Random

-- count for player, then count for computer, then random generator
data MorraState g where
  MorraState :: RandomGen g => Int -> Int -> g -> MorraState g

winner :: MorraState g -> String
winner (MorraState p c _) = if(p >= c) then "P" else "C"

-- evaluates user input and computer answer and returns,
-- who wins: (1,0) - player, (0,1) - computer
evalTurn :: Int -> Int -> (Int, Int)
evalTurn p c =
  let sum = p + c
  in (sum `mod` 2, (sum + 1) `mod` 2)

-- takes user input and returns computer's result
computerTurn :: (Monad m, RandomGen g) => StateT (MorraState g) m Int
computerTurn =
  StateT $ \(MorraState p c gen) ->
    let
      (computerInput, gen') = randomR (1, 10) gen
    in 
      return (computerInput, MorraState p c gen')

updateState :: (Monad m, RandomGen g) => Int -> Int -> StateT (MorraState g) m String
updateState userInput computerInput =
  StateT $ \(MorraState p c gen) ->
    let
      (p', c') = evalTurn userInput computerInput
      winner   = if(p' == 1) then "P" else "C"
    in 
      return (winner, MorraState (p + p') (c + c') gen)

initialState :: MorraState StdGen
initialState = MorraState 0 0 (mkStdGen 0)

loop ::  RandomGen g => MorraState g -> IO (MorraState g)
loop s = do
  putStr "P: "
  p <- readLn :: IO Int
  case p of
   0 -> return s
   _ -> do
          (c, s') <- runStateT computerTurn s
          putStrLn $ "C: " <> (show c)
          (w, s'') <- runStateT (updateState p c) s'
          putStrLn $ w <> " wins"
          loop s''
  
runMorra :: IO()
runMorra = do
  putStrLn "Welcome to Morra"
  finalState <- loop initialState
  putStrLn $ "Game Over"
  let (MorraState p c _) = finalState
  putStrLn $ (winner finalState) <> " is the winner. P:C = "
             <> (show p) <> ":" <> (show c)
