{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module TransitionGraphSpec where

import           Control.Monad.Free    (Free (..), foldFree, liftF)
import qualified Data.ByteString.Char8 as BS
import           Test.Hspec

import           Lib

printString :: String -> IO (String, ())
printString msg = do
  putStrLn msg
  pure ("", ())

nop :: IO (String, ())
nop = pure ("", ())

travel3Graph :: Graph IO () ()
travel3Graph = graph $
  with (printString "3")
    <~> on "forward" (leaf nop)

travel2Graph :: Graph IO () ()
travel2Graph = graph $
  with (printString "2")
    <~> on "forward" travel3Graph

travel1Graph :: Graph IO () ()
travel1Graph = graph $
  with (printString "1")
    <~> on "forward" travel2Graph

ioRunner :: IO (Event, output) -> IO (Event, output)
ioRunner act = act >>= \(_, o) -> pure ("forward", o)

spec = describe "Graph transitions test." $
  it "Test Graph transitions." $
    runGraph (Runtime ioRunner (== "back")) travel1Graph
