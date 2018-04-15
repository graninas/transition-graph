{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module TransitionGraphSpec where

import           Control.Monad.State.Lazy (State, execState, get, modify, put)
import           Test.Hspec

import           Lib

type TestLang = State String

trackStep :: String -> TestLang (String, ())
trackStep msg = do
  modify (++ msg)
  st <- get
  pure (st, ())

nop :: TestLang (String, ())
nop = pure ("", ())

node3 :: Graph TestLang () ()
node3 = graph $
  with (trackStep "3")
    <~> on "forward" (impossibleNode "3")
    -- No default, should end here.

node2 :: Graph TestLang () ()
node2 = graph $
  with (trackStep "2")
    <~> on "01" (impossibleNode "2")
     /> node3
    <~> on "0"  (impossibleNode "2")
     />          impossibleNode "2"

node1 :: Graph TestLang () ()
node1 = graph $
  with (trackStep "1")
     /> impossibleNode "1"
    <~> on "01" node2
     /> impossibleNode "1"

node0 :: Graph TestLang () ()
node0 = graph $
  with (trackStep "0")
    -/> node1

impossibleNode :: String -> Graph TestLang () ()
impossibleNode n = graph $
  with (trackStep $ n ++ " impossible")
    -/> leaf nop

spec = describe "Graph transitions test." $
  it "Test Graph transitions." $ do
    let result = execState (runGraph' id (== "back") node0) ""
    result `shouldBe` "0123"
