{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module TransitionGraphSpec where

import           Control.Monad.Writer.Lazy (Writer, runWriter, tell)
import           Test.Hspec

import           Lib

type TestLang = Writer String

printString :: String -> TestLang (String, ())
printString msg = do
  tell msg
  pure ("forward", ())

nop :: TestLang (String, ())
nop = pure ("", ())

travel3Graph :: Graph TestLang () ()
travel3Graph = graph $
  with (printString "3")
    <~> on "forward" (leaf nop)

travel2Graph :: Graph TestLang () ()
travel2Graph = graph $
  with (printString "2")
    <~> on "forward" travel3Graph

travel1Graph :: Graph TestLang () ()
travel1Graph = graph $
  with (printString "1")
    <~> on "forward" travel2Graph

-- Implementation tip:
--      on "forward" travel2Graph :: TransitionTemplate

travel0Graph :: Graph TestLang () ()
travel0Graph = graph $
  with (printString "0")
    ~~> travel1Graph

spec = describe "Graph transitions test." $
  it "Test Graph transitions." $ do
    let (_, result) = runWriter $ runGraph' id (== "back") travel0Graph
    result `shouldBe` "0123"
