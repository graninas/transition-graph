{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}

module TransitionGraph.Runtime where

import           Control.Monad             (void, when)
import           Control.Monad.Free        (Free (..), foldFree, liftF)
import           Control.Monad.State       (State (..), evalState, execState,
                                            get, put, runState)
import qualified Control.Monad.Trans.State as ST

import           Data.Exists

import           TransitionGraph.Graph
import           TransitionGraph.Interpreter

data Runtime lang m = Runtime
  { runLang_     :: forall output. lang output -> m (Event, output)
  , isBackEvent_ :: Event -> Bool
  }

data LangResult a b   = Forward a b | Backward
data TransitionResult = Fallback    | FallbackRerun | Done

runLang'
  :: (Monad m, Monad lang)
  => Runtime lang m
  -> lang a
  -> m (LangResult Event a)
runLang' (Runtime runLang isBackEvent) flow = do
  (e, i) <- runLang flow
  if isBackEvent e
    then pure Backward
    else pure $ Forward e i

getLang
  :: i
  -> GraphF lang i o b
  -> lang b
getLang input (GraphF1 flowF _) = flowF input

type ThisBackable = Bool

runTransition'
  :: (Monad m, Monad lang)
  => Runtime lang m
  -> ThisBackable
  -> i
  -> GraphF lang i o b
  -> m TransitionResult
runTransition' runtime backable i3 g3 = do
  let f3 = getLang i3 g3
  transitionResult <- runTransition runtime f3 g3
  case transitionResult of
    Done          -> pure Done
    FallbackRerun -> runTransition' runtime backable i3 g3
    Fallback      ->
      if backable
      then pure FallbackRerun
      else pure Done -- throw "No fallback"

runTransition
  :: (Monad m, Monad lang)
  => Runtime lang m
  -> lang b
  -> GraphF lang i o b
  -> m TransitionResult
runTransition runtime f2 g2 = do
  langResult <- runLang' runtime f2
  case langResult of
    Backward      -> pure Fallback
    Forward e2 i3 -> case matchTransition e2 g2 of
      Nop                           -> pure Done
      BackTrack     g3@(Graph g3Ex) -> runExists (runTransition' runtime True i3) g3Ex
      ForwardTrack  g3@(Graph g3Ex) -> runExists (runTransition' runtime False i3) g3Ex
      AutoBackTrack g3@(Graph g3Ex) -> do
        runExists (runTransition' runtime True i3) g3Ex

runGraph
  :: (Monad m, Monad lang)
  => Runtime lang m
  -> Graph lang () ()
  -> m ()
runGraph runtime (Graph ex) = do
  _ <- runExists (runTransition' runtime False ()) ex
  pure ()
