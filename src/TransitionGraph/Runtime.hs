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

data LangResult a b   = GoForward a b | GoBackward
data TransitionResult
  = Fallback
  | AutoFallbackRerun
  | FallbackRerun
  | Done

runLang
  :: (Monad m, Monad lang)
  => Runtime lang m
  -> lang a
  -> m (LangResult Event a)
runLang (Runtime runLang isBackEvent) lang = do
  (e, i) <- runLang lang
  if isBackEvent e
    then pure GoBackward
    else pure $ GoForward e i

getLang
  :: i
  -> GraphF lang i o b
  -> lang b
getLang input (GraphF1 lang _) = lang input

type ThisBackable = Bool
type AutoReturn   = Bool

runTransition'
  :: (Monad m, Monad lang)
  => Runtime lang m
  -> ThisBackable
  -> AutoReturn
  -> i
  -> GraphF lang i o b
  -> m TransitionResult
runTransition' runtime autoReturn backable i3 g3 = do
  let f3 = getLang i3 g3
  transitionResult <- runTransition runtime autoReturn f3 g3
  case transitionResult of
    Done              -> pure Done
    AutoFallbackRerun -> pure FallbackRerun
    FallbackRerun     -> runTransition' runtime backable False i3 g3
    Fallback          ->
      if backable
      then pure FallbackRerun
      else pure Done -- throw "No fallback"

runTransition
  :: (Monad m, Monad lang)
  => Runtime lang m
  -> AutoReturn
  -> lang b
  -> GraphF lang i o b
  -> m TransitionResult
runTransition runtime autoReturn f2 g2 = do
  langResult <- runLang runtime f2
  if autoReturn
    then pure AutoFallbackRerun
    else case langResult of
      GoBackward      -> pure Fallback
      GoForward e2 i3 -> case matchTransition e2 g2 of
        Nop                         -> pure Done
        Backable    g3@(Graph g3Ex) -> runExists (runTransition' runtime True  False i3) g3Ex
        ForwardOnly g3@(Graph g3Ex) -> runExists (runTransition' runtime False False i3) g3Ex
        AutoBack    g3@(Graph g3Ex) -> runExists (runTransition' runtime True  True  i3) g3Ex

runGraph
  :: (Monad m, Monad lang)
  => Runtime lang m
  -> Graph lang () ()
  -> m ()
runGraph runtime (Graph ex) = do
  _ <- runExists (runTransition' runtime False False ()) ex
  pure ()
