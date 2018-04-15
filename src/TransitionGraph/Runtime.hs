{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}

module TransitionGraph.Runtime where

import           Control.Monad               (void, when)
import           Control.Monad.Free          (Free (..), foldFree, liftF)
import           Control.Monad.State         (State (..), evalState, execState,
                                              get, put, runState)
import qualified Control.Monad.Trans.State   as ST

import           Data.Exists

import           TransitionGraph.Graph
import           TransitionGraph.Interpreter

type Runner lang m = forall a. lang (LangOutput a) -> m (LangOutput a)

data Runtime lang m = Runtime
  { runLang_     :: Runner lang m
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
  -> lang (LangOutput a)
  -> m (LangResult Event a)
runLang (Runtime runLang isBackEvent) lang = do
  (e, i) <- runLang lang
  if isBackEvent e
    then pure GoBackward
    else pure $ GoForward e i

getLang
  :: i
  -> GraphF lang i o b
  -> lang (LangOutput b)
getLang input (GraphF1 lang _) = lang input

newtype ThisBackable = ThisBackable Bool
newtype AutoReturn   = AutoReturn Bool

thisBackable :: ThisBackable
thisBackable = ThisBackable True

thisNotBackable :: ThisBackable
thisNotBackable = ThisBackable False

isBackable :: ThisBackable -> Bool
isBackable (ThisBackable b) = b

isAutoReturn :: AutoReturn -> Bool
isAutoReturn (AutoReturn b) = b

thisAutoReturn :: AutoReturn
thisAutoReturn = AutoReturn True

thisNotAutoReturn :: AutoReturn
thisNotAutoReturn = AutoReturn False

runTransition'
  :: (Monad m, Monad lang)
  => Runtime lang m
  -> ThisBackable
  -> AutoReturn
  -> i
  -> GraphF lang i o b
  -> m TransitionResult
runTransition' runtime backable autoReturn i3 g3 = do
  let f3 = getLang i3 g3
  transitionResult <- runTransition runtime autoReturn f3 g3
  case transitionResult of
    Done              -> pure Done
    AutoFallbackRerun -> pure FallbackRerun
    FallbackRerun     -> runTransition' runtime backable thisNotAutoReturn i3 g3
    Fallback          ->
      if isBackable backable
      then pure FallbackRerun
      else pure Done -- throw "No fallback"

runTransition
  :: (Monad m, Monad lang)
  => Runtime lang m
  -> AutoReturn
  -> lang (LangOutput b)
  -> GraphF lang i o b
  -> m TransitionResult
runTransition runtime autoReturn f2 g2 = do
  langResult <- runLang runtime f2
  if isAutoReturn autoReturn
    then pure AutoFallbackRerun
    else case langResult of
      GoBackward      -> pure Fallback
      GoForward e2 i3 -> case matchTransition e2 g2 of
        NoTransition                -> pure Done
        PassThrough g3@(Graph g3Ex) ->
          runExists (runTransition' runtime thisNotBackable thisNotAutoReturn i3) g3Ex
        Backable    g3@(Graph g3Ex) ->
          runExists (runTransition' runtime thisBackable    thisNotAutoReturn i3) g3Ex
        ForwardOnly g3@(Graph g3Ex) ->
          runExists (runTransition' runtime thisNotBackable thisNotAutoReturn i3) g3Ex
        AutoBack    g3@(Graph g3Ex) ->
          runExists (runTransition' runtime thisBackable    thisAutoReturn    i3) g3Ex

runGraph'
  :: (Monad m, Monad lang)
  => Runner lang m
  -> (Event -> Bool)
  -> Graph lang () ()
  -> m ()
runGraph' runner isBackEv g = runGraph (Runtime runner isBackEv) g

runGraph
  :: (Monad m, Monad lang)
  => Runtime lang m
  -> Graph lang () ()
  -> m ()
runGraph runtime (Graph ex) = do
  _ <- runExists (runTransition' runtime thisNotBackable thisNotAutoReturn ()) ex
  pure ()
