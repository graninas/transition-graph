{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}

module TransitionGraph.Runtime where

import           Control.Monad               (void, when)
import           Control.Monad.Free          (Free (..), foldFree, liftF)
import           Control.Monad.State         (State (..), evalState, execState,
                                              get, put, runState)
import qualified Control.Monad.Trans.State   as ST

import           TransitionGraph.Graph
import           TransitionGraph.Interpreter

type Runner lang m = forall a. lang (LangOutput a) -> m (LangOutput a)

data Runtime lang m = Runtime
  { runLang_     :: Runner lang m
  , isBackEvent_ :: Event -> Bool
  }

data LangResult a b = GoForward a b | GoBackward
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
  -> Graph lang i
  -> m TransitionResult
runTransition' runtime backable autoReturn i g@(Graph langF ts) = do
  let lang = langF i
  transitionResult <- runTransition runtime autoReturn lang ts
  case transitionResult of
    Done              -> pure Done
    AutoFallbackRerun -> pure FallbackRerun
    FallbackRerun     -> runTransition' runtime backable thisNotAutoReturn i g
    Fallback          ->
      if isBackable backable
      then pure FallbackRerun
      else pure Done -- throw "No fallback"

runTransition
  :: (Monad m, Monad lang)
  => Runtime lang m
  -> AutoReturn
  -> lang (LangOutput o)
  -> Transitions lang o ()
  -> m TransitionResult
runTransition runtime autoReturn lang ts = do
  langResult <- runLang runtime lang
  if isAutoReturn autoReturn
    then pure AutoFallbackRerun
    else case langResult of
      GoBackward                     -> pure Fallback
      GoForward langEvent langOutput -> case selectTransition langEvent ts of
        NotSelected                     -> pure Done
        Selected _ PassThrough subGraph -> runTransition' runtime thisNotBackable thisNotAutoReturn langOutput subGraph
        Selected ForwardOnly _ subGraph -> runTransition' runtime thisNotBackable thisNotAutoReturn langOutput subGraph
        Selected Backable    _ subGraph -> runTransition' runtime thisBackable    thisNotAutoReturn langOutput subGraph
        Selected AutoBack    _ subGraph -> runTransition' runtime thisBackable    thisAutoReturn    langOutput subGraph

runGraph'
  :: (Monad m, Monad lang)
  => Runner lang m
  -> (Event -> Bool)
  -> Graph lang ()
  -> m ()
runGraph' runner isBackEv = runGraph (Runtime runner isBackEv)

runGraph
  :: (Monad m, Monad lang)
  => Runtime lang m
  -> Graph lang ()
  -> m ()
runGraph runtime g = void $ runTransition' runtime thisNotBackable thisNotAutoReturn () g
