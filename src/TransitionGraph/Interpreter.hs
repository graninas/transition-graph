{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}

module TransitionGraph.Interpreter where

import           Control.Monad         (void, when)
import           Control.Monad.Free    (Free (..), foldFree, liftF)
import           Control.Monad.State   (State (..), evalState, execState, get,
                                        put, runState)

import           Data.Exists

import           TransitionGraph.Graph

data SelectedTransition lang i where
  Selected    :: TransType -> TransActivation -> Graph lang i -> SelectedTransition lang i
  NotSelected :: SelectedTransition lang i

interpret
  :: Event
  -> TransitionF lang i s
  -> State (SelectedTransition lang i) s

interpret currentEvent (TransitionF tt act@(ByEvent matchEvent) g next) = do
  selected <- get
  case selected of
    NotSelected            -> trySelect
    Selected _ ByDefault _ -> trySelect
    _                      -> pure next
  where
    trySelect = do
      when (matchEvent == currentEvent) (put $ Selected tt act g)
      pure next

interpret currentEvent (TransitionF tt PassThrough g next) = do
  put $ Selected tt PassThrough g
  pure next

interpret currentEvent (TransitionF tt ByDefault g next) = do
  selected <- get
  case selected of
    NotSelected -> put (Selected tt ByDefault g) >> pure next
    _           -> pure next

runTransitions
  :: Event
  -> Transitions lang i s
  -> State (SelectedTransition lang i) s
runTransitions e = foldFree (interpret e)

selectTransition
  :: Event
  -> Transitions lang i s
  -> SelectedTransition lang i
selectTransition e ts = execState (runTransitions e ts) NotSelected
