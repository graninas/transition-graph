{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}

module TransitionGraph.Interpreter where

import           Control.Monad             (void, when)
import           Control.Monad.Free        (Free (..), foldFree, liftF)
import           Control.Monad.State       (State (..), evalState, execState,
                                            get, put, runState)

import           Data.Exists

import           TransitionGraph.Graph

type Interpreter graph b = State (TransitionDef graph) b

interpret
  :: Event
  -> TransitionF lang b o u
  -> Interpreter (Graph lang b o) u
interpret currentEvent (Transition matchEvent transDef next) = do
  when (matchEvent == currentEvent) (put transDef)
  pure next

runTransitions
  :: Event
  -> Transitions lang b o s
  -> Interpreter (Graph lang b o) s
runTransitions e = foldFree (interpret e)

matchTransition
  :: Event
  -> GraphF lang i o b
  -> TransitionDef (Graph lang b o)
matchTransition e (GraphF1 _ t) = execState (runTransitions e t) Nop
