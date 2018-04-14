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

data TrackResult a
  = BackTrack a
  | ForwardTrack a
  | AutoBackTrack a
  | Nop

type Interpreter a b = State (TrackResult a) b

interpretTransition
  :: Event
  -> TransitionF lang b o u
  -> Interpreter (Graph lang b o) u
interpretTransition e (Backable expectedE g next) = do
  when (e == expectedE) (put $ BackTrack g)
  pure next
interpretTransition e (ForwardOnly expectedE g next) = do
  when (e == expectedE) (put $ ForwardTrack g)
  pure next
interpretTransition e (AutoBack expectedE g next) = do
  when (e == expectedE) (put $ AutoBackTrack g)
  pure next

runTransitions
  :: Event
  -> Transitions lang b o s
  -> Interpreter (Graph lang b o) s
runTransitions e = foldFree (interpretTransition e)

matchTransition
  :: Event
  -> GraphF lang i o b
  -> TrackResult (Graph lang b o)
matchTransition e (GraphF1 _ t) = execState (runTransitions e t) Nop
