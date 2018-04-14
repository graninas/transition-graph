{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}

{-# LANGUAGE PartialTypeSignatures                #-}

module TransitionGraph.Graph where

import           Control.Monad             (void, when)
import           Control.Monad.Free        (Free (..), foldFree, liftF)
import           Control.Monad.State       (State (..), evalState, execState,
                                            get, put, runState)
import qualified Control.Monad.Trans.State as ST

import           Data.Exists

type Event = String

data TransitionF lang b o u
  = Backable    Event (Graph lang b o) u
  | ForwardOnly Event (Graph lang b o) u
  | AutoBack    Event (Graph lang b o) u

-- This Free monad type is used to hold "list of possible transitions".
-- Interpreting of it means matching event with events in transitions.
-- This is definitely an overkill. Using Map will be more effective and
-- intuitive.
type Transitions lang b o u = Free (TransitionF lang b o) u

data GraphF lang i o b
  = GraphF1 (i -> lang b) (Transitions lang b o ())

newtype Graph lang i o
  = Graph (Exists (GraphF lang i o))

data TransitionTemplate lang i o = TransitionTemplate Event (Graph lang i o)

instance Functor (TransitionF lang b o) where
  fmap f (Backable    e g next) = Backable    e g (f next)
  fmap f (ForwardOnly e g next) = ForwardOnly e g (f next)
  fmap f (AutoBack    e g next) = AutoBack    e g (f next)

(<~>) = transable backable
(~>)  = transable forwardOnly
(>~<) = transable autoBack

infixl 3 <~>
infixl 3 ~>
infixl 3 >~<

with1
  :: (Monad lang)
  => (i -> lang b)
  -> Transitions lang b o ()
  -> Graph lang i o
with1 flowF1 table = Graph $ mkExists $ GraphF1 flowF1 table

with
  :: (Monad lang)
  => lang b
  -> Transitions lang b o ()
  -> Graph lang () o
with flow = with1 (const flow)

leaf1
  :: (Monad lang)
  => (i -> lang ())
  -> Graph lang i ()
leaf1 flowF1 = with1 flowF1 (pure ())

leaf
  :: (Monad lang)
  => lang ()
  -> Graph lang () ()
leaf = leaf1 . const

graph part = part $ pure ()

on
  :: Event
  -> Graph lang i o
  -> TransitionTemplate lang i o
on = TransitionTemplate

transable
  :: (Event
      -> Graph lang i o
      -> Free (TransitionF lang i o) b
      )
  -> (Free (TransitionF lang i o) b -> c)
  -> TransitionTemplate lang i o
  -> Free (TransitionF lang i o) a
  -> c

transable transType part (TransitionTemplate e g) = part . transed
  where
    transed prevTrans = do
      prevTrans
      transType e g

backable
  :: Event
  -> Graph lang i o
  -> Transitions lang i o ()
backable e g = liftF $ Backable e g ()

forwardOnly
  :: Event
  -> Graph lang i o
  -> Transitions lang i o ()
forwardOnly e g = liftF $ ForwardOnly e g ()

autoBack
  :: Event
  -> Graph lang i o
  -> Transitions lang i o ()
autoBack e g = liftF $ AutoBack e g ()
