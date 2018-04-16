{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}

{-# LANGUAGE PartialTypeSignatures     #-}

module TransitionGraph.Graph where

import           Control.Monad             (void, when)
import           Control.Monad.Free        (Free (..), foldFree, liftF)
import           Control.Monad.State       (State (..), evalState, execState,
                                            get, put, runState)
import qualified Control.Monad.Trans.State as ST

import           Data.Exists

-- TODO: parametrize this
type Event = String

-- TODO: parametrize by reaction to unknown event.
-- TODO: parametrize by stop condition.

type LangOutput a = (Event, a)

data TransitionDef graph
  = Backable    graph
  | ForwardOnly graph
  | AutoBack    graph
  | PassThrough graph
  | PassDefaultForwardOnly graph
  | PassDefaultBackable    graph
  | NoTransition

data TransitionF lang b o next
  = Transition Event (TransitionDef  (Graph lang b o)) next
  | PassThroughTransition            (Graph lang b o)  next
  | PassDefaultForwardOnlyTransition (Graph lang b o)  next
  | PassDefaultBackableTransition    (Graph lang b o)  next

-- This Free monad type is used to hold "list of possible transitions".
-- Interpreting of it means matching event with events in transitions.
-- This is definitely an overkill. Using Map will be more effective and
-- intuitive.
type Transitions lang b o u = Free (TransitionF lang b o) u

data GraphF lang i o b
  = GraphF1 (i -> lang (LangOutput b)) (Transitions lang b o ())

newtype Graph lang i o
  = Graph (Exists (GraphF lang i o))

data TransitionTemplate lang i o = TransitionTemplate Event (Graph lang i o)

instance Functor (TransitionF lang b o) where
  fmap f (Transition              e transDef next) = Transition              e transDef (f next)
  fmap f (PassThroughTransition   g          next) = PassThroughTransition   g          (f next)
  fmap f (PassDefaultForwardOnlyTransition g next) = PassDefaultForwardOnlyTransition  g (f next)
  fmap f (PassDefaultBackableTransition    g next) = PassDefaultBackableTransition     g (f next)

(<~>) = transable backable
(~>)  = transable forwardOnly
(>~<) = transable autoBack
(-/>) = defaultTransable passThrough
(/>)  = defaultTransable passDefaultForwardOnly
(</>) = defaultTransable passDefaultBackable

infixl 3 <~>
infixl 3 ~>
infixl 3 >~<
infixl 3 -/>
infixl 3 />
infixl 3 </>

with1
  :: (Monad lang)
  => (i -> lang (LangOutput b))
  -> Transitions lang b o ()
  -> Graph lang i o
with1 langF1 table = Graph $ mkExists $ GraphF1 langF1 table

with
  :: (Monad lang)
  => lang (LangOutput b)
  -> Transitions lang b o ()
  -> Graph lang () o
with lang = with1 (const lang)

leaf1
  :: (Monad lang)
  => (i -> lang (LangOutput ()))
  -> Graph lang i ()
leaf1 langF1 = with1 langF1 (pure ())

leaf
  :: (Monad lang)
  => lang (LangOutput ())
  -> Graph lang () ()
leaf = leaf1 . const

graph part = part $ pure ()

-- Implementation tip:
--      on "forward" travel2Graph :: TransitionTemplate
on
  :: Event
  -> Graph lang i o
  -> TransitionTemplate lang i o
on = TransitionTemplate

transable
  :: (Event -> Graph lang i o -> Free (TransitionF lang i o) b)
  -> (Free (TransitionF lang i o) b -> c)
  -> TransitionTemplate lang i o
  -> Free (TransitionF lang i o) a
  -> c
transable transition part (TransitionTemplate e g) nextTransitions = part $ do
  r <- transition e g
  nextTransitions
  pure r

defaultTransable
  :: (Graph lang i o -> Free (TransitionF lang i o) b)
  -> (Free (TransitionF lang i o) b -> c)
  -> Graph lang i o
  -> Free (TransitionF lang i o) a
  -> c
defaultTransable defaultTransition part g nextTransitions = part $ do
  r <- defaultTransition g
  nextTransitions
  pure r

backable
  :: Event
  -> Graph lang i o
  -> Transitions lang i o ()
backable e g = liftF $ Transition e (Backable g) ()

forwardOnly
  :: Event
  -> Graph lang i o
  -> Transitions lang i o ()
forwardOnly e g = liftF $ Transition e (ForwardOnly g) ()

autoBack
  :: Event
  -> Graph lang i o
  -> Transitions lang i o ()
autoBack e g = liftF $ Transition e (AutoBack g) ()

passThrough
  :: Graph lang i o
  -> Transitions lang i o ()
passThrough g = liftF $ PassThroughTransition g ()

passDefaultForwardOnly
  :: Graph lang i o
  -> Transitions lang i o ()
passDefaultForwardOnly g = liftF $ PassDefaultForwardOnlyTransition g ()

passDefaultBackable
  :: Graph lang i o
  -> Transitions lang i o ()
passDefaultBackable g = liftF $ PassDefaultBackableTransition g ()
