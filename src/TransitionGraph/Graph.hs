{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}

{-# LANGUAGE PartialTypeSignatures     #-}

module TransitionGraph.Graph where

import           Control.Monad             (void, when)
import           Control.Monad.Free        (Free (..), foldFree, liftF)
import           Control.Monad.State       (State (..), evalState, execState,
                                            get, put, runState)
import qualified Control.Monad.Trans.State as ST

-- TODO: parametrize this
type Event = String

-- TODO: parametrize by reaction to unknown event.
-- TODO: parametrize by stop condition.

type LangOutput a = (Event, a)

data TransActivation
  = ByEvent Event
  | ByDefault
  | PassThrough

data TransType
  = Backable
  | ForwardOnly
  | AutoBack

data TransitionF lang i next
  = TransitionF TransType TransActivation (Graph lang i) next

-- This Free monad type is used to hold "list of possible transitions".
-- Interpreting of it means matching event with events in transitions.
-- This is definitely an overkill. Using Map will be more effective and
-- intuitive.
type Transitions lang i u = Free (TransitionF lang i) u

data Graph lang i where
  Graph :: (i -> lang (LangOutput o)) -> Transitions lang o () -> Graph lang i

data TransitionTemplate lang i = TransitionTemplate Event (Graph lang i)

instance Functor (TransitionF lang i) where
  fmap f (TransitionF t me g next) = TransitionF t me g (f next)

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
  -> Transitions lang b ()
  -> Graph lang i
with1 = Graph

with
  :: (Monad lang)
  => lang (LangOutput b)
  -> Transitions lang b ()
  -> Graph lang ()
with lang = with1 (const lang)

leaf1
  :: (Monad lang)
  => (i -> lang (LangOutput ()))
  -> Graph lang i
leaf1 langF1 = with1 langF1 (pure ())

leaf
  :: (Monad lang)
  => lang (LangOutput ())
  -> Graph lang ()
leaf = leaf1 . const

graph part = part $ pure ()

-- Implementation tip:
--      on "forward" travel2Graph :: TransitionTemplate
on
  :: Event
  -> Graph lang i
  -> TransitionTemplate lang i
on = TransitionTemplate

transable
  :: (Event -> Graph lang i -> Free (TransitionF lang i) b)
  -> (Free (TransitionF lang i) b -> c)
  -> TransitionTemplate lang i
  -> Free (TransitionF lang i) a
  -> c
transable transition part (TransitionTemplate e g) nextTransitions = part $ do
  r <- transition e g
  nextTransitions
  pure r

defaultTransable
  :: (Graph lang i -> Free (TransitionF lang i) b)
  -> (Free (TransitionF lang i) b -> c)
  -> Graph lang i
  -> Free (TransitionF lang i) a
  -> c
defaultTransable defaultTransition part g nextTransitions = part $ do
  r <- defaultTransition g
  nextTransitions
  pure r

backable
  :: Event
  -> Graph lang i
  -> Transitions lang i ()
backable e g = liftF $ TransitionF Backable (ByEvent e) g ()

forwardOnly
  :: Event
  -> Graph lang i
  -> Transitions lang i ()
forwardOnly e g = liftF $ TransitionF ForwardOnly (ByEvent e) g ()

autoBack
  :: Event
  -> Graph lang i
  -> Transitions lang i ()
autoBack e g = liftF $ TransitionF AutoBack (ByEvent e) g ()

passThrough
  :: Graph lang i
  -> Transitions lang i ()
passThrough g = liftF $ TransitionF ForwardOnly PassThrough g ()

passDefaultForwardOnly
  :: Graph lang i
  -> Transitions lang i ()
passDefaultForwardOnly g = liftF $ TransitionF ForwardOnly ByDefault g ()

passDefaultBackable
  :: Graph lang i
  -> Transitions lang i ()
passDefaultBackable g = liftF $ TransitionF Backable ByDefault g ()
