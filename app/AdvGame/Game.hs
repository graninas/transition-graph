module AdvGame.Game where

import           Control.Monad             (unless, void, when)
import           Control.Monad.Free        (Free (..), foldFree, liftF)
import           Control.Monad.Trans.State (evalStateT)
import qualified Data.ByteString.Char8     as BS
import qualified Data.Map                  as Map

import           AdvGame.Lang
import           AdvGame.Objects
import           AdvGame.Runtime           (run)
import           AdvGame.Init              (inititalState)
import           Lib

type AGGraph a b = Graph AdventureL a b

getInput :: AdventureL (Event, ())
getInput = do
  userInput <- getUserInput
  pure (userInput, ())

nop :: AdventureL (Event, ())
nop = pure ("", ())


-- mailboxOpened :: AGGraph () ()
-- mailboxOpened = graph $
--   with (westOfHouse  >> getInput)
--     <~> on "forward" travel3Graph

westOfHouse :: AGGraph (Bool, Bool) ()
westOfHouse = graph $
  with1 (\x -> westOfHouse' x >> getInput)
    -- ~> on "open mailbox" mailboxOpened
    /> leaf nop

westOfHouse' :: (Bool, Bool) -> AdventureL ()
westOfHouse' (showDescr, showMailbox) = do
  mailbox :: Mailbox <- getObject "mailbox"
  printMessage "West of House"
  when showDescr   $ printMessage "This is an open field west of a white house, with a boarded front door."
  when showMailbox $ printMessage $ describeObject mailbox
  when showDescr   $ printMessage "A rubber mat saying 'Welcome to Zork!' lies by the door."

game :: AGGraph () ()
game = graph $
  with (pure ("", (True, True)))
    -/> westOfHouse

runGame :: IO ()
runGame = evalStateT (runGraph' run (== "back") game) inititalState
