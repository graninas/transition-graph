module AdvGame.Game where

import qualified Data.Map as Map
import           Control.Monad.Free    (Free (..), foldFree, liftF)
import qualified Data.ByteString.Char8 as BS
import           Control.Monad.Trans.State (runStateT)

import           Lib
import           AdvGame.Lang
import           AdvGame.Runtime (run, inititalState)
import           AdvGame.Object

type AGGraph a b = Graph AdventureL a b

getInput :: AdventureL (Event, ())
getInput = do
  userInput <- getUserInput
  pure (userInput, ())

nop :: AdventureL (Event, ())
nop = pure ("", ())


travel3Graph :: AGGraph () ()
travel3Graph = graph $
  with (location1 >> getInput)
    <~> on "forward" (leaf nop)
    -- >~< on "list"    (leaf list)

mailboxOpened :: AGGraph () ()
mailboxOpened = graph $
  with (westOfHouse  >> getInput)
    <~> on "forward" travel3Graph


data Mailbox = Mailbox
  { _containerState :: ContainerState
  , _items          :: [Item]
  }

mkMailbox :: Object Mailbox
mkMailbox = Object "mailbox" (Mailbox Closed ["leaflet"]) $ Map.fromList
  [ ("open mailbox",
  ]

westOfHouse :: AGGraph () ()
westOfHouse = graph $
  with westOfHouse'
    ~> on "open mailbox" mailboxOpened

game :: AGGraph () (Bool, Object )
game = graph $ pure (True, mkMailbox) --> westOfHouse

westOfHouse' :: AdventureL ()
westOfHouse' mailbox = do
  printS "West of House\n"
--  \This is an open field west of a white house, with a boarded front door.\n\
--  \There is a small mailbox here.\n\
--  \A rubber mat saying 'Welcome to Zork!' lies by the door."

runGame :: IO ()
runGame = do
  let
  evalStateT (runGraph' run (== "back") game) inititalState
