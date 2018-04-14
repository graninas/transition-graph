module Locations.Upperground where

import           Control.Monad.Free    (Free (..), foldFree, liftF)
import qualified Data.ByteString.Char8 as BS
import           Control.Monad.Trans.State (runStateT)

import           Lib as Lib
import           AdvGame.Lang

mailboxClosed = object "mailbox_closed" $ do
  name "mailbox"
  description "There is a small mailbox here."
  action "open" $ do
    description "Opening the small mailbox reveals a leaflet."
    mailboxOpened

westOfHouse :: AdventureL ()
westOfHouse = location $ do
  title "West of House"
  description "This is an open field west of a white house, with a boarded front door."

  object "mat" $ do
    description "A rubber mat saying 'Welcome to Zork!' lies by the door."

location2 :: AdventureL ()
location2 = location "Another location."

location3 :: AdventureL ()
location3 = location "Location #3."
