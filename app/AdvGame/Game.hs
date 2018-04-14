module AdvGame.Game where

import           Control.Monad.Free    (Free (..), foldFree, liftF)
import qualified Data.ByteString.Char8 as BS
import           Control.Monad.Trans.State (runStateT)

import           Lib as Lib
import           AdvGame.Lang
import           AdvGame.AdvGameRuntime

type AGGraph a b = Lib.Graph AdventureL a b

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

travel2Graph :: AGGraph () ()
travel2Graph = graph $
  with (location1 >> getInput)
    <~> on "forward" travel3Graph
    -- >~< on "list"    (leaf list)

travel1Graph :: AGGraph () ()
travel1Graph = graph $
  with (location1 >> getInput)
    <~> on "forward" travel2Graph
    -- >~< on "list"    (leaf list)

location1 :: AdventureL ()
location1 = location "West of House\n\
  \This is an open field west of a white house, with a boarded front door.\n\
  \There is a small mailbox here.\n\
  \A rubber mat saying 'Welcome to Zork!' lies by the door."

location2 :: AdventureL ()
location2 = location "Another location."

location3 :: AdventureL ()
location3 = location "Location #3."

location :: String -> AdventureL ()
location = printS

runGame :: IO ()
runGame = do
  let runtime = Runtime run (== "back")
  _ <- runStateT (runGraph runtime travel1Graph) initialAdvGameRuntime
  pure ()
