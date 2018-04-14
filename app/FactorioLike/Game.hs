module FactorioLike.Game where

import           Control.Monad.Free    (Free (..), foldFree, liftF)
import qualified Data.ByteString.Char8 as BS
import           Control.Monad.Trans.State (runStateT)

import           Lib (Graph, runGraph')
import           FactorioLike.Lang
import           FactorioLike.Runtime

type FLGraph a b = Graph AdventureL a b

-- getInput :: AdventureL (Event, ())
-- getInput = do
--   userInput <- getUserInput
--   pure (userInput, ())
--
-- nop :: AdventureL (Event, ())
-- nop = pure ("", ())


ironOreDrill :: FactorioLike IronOre
ironOreDrill = do
  delay 2
  produce 2

conveyorBelt :: FLGraph IronOre ()
conveyorBelt = leaf1 tryPushBelt

game :: FLGraph () ()
game = graph $
  with ironOreDrill
    >~< on "produce" conveyorBelt

runGame :: IO ()
runGame = do
  _ <- runStateT (runGraph' run (== "---------") runtime game) initialFactorioLikeGame
  pure ()
