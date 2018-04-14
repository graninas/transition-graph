module FactorioLike.Game where

import           Control.Monad.Free    (Free (..), foldFree, liftF)
import qualified Data.ByteString.Char8 as BS
import           Control.Monad.Trans.State (runStateT)

import           Lib (Graph, runGraph')
import           FactorioLike.Lang
import           FactorioLike.Runtime
import           FactorioLike.Products

type Automation a b = Graph Factory a b

ironOreDrill :: Factory IronOre
ironOreDrill = do
  delay 2
  produce 2

conveyorBelt :: Automation IronOre ()
conveyorBelt = leaf1 tryPushBelt

game :: Automation () ()
game = graph $
  with ironOreDrill
    >~< on "produce" conveyorBelt

runGame :: IO ()
runGame = do
  _ <- runStateT (runGraph' run (== "---------") runtime game) initialState
  pure ()
