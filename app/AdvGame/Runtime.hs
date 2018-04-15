module AdvGame.Runtime where

import           Control.Monad (mapM)
import           Control.Monad.Trans.Class (lift)
import qualified Data.Map as Map
import           Control.Monad.Free    (Free (..), foldFree, liftF)
import           Control.Monad.Trans.State (StateT, get, put)

import           AdvGame.Lang
import           Lib (Event)

data Runtime = Runtime
  { _inventory :: Map.Map String Item
  }

type Interpreter a = StateT Runtime IO a

interpret :: AdventureLF s -> Interpreter s
interpret (GetUserInput nextF) = do
  lift $ print "> "
  input <- lift getLine
  pure $ nextF input
interpret (PrintS s next)  = do
  lift $ putStrLn s
  pure next
interpret (Put s next)     = error "Not implemented."
interpret (Drop s next)    = error "Not implemented."
interpret (List next)      = do
  Runtime inv <- get
  mapM (lift . putStrLn . snd) $ Map.toList inv
  pure next

run :: AdventureL (Event, s) -> Interpreter (Event, s)
run l = foldFree interpret l

inititalState :: Runtime
inititalState = Runtime Map.empty
