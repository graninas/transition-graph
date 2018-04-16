
module AdvGame.Interaction where

import           Control.Lens    ((.~), (^.))
import           Control.Lens.TH (makeClassy, makeFieldsNoPrefix, makeLenses)
import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Either     (Either)
import qualified Data.Map        as Map
import           Data.Maybe      (Maybe (..))
import           GHC.Generics    (Generic)

import           Lib (Event)

import           AdvGame.Lang
import           AdvGame.Container

getInput :: AdventureL (Event, ())
getInput = do
  userInput <- getUserInput
  pure (userInput, ())

nop :: AdventureL (Event, ())
nop = pure ("", ())

inputOnly :: a -> AdventureL (Event, a)
inputOnly a = pure ("", a)

evalAction
  :: (FromJSON objSt, ToObject objType objSt)
  => objType
  -> String
  -> String
  -> AdventureL (Event, ())
evalAction objType act objName = do
  obj <- getObject' objType objName
  nop
