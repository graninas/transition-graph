{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}

module AdvGame.Lang where

import           Control.Lens.TH           (makeLenses)
import           Control.Monad             (void, when)
import           Control.Monad.Free        (Free (..), foldFree, liftF)
import           Control.Monad.State       (State (..), evalState, execState,
                                            get, put, runState)
import qualified Control.Monad.Trans.State as ST
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Exists
import           Data.Maybe (Maybe (..))
import qualified Data.Map as Map

type Item = String

type ObjectModifier a = a -> Maybe a

type OnSuccessAction a = a -> AdventureL ()
type OnFailAction    a = a -> AdventureL ()

data Action a  = Action (ObjectModifier a) (OnSuccessAction a) (OnFailAction a)
type Actions a = Map.Map String (Action a)

data Object objType objSt = Object
  { _name :: String
  , _objectState :: objSt
  , _actions :: (Actions objSt)
  }

class FromJSON objSt => ToObject objType objSt | objType -> objSt, objSt -> objType where
  object :: objSt -> Object objType objSt

data AdventureLF next where
  GetUserInput :: (String -> next) -> AdventureLF next
  PrintMessage :: String  -> next  -> AdventureLF next
  Put          :: Item    -> next  -> AdventureLF next
  Drop         :: Item    -> next  -> AdventureLF next
  List         ::            next  -> AdventureLF next

  GetObj       :: FromJSON a => String -> (a -> next) -> AdventureLF next
  -- EvalAction   :: String -> Object objType objSt -> AdventureLF next

type AdventureL = Free AdventureLF

instance Functor AdventureLF where
  fmap f (GetUserInput   nextF) = GetUserInput   (f . nextF)
  fmap f (PrintMessage s next)  = PrintMessage s (f next)
  fmap f (Put          s next)  = Put          s (f next)
  fmap f (Drop         s next)  = Drop         s (f next)
  fmap f (List           next)  = List           (f next)

  fmap f (GetObj name    nextF) = GetObj name    (f . nextF)
  -- fmap f (EvalAction act objName next) = EvalAction act objName (f next)

getUserInput :: AdventureL String
getUserInput = liftF $ GetUserInput id

printMessage :: String -> AdventureL ()
printMessage message = liftF $ PrintMessage message ()

put :: String -> AdventureL ()
put s = liftF $ Put s ()

drop :: String -> AdventureL ()
drop s = liftF $ Drop s ()

list :: AdventureL ()
list = liftF $ List ()

-- TODO: move to type literals

getObject
  :: (FromJSON objSt, ToObject objType objSt)
  => String
  -> AdventureL (Object objType objSt)
getObject name = do
  objSt <- liftF $ GetObj name id
  pure $ object objSt

getObject'
  :: (FromJSON objSt, ToObject objType objSt)
  => objType
  -> String
  -> AdventureL (Object objType objSt)
getObject' _ = getObject


makeLenses ''Object
