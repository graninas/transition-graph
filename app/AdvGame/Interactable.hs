{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}

module AdvGame.Interactable where

import           Control.Lens              ((.~))
import           Control.Lens.TH           (makeFieldsNoPrefix)
import           Control.Monad             (void, when)
import           Control.Monad.Free        (Free (..), foldFree, liftF)
import           Control.Monad.State       (State (..), evalState, execState,
                                            get, put, runState)
import qualified Control.Monad.Trans.State as ST
import           Data.Either               (Either)
import qualified Data.Map                  as Map

import           AdvGame.Lang


type ObjectModifier a = a -> Maybe a

type OnSuccessAction a = a -> AdventureL ()
type OnFailAction    a = a -> AdventureL ()

data Action a  = Action (ObjectModifier a) (OnSuccessAction a) (OnFailAction a)
type Actions a = Map.Map String (Action a)

data Object a = Object String a (Actions a)
