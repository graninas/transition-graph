{-# LANGUAGE TemplateHaskell #-}

module AdvGame.Container where

import           Control.Lens    ((.~), (^.))
import           Control.Lens.TH (makeClassy, makeFieldsNoPrefix, makeLenses)
import           Data.Either     (Either)
import qualified Data.Map        as Map
import           Data.Maybe      (Maybe (..))

import           AdvGame.Lang

data ContainerState = Opened | Closed

data Container = Container
  { _state :: ContainerState
  , _items :: [Item]
  }

makeLenses ''Container
