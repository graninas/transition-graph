{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}

module AdvGame.Interactable where

import qualified Data.Map as Map
import           Control.Monad             (void, when)
import           Control.Monad.Free        (Free (..), foldFree, liftF)
import           Control.Monad.State       (State (..), evalState, execState,
                                            get, put, runState)
import qualified Control.Monad.Trans.State as ST

import           Data.Exists

type Item = String

data ContainerState = Opened | Closed

type ObjectActions a = Map.Map String (Object a -> Object a)

data Object a = Object String a (ObjectActions a)
