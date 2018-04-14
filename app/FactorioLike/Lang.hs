{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}

module FactorioLike.Lang where

import           Control.Monad.Free        (Free (..), foldFree, liftF)

class Product p where
  mk :: Int -> p
  stackSize :: p -> Int

data FactoryF next where
  Delay   :: Int                    -> next  -> FactoryF next
  Produce :: Product p => Int -> (p -> next) -> FactoryF next

type Factory' = Free FactoryF

instance Functor FactoryF where
  fmap f (Delay   d next)  = Delay   d (f next)
  fmap f (Produce i nextF) = Produce i (f . nextF)


delay :: Int -> Factory ()
delay i = liftF $ Delay i ()

produce :: Product p => Int -> Factory p
produce i = liftF $ Produce i id
