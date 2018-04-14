module FactorioLike.Products where

import FactorioLike.Lang

data IronOre = IronOre Int

instance Product IronOre where
  mk = IronOre
  stackSize _ = 50
