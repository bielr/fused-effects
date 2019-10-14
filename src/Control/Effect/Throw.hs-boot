{-# LANGUAGE FlexibleInstances, KindSignatures, MultiParamTypeClasses #-}
module Control.Effect.Throw
( Throw(..)
) where

import Control.Effect.Class

data Throw e (m :: * -> *) k
  = Throw e

instance HFunctor (Throw e)
instance Functor f => Handles f (Throw e)
