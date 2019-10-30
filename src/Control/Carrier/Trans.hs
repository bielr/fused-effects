{-# language DataKinds, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Trans where

import Control.Effect.Class
import Control.Monad.IO.Class
import Data.Functor.Identity


newtype ComposeC
    (t :: (* -> *) -> * -> *)
    (t' :: (* -> *) -> * -> *)
    (m :: * -> *)
    a
    = ComposeC { runComposeC :: t (t' m) a }
  deriving (Functor, Applicative, Monad, MonadIO)


-- instance (HFunctor t, HFunctor t') => Weaves Identity (ComposeC t t') where
--   weave ctx hdl (ComposeC m) = ComposeC $ fmap _ $ _ m
