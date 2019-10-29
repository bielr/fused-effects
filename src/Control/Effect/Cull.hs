{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving #-}
{- | Provides an effect to cull choices in a given nondeterministic context. This effect is used in concert with 'Control.Effect.NonDet.NonDet'.

Computations run inside a call to 'cull' will return at most one result.

Predefined carriers:

* "Control.Carrier.Cull.Church"

@since 0.1.2.0
-}
module Control.Effect.Cull
( -- * Cull effect
  Cull(..)
, cull
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra

-- | 'Cull' effects are used with 'Choose' to provide control over branching.
--
-- @since 0.1.2.0
data Cull m k
  = forall a . Cull (m a) (a -> m k)

deriving instance Functor m => Functor (Cull m)

instance Functor ctx => Threads ctx Cull where
  thread ctx handler (Cull m k) = Cull (handler (m <$ ctx)) (handler . fmap k)
  {-# INLINE thread #-}

-- | Cull nondeterminism in the argument, returning at most one result.
--
-- @
-- 'cull' ('pure' a 'Control.Effect.Choose.<|>' m) 'Control.Effect.Choose.<|>' n = 'pure' a 'Control.Effect.Choose.<|>' n
-- @
--
-- @since 0.1.2.0
cull :: Has Cull sig m => m a -> m a
cull m = send (Cull m pure)
