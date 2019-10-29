{-# LANGUAGE DefaultSignatures, EmptyCase, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}

-- | Provides the 'Effect' class that effect types implement.
--
-- @since 1.0.0.0
module Control.Effect.Class
( -- * 'Effect' class
  Weaves(..)
  -- * Generic deriving of 'Effect' instances.
, GWeaves(..)
) where

import Data.Coerce
import GHC.Generics

class Functor ctx => Weaves ctx sig where
  -- | Handle any effects in a signature by weaveing the algebra’s handler all the way through to the continuation, starting from some initial context.
  --
  -- The handler is expressed as a /distributive law/, and required to adhere to the following laws:
  --
  -- @
  -- handler . 'fmap' 'pure' = 'pure'
  -- @
  -- @
  -- handler . 'fmap' (k '=<<') = handler . 'fmap' k 'Control.Monad.<=<' handler
  -- @
  --
  -- respectively expressing that the handler does not alter the context of pure computations, and that the handler distributes over monadic composition.
  weave
    :: Monad m
    => ctx ()
    -> (forall x . ctx (m x) -> n (ctx x))
    -> sig m a
    -> sig n (ctx a)
  default weave
    :: (Monad m, Generic1 (sig m), Generic1 (sig n), GWeaves ctx m n (Rep1 (sig m)) (Rep1 (sig n)))
    => ctx ()                              -- ^ The initial context.
    -> (forall x . ctx (m x) -> n (ctx x)) -- ^ A handler for actions in a context, producing actions with a derived context.
    -> sig m a                             -- ^ The effect to weave the handler through.
    -> sig n (ctx a)
  weave state handler = to1 . gweave state handler . from1
  {-# INLINE weave #-}


-- | Generic implementation of 'Handles'.
class GWeaves ctx m m' rep rep' where
  -- | Generic implementation of 'handle'.
  gweave
    :: Monad m
    => ctx ()
    -> (forall x . ctx (m x) -> m' (ctx x))
    -> rep a
    -> rep' (ctx a)

instance GWeaves ctx m m' rep rep' => GWeaves ctx m m' (M1 i c rep) (M1 i c rep') where
  gweave state handler = M1 . gweave state handler . unM1
  {-# INLINE gweave #-}

instance (GWeaves ctx m m' l l', GWeaves ctx m m' r r') => GWeaves ctx m m' (l :+: r) (l' :+: r') where
  gweave state handler (L1 l) = L1 (gweave state handler l)
  gweave state handler (R1 r) = R1 (gweave state handler r)
  {-# INLINE gweave #-}

instance (GWeaves ctx m m' l l', GWeaves ctx m m' r r') => GWeaves ctx m m' (l :*: r) (l' :*: r') where
  gweave state handler (l :*: r) = gweave state handler l :*: gweave state handler r
  {-# INLINE gweave #-}

instance GWeaves ctx m m' V1 V1 where
  gweave _ _ v = case v of {}
  {-# INLINE gweave #-}

instance GWeaves ctx m m' U1 U1 where
  gweave _ _ = coerce
  {-# INLINE gweave #-}

instance GWeaves ctx m m' (K1 R c) (K1 R c) where
  gweave _ _ = coerce
  {-# INLINE gweave #-}

instance Functor ctx => GWeaves ctx m m' Par1 Par1 where
  gweave state _ = Par1 . (<$ state) . unPar1
  {-# INLINE gweave #-}

instance (Functor l, GWeaves ctx m m' r r') => GWeaves ctx m m' (l :.: r) (l :.: r') where
  gweave state handler = Comp1 . fmap (gweave state handler) . unComp1
  {-# INLINE gweave #-}

instance Functor ctx => GWeaves ctx m m' (Rec1 m) (Rec1 m') where
  gweave state handler = Rec1 . handler . (<$ state) . unRec1
  {-# INLINE gweave #-}

instance Weaves ctx sig => GWeaves ctx m m' (Rec1 (sig m)) (Rec1 (sig m')) where
  gweave state handler = Rec1 . weave state handler . unRec1
  {-# INLINE gweave #-}

