{-# LANGUAGE DefaultSignatures, EmptyCase, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}

-- | Provides the 'Effect' class that effect types implement.
--
-- @since 1.0.0.0
module Control.Effect.Class
( -- * 'Effect' class
  Weaves(..)
, hmap
  -- * Generic deriving of 'Effect' instances.
, GWeaves(..)
) where

import Data.Coerce
import Data.Functor.Identity
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
    :: (Monad m, Monad n)
    => ctx ()
    -> (forall x . ctx (m x) -> n (ctx x))
    -> sig m a
    -> sig n (ctx a)
  default weave
    :: (Monad m, Monad n, Generic1 (sig m), Generic1 (sig n), GWeaves ctx m n (Rep1 (sig m)) (Rep1 (sig n)))
    => ctx ()                              -- ^ The initial context.
    -> (forall x . ctx (m x) -> n (ctx x)) -- ^ A handler for actions in a context, producing actions with a derived context.
    -> sig m a                             -- ^ The effect to weave the handler through.
    -> sig n (ctx a)
  weave ctx handler = to1 . gweave ctx handler . from1
  {-# INLINE weave #-}


hmap :: (Monad m, Monad n, Functor (sig n), Weaves Identity sig) => (forall x. m x -> n x) -> sig m a -> sig n a
hmap f = fmap runIdentity . weave (Identity ()) (fmap Identity . f . runIdentity)


-- | Generic implementation of 'Weaves'.
class GWeaves ctx m m' rep rep' where
  -- | Generic implementation of 'handle'.
  gweave
    :: (Monad m, Monad m')
    => ctx ()
    -> (forall x . ctx (m x) -> m' (ctx x))
    -> rep a
    -> rep' (ctx a)

instance GWeaves ctx m m' rep rep' => GWeaves ctx m m' (M1 i c rep) (M1 i c rep') where
  gweave ctx handler = M1 . gweave ctx handler . unM1
  {-# INLINE gweave #-}

instance (GWeaves ctx m m' l l', GWeaves ctx m m' r r') => GWeaves ctx m m' (l :+: r) (l' :+: r') where
  gweave ctx handler (L1 l) = L1 (gweave ctx handler l)
  gweave ctx handler (R1 r) = R1 (gweave ctx handler r)
  {-# INLINE gweave #-}

instance (GWeaves ctx m m' l l', GWeaves ctx m m' r r') => GWeaves ctx m m' (l :*: r) (l' :*: r') where
  gweave ctx handler (l :*: r) = gweave ctx handler l :*: gweave ctx handler r
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
  gweave ctx _ = Par1 . (<$ ctx) . unPar1
  {-# INLINE gweave #-}

instance (Functor l, GWeaves ctx m m' r r') => GWeaves ctx m m' (l :.: r) (l :.: r') where
  gweave ctx handler = Comp1 . fmap (gweave ctx handler) . unComp1
  {-# INLINE gweave #-}

instance Functor ctx => GWeaves ctx m m' (Rec1 m) (Rec1 m') where
  gweave ctx handler = Rec1 . handler . (<$ ctx) . unRec1
  {-# INLINE gweave #-}

instance Weaves ctx sig => GWeaves ctx m m' (Rec1 (sig m)) (Rec1 (sig m')) where
  gweave ctx handler = Rec1 . weave ctx handler . unRec1
  {-# INLINE gweave #-}

