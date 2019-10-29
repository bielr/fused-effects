{-# LANGUAGE DefaultSignatures, EmptyCase, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeOperators #-}

-- | Provides the 'Effect' class that effect types implement.
--
-- @since 1.0.0.0
module Control.Effect.Class
( -- * 'Effect' class
  Threads(..)
  -- * Generic deriving of 'Effect' instances.
, GThreads(..)
) where

import Data.Coerce
import GHC.Generics

  -- | Handle any effects in a signature by threading the algebra’s handler all the way through to the continuation, starting from some initial context.
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
class Functor ctx => Threads ctx sig where
  -- | Handle any effects in a signature by threading the carrier’s state all the way through to the continuation.
  thread
    :: Monad m
    => ctx ()
    -> (forall x . ctx (m x) -> n (ctx x))
    -> sig m a
    -> sig n (ctx a)
  default thread
    :: (Monad m, Generic1 (sig m), Generic1 (sig n), GThreads ctx m n (Rep1 (sig m)) (Rep1 (sig n)))
    => ctx ()                              -- ^ The initial context.
    -> (forall x . ctx (m x) -> n (ctx x)) -- ^ A handler for actions in a context, producing actions with a derived context.
    -> sig m a                             -- ^ The effect to thread the handler through.
    -> sig n (ctx a)
  thread state handler = to1 . gthread state handler . from1
  {-# INLINE thread #-}


-- | Generic implementation of 'Handles'.
class GThreads ctx m m' rep rep' where
  -- | Generic implementation of 'handle'.
  gthread
    :: Monad m
    => ctx ()
    -> (forall x . ctx (m x) -> m' (ctx x))
    -> rep a
    -> rep' (ctx a)

instance GThreads ctx m m' rep rep' => GThreads ctx m m' (M1 i c rep) (M1 i c rep') where
  gthread state handler = M1 . gthread state handler . unM1
  {-# INLINE gthread #-}

instance (GThreads ctx m m' l l', GThreads ctx m m' r r') => GThreads ctx m m' (l :+: r) (l' :+: r') where
  gthread state handler (L1 l) = L1 (gthread state handler l)
  gthread state handler (R1 r) = R1 (gthread state handler r)
  {-# INLINE gthread #-}

instance (GThreads ctx m m' l l', GThreads ctx m m' r r') => GThreads ctx m m' (l :*: r) (l' :*: r') where
  gthread state handler (l :*: r) = gthread state handler l :*: gthread state handler r
  {-# INLINE gthread #-}

instance GThreads ctx m m' V1 V1 where
  gthread _ _ v = case v of {}
  {-# INLINE gthread #-}

instance GThreads ctx m m' U1 U1 where
  gthread _ _ = coerce
  {-# INLINE gthread #-}

instance GThreads ctx m m' (K1 R c) (K1 R c) where
  gthread _ _ = coerce
  {-# INLINE gthread #-}

instance Functor ctx => GThreads ctx m m' Par1 Par1 where
  gthread state _ = Par1 . (<$ state) . unPar1
  {-# INLINE gthread #-}

instance (Functor l, GThreads ctx m m' r r') => GThreads ctx m m' (l :.: r) (l :.: r') where
  gthread state handler = Comp1 . fmap (gthread state handler) . unComp1
  {-# INLINE gthread #-}

instance Functor ctx => GThreads ctx m m' (Rec1 m) (Rec1 m') where
  gthread state handler = Rec1 . handler . (<$ state) . unRec1
  {-# INLINE gthread #-}

instance Threads ctx sig => GThreads ctx m m' (Rec1 (sig m)) (Rec1 (sig m')) where
  gthread state handler = Rec1 . thread state handler . unRec1
  {-# INLINE gthread #-}

