{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances #-}

-- | A carrier for 'Cull' and 'NonDet' effects used in tandem (@Cull :+: NonDet@).
module Control.Carrier.Cull.Church
( -- * Cull carrier
  runCull
, runCullA
, runCullM
, CullC(..)
  -- * Cull effect
, module Control.Effect.Cull
  -- * NonDet effects
, module Control.Effect.NonDet
) where

import Control.Applicative (liftA2)
import Control.Carrier
import Control.Carrier.NonDet.Church
import Control.Carrier.Reader
import Control.Effect.Cull
import Control.Effect.NonDet
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Run a 'Cull' effect with the supplied continuations for '<|>', 'pure', and 'empty'. Branches outside of any 'cull' block will not be pruned.
--
-- @since 1.0.0.0
runCull :: (m b -> m b -> m b) -> (a -> m b) -> m b -> CullC m a -> m b
runCull fork leaf nil = runNonDet fork leaf nil . runReader False . runCullC

-- | Run a 'Cull' effect, interpreting the result into an 'Alternative' functor. Choice is handled with '<|>', embedding with 'pure', and failure with 'empty'.
--
-- @since 1.0.0.0
runCullA :: (Alternative f, Applicative m) => CullC m a -> m (f a)
runCullA = runCull (liftA2 (<|>)) (pure . pure) (pure empty)

-- | Run a 'Cull' effect, mapping the result into a 'Monoid'.
--
-- @since 1.0.0.0
runCullM :: (Applicative m, Monoid b) => (a -> b) -> CullC m a -> m b
runCullM leaf = runCull (liftA2 mappend) (pure . leaf) (pure mempty)

-- | @since 1.0.0.0
newtype CullC m a = CullC { runCullC :: ReaderC Bool (NonDetC m) a }
  deriving (Applicative, Functor, Monad, Fail.MonadFail, MonadIO)

instance Alternative (CullC m) where
  empty = CullC empty
  {-# INLINE empty #-}
  l <|> r = CullC $ ReaderC $ \ cull ->
    if cull then
      NonDetC $ \ fork leaf nil ->
        runNonDetC (runReader cull (runCullC l)) fork leaf (runNonDetC (runReader cull (runCullC r)) fork leaf nil)
    else
      runReader cull (runCullC l) <|> runReader cull (runCullC r)
  {-# INLINE (<|>) #-}

-- | Separate fixpoints are computed for each branch.
deriving instance MonadFix m => MonadFix (CullC m)

instance MonadPlus (CullC m)

instance MonadTrans CullC where
  lift = CullC . lift . lift
  {-# INLINE lift #-}

instance (Carrier sig m, Handles BinaryTree sig) => Carrier (Cull :+: NonDet :+: sig) (CullC m) where
  eff (L (Cull m k))         = CullC (local (const True) (runCullC m)) >>= k
  eff (R (L (L Empty)))      = empty
  eff (R (L (R (Choose k)))) = k True <|> k False
  eff (R (R other))          = CullC (eff (R (R (handleCoercible other))))
  {-# INLINE eff #-}
