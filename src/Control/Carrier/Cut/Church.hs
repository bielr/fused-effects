{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}

-- | A carrier for 'Cut' and 'NonDet' effects used in tandem (@Cut :+: NonDet@).
module Control.Carrier.Cut.Church
( -- * Cut effect
  module Control.Effect.Cut
  -- * NonDet effects
, module Control.Effect.NonDet
  -- * Cut carrier
, runCut
, runCutA
, runCutM
, CutC(..)
  -- * Re-exports
, Carrier
, run
) where

import Control.Carrier
import Control.Effect.Cut
import Control.Effect.NonDet
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Run a 'Cut' effect with the supplied continuations for 'pure' / '<|>', 'empty', and 'cutfail'.
--
--   prop> run (runCut (fmap . (:)) (pure []) (pure []) (pure a)) === [a]
--
-- @since 1.0.0.0
runCut :: (a -> m b -> m b) -> m b -> m b -> CutC m a -> m b
runCut cons nil fail m = runCutC m cons nil fail

-- | Run a 'Cut' effect, returning all its results in an 'Alternative' collection.
--
-- @since 1.0.0.0
runCutA :: (Alternative f, Applicative m) => CutC m a -> m (f a)
runCutA = runCut (fmap . (<|>) . pure) (pure empty) (pure empty)

-- | Run a 'Cut' effect, modeling choice and failure with '<>' and 'mempty' and embedding results with the passed function.
--
-- @since 1.0.0.0
runCutM :: (Applicative m, Monoid b) => (a -> b) -> CutC m a -> m b
runCutM leaf = runCut (fmap . mappend . leaf) (pure mempty) (pure mempty)

-- | @since 1.0.0.0
newtype CutC m a = CutC
  { -- | A higher-order function receiving three parameters: a function to combine each solution with the rest of the solutions, an action to run when no results are produced (e.g. on 'empty'), and an action to run when no results are produced and backtrcking should not be attempted (e.g. on 'cutfail').
    runCutC :: forall b . (a -> m b -> m b) -> m b -> m b -> m b
  }
  deriving (Functor)

instance Applicative (CutC m) where
  pure a = CutC (\ cons nil _ -> cons a nil)
  {-# INLINE pure #-}
  CutC f <*> CutC a = CutC $ \ cons nil fail ->
    f (\ f' fs -> a (cons . f') fs fail) nil fail
  {-# INLINE (<*>) #-}

instance Alternative (CutC m) where
  empty = CutC (\ _ nil _ -> nil)
  {-# INLINE empty #-}
  CutC l <|> CutC r = CutC (\ cons nil fail -> l cons (r cons nil fail) fail)
  {-# INLINE (<|>) #-}

instance Monad (CutC m) where
  CutC a >>= f = CutC $ \ cons nil fail ->
    a (\ a' as -> runCutC (f a') cons as fail) nil fail
  {-# INLINE (>>=) #-}

instance Fail.MonadFail m => Fail.MonadFail (CutC m) where
  fail s = lift (Fail.fail s)
  {-# INLINE fail #-}

-- | Separate fixpoints are computed for each branch.
--
-- >>> run (runCutA @[] (take 3 <$> mfix (\ as -> pure (0 : map succ as) <|> pure (0 : map pred as))))
-- [[0,1,2],[0,-1,-2]]
instance MonadFix m => MonadFix (CutC m) where
  mfix f = CutC $ \ cons nil fail ->
    mfix (runCutA . f . head)
    >>= runCut cons nil fail . foldr
      (\ a _ -> pure a <|> mfix (liftAll . fmap tail . runCutA . f))
      empty where
    liftAll m = CutC $ \ cons nil _ -> m >>= foldr cons nil
  {-# INLINE mfix #-}

instance MonadIO m => MonadIO (CutC m) where
  liftIO io = lift (liftIO io)
  {-# INLINE liftIO #-}

instance MonadPlus (CutC m)

instance MonadTrans CutC where
  lift m = CutC (\ cons nil _ -> m >>= flip cons nil)
  {-# INLINE lift #-}

instance (Carrier sig m, Handles [] sig) => Carrier (Cut :+: NonDet :+: sig) (CutC m) where
  eff (L Cutfail)    = CutC $ \ _    _   fail -> fail
  eff (L (Call m k)) = CutC $ \ cons nil fail -> runCutC m (\ a as -> runCutC (k a) cons as fail) nil nil
  eff (R (L (L Empty)))      = empty
  eff (R (L (R (Choose k)))) = k True <|> k False
  eff (R (R other))          = CutC $ \ cons nil _ -> eff (handle [()] (fmap concat . traverse runCutA) other) >>= foldr cons nil
  {-# INLINE eff #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> :seti -XTypeApplications
-- >>> import Test.QuickCheck
