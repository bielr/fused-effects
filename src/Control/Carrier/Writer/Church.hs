{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- | A high-performance, strict, church-encoded carrier for 'Writer'.

This carrier issues left-associated 'mappend's, meaning that 'Monoid's such as @[]@ with poor performance for left-associated 'mappend's are ill-suited for use with this carrier. Alternatives such as 'Data.Monoid.Endo', @Seq@, or @DList@ may be preferred.

@since 1.1.0.0
-}
module Control.Carrier.Writer.Church
( -- * Writer carrier
  runWriter
, execWriter
, WriterC(WriterC)
  -- * Writer effect
, module Control.Effect.Writer
) where

import Control.Algebra
import Control.Applicative (Alternative)
import Control.Carrier.State.Church
import Control.Effect.Writer
import Control.Monad (MonadPlus)
import Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Run a 'Writer' effect with a 'Monoid'al log, applying a continuation to the final log and result.
--
-- @
-- 'runWriter' k ('pure' a) = k 'mempty' a
-- @
-- @
-- 'runWriter' k ('tell' w) = k w ()
-- @
-- @
-- 'runWriter' k ('listen' ('tell' w)) = k w (w, ())
-- @
-- @
-- 'runWriter' k ('censor' f ('tell' w)) = k (f w) ()
-- @
--
-- @since 1.1.0.0
runWriter :: Monoid w => (w -> a -> m b) -> WriterC w m a -> m b
runWriter k = runState k mempty . runWriterC
{-# INLINE runWriter #-}

-- | Run a 'Writer' effect with a 'Monoid'al log, producing the final log and discarding the result value.
--
-- @
-- 'execWriter' = 'runWriter' ('const' '.' 'pure')
-- @
--
-- @since 1.1.0.0
execWriter :: (Monoid w, Applicative m) => WriterC w m a -> m w
execWriter = runWriter (const . pure)
{-# INLINE execWriter #-}

-- | @since 1.1.0.0
newtype WriterC w m a = WriterC { runWriterC :: StateC w m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (ThreadAlgebra ((,) w) ctx m, Monoid w) => Algebra ctx (WriterC w m) where
  type Sig (WriterC w m) = Writer w :+: Sig m
  alg hdl sig ctx = WriterC $ case sig of
    L writer -> StateC $ \ k w -> case writer of
      Tell w'    -> do
        let !w'' = mappend w w'
        k w'' ctx
      Listen   m -> runWriter (\ w' a -> do
        let !w'' = mappend w w'
        k w'' ((,) w' <$> a)) (hdl (m <$ ctx))
      Censor f m -> runWriter (\ w' a -> do
        let !w'' = mappend w (f w')
        k w'' a) (hdl (m <$ ctx))
    R other  -> alg (runWriterC . hdl) (R other) ctx
  {-# INLINE alg #-}
