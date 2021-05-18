{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A carrier for the 'Trace' effect that prints all traced results to stderr.
--
-- @since 1.0.0.0
module Control.Carrier.Trace.Printing
( -- * Trace carrier
  runTrace
, TraceC(..)
  -- * Trace effect
, module Control.Effect.Trace
) where

import Control.Algebra
import Control.Applicative (Alternative)
import Control.Effect.Trace
import Control.Monad (MonadPlus)
import Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import System.IO

-- | Run a 'Trace' effect, printing traces to 'stderr'.
--
-- @
-- 'runTrace' ('trace' s) = 'liftIO' ('putStrLn' s)
-- @
-- @
-- 'runTrace' ('pure' a) = 'pure' a
-- @
--
-- @since 1.0.0.0
runTrace :: TraceC m a -> m a
runTrace (TraceC m) = m
{-# INLINE runTrace #-}

-- | @since 1.0.0.0
newtype TraceC m a = TraceC (m a)
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans TraceC where
  lift = TraceC
  {-# INLINE lift #-}

instance (MonadIO m, Algebra ctx m) => Algebra ctx (TraceC m) where
  type Sig (TraceC m) = Trace :+: Sig m
  alg hdl sig ctx = case sig of
    L (Trace s) -> ctx <$ liftIO (hPutStrLn stderr s)
    R other     -> TraceC (alg (runTrace . hdl) other ctx)
  {-# INLINE alg #-}
