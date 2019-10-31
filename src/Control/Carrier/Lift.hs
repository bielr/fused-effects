{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, KindSignatures, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}

-- | A carrier for 'Lift' allowing monadic actions to be lifted from an outer context into an inner one with 'sendM', and for an inner context to run actions in an outer one with 'liftWith'.
--
-- @since 1.0.0.0
module Control.Carrier.Lift
( -- * Lift carrier
  runM
, LiftC(..)
  -- * Lift effect
, module Control.Effect.Lift
) where

import Control.Algebra
import Control.Applicative (Alternative)
import Control.Effect.Lift
import Control.Monad (MonadPlus)
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
import Data.Functor.Identity

-- | Extract a 'Lift'ed 'Monad'ic action from an effectful computation.
--
-- @since 1.0.0.0
runM :: LiftC m m a -> m a
runM (LiftC m) = runIdentityT m

-- | @since 1.0.0.0
newtype LiftC (n :: * -> *) m a = LiftC (IdentityT m a)
  deriving (AlgebraTrans, Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (n ~ m, Monad m) => Carrier m (LiftC n) where
  type Eff (LiftC n) = Lift n
  eff (LiftWith with k) = LiftC (IdentityT (with (Identity ()) (fmap Identity . runM . runIdentity))) >>= k . runIdentity

instance (n ~ m, Monad m) => Algebra (Lift m) (LiftC n m) where
  alg (LiftWith with k) = LiftC (IdentityT (with (Identity ()) (fmap Identity . runM . runIdentity))) >>= k . runIdentity
