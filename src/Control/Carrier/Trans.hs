{-# language DataKinds, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, QuantifiedConstraints, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Trans where

import Control.Algebra
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Fail (MonadFail)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Functor (($>))
import Data.Functor.Compose


newtype ComposeC
    (t :: (* -> *) -> * -> *)
    (t' :: (* -> *) -> * -> *)
    (m :: * -> *)
    a
    = ComposeC { runComposeC :: t (t' m) a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO)


instance
  ( forall m. Monad m => Monad (t' m)
  , MonadTrans t
  , MonadTrans t'
  )
  => MonadTrans (ComposeC t t') where
  lift = ComposeC . lift . lift


type family TransC (ts :: [(* -> *) -> * -> *]) :: (* -> *) -> * -> * where
  TransC '[]        = IdentityT
  TransC (t ': '[]) = t
  TransC (t ': ts)  = ComposeC t (TransC ts)



instance
  ( AlgebraTrans t
  , AlgebraTrans t'
  , forall m. Monad m => Monad (t' m)
  ) => AlgebraTrans (ComposeC t t') where

  type Context (ComposeC t t') = Compose (Context t') (Context t)

  liftWithC f = ComposeC $
    liftWithC $ \ctx hdl ->
      liftWithC $ \ctx' hdl' ->
        fmap getCompose $
          f (Compose (ctx' $> ctx))
            (fmap Compose . hdl' . fmap hdl . getCompose . fmap runComposeC)
  {-# inline liftWithC #-}


-- newtype instance TransC '[] m a = IdTransC { runIdTransC :: m a }
--   deriving (Functor, Applicative, Monad, MonadIO)
--
-- newtype instance TransC (t ': ts) m a = TransC { runTransC :: t (TransC ts m) a }
--
-- deriving instance Functor (t (TransC ts m)) => Functor (TransC (t ': ts) m)
-- deriving instance Applicative (t (TransC ts m)) => Applicative (TransC (t ': ts) m)
-- deriving instance Monad (t (TransC ts m)) => Monad (TransC (t ': ts) m)
-- deriving instance MonadIO (t (TransC ts m)) => MonadIO (TransC (t ': ts) m)


-- instance (HFunctor t, HFunctor t') => Weaves Identity (ComposeC t t') where
--   weave ctx hdl (ComposeC m) = ComposeC $ fmap _ $ _ m


-- instance AlgebraTrans (TransC '[]) where
--   type Context (TransC '[]) = Identity
--
--   liftWithC f = IdTransC $ fmap runIdentity $ f (Identity ()) (fmap Identity . runIdTransC . runIdentity)
--
--
-- instance
--   ( AlgebraTrans t
--   , AlgebraTrans (TransC ts)
--   , forall m. Monad m => Monad (TransC ts m)
--   ) => AlgebraTrans (TransC (t ': ts)) where
--   type Context (TransC (t ': ts)) = Compose (Context (TransC ts)) (Context t)
--
--   liftWithC f = TransC $
--     liftWithC \ctx hdl ->
--       liftWithC \ctxs hdls ->
--         fmap getCompose $
--           f (Compose (ctxs $> ctx))
--             (fmap Compose . hdls . fmap hdl . getCompose . fmap runTransC)

--       liftWithC \ctx' hdl' ->
--         fmap getCompose $
--           f (Compose (ctx' $> ctx))
--             (fmap Compose . hdl' . fmap hdl . getCompose . fmap runComposeC)
