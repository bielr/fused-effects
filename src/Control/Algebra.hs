{-# LANGUAGE BlockArguments, ConstraintKinds, DataKinds, DefaultSignatures, DeriveFunctor, FlexibleContexts, FlexibleInstances, FunctionalDependencies, KindSignatures, QuantifiedConstraints, RankNTypes, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}

{- | The 'Algebra' class is the mechanism with which effects are interpreted.

An instance of the 'Algebra' class defines an interpretation of an effect signature atop a given monad.

@since 1.0.0.0
-}
module Control.Algebra
( Algebra(..)
, run
, Has
, send
, handleIdentity
, handleCoercible
  -- * Re-exports
, (:+:) (..)
, module Control.Effect.Class
, Carrier(..)
, AlgebraTrans(..)
, Algebra'(..)
) where

import Control.Carrier.Trans
import Control.Effect.Catch.Internal
import Control.Effect.Choose.Internal
import Control.Effect.Class
import Control.Effect.Empty.Internal
import Control.Effect.Error.Internal
import Control.Effect.Lift.Internal
import Control.Effect.NonDet.Internal
import Control.Effect.Reader.Internal
import Control.Effect.State.Internal
import Control.Effect.Sum ((:+:)(..), Member(..), Members)
import Control.Effect.Throw.Internal
import Control.Effect.Writer.Internal
import Control.Monad ((<=<))
import Data.Functor (($>))
import Data.Functor.Compose
import Data.Functor.Identity
import Control.Monad.Trans.Class (MonadTrans(..))
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Identity as Identity
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.RWS.Lazy as RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict as RWS.Strict
import qualified Control.Monad.Trans.State.Lazy as State.Lazy
import qualified Control.Monad.Trans.State.Strict as State.Strict
import qualified Control.Monad.Trans.Writer.Lazy as Writer.Lazy
import qualified Control.Monad.Trans.Writer.Strict as Writer.Strict
import GHC.Generics hiding (type (:+:))
import Data.Coerce
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Semigroup as S
import Data.Tuple (swap)



class (HFunctor (Eff t), Monad m, Monad (t m), MonadTrans t) => Carrier m t where
  type Eff t :: (* -> *) -> * -> *
  eff :: Eff t (t m) a -> t m a

class (HFunctor (Sig m), Monad m) => Algebra' m where
  type Sig m :: (* -> *) -> * -> *
  alg' :: Sig m m a -> m a


class Functor (Context t) => AlgebraTrans t where
  type Context t :: * -> *

  liftWithC
    :: (Monad m)
    => (Context t ()
        -> (forall x. Context t (t m x) -> m (Context t x))
        -> m (Context t a))
    -> t m a


liftedAlg
  :: (Algebra' m, AlgebraTrans t, Monad (t m), Weaves (Context t) (Sig m))
  => Sig m (t m) a
  -> t m a
liftedAlg sig = liftWithC (\ctx hdl -> alg' (weave ctx hdl sig))


-- class Algebra' m => HelperTransC ts m where
--   liftedAlgTransC :: Sig m (TransC ts m) a -> TransC ts m a
--
-- instance (Algebra' m) => HelperTransC '[] m where
--   liftedAlgTransC = TransC . alg' . hmap runTransC
--
-- instance (Algebra' m, HFunctor (Sig m), HFunctor t, Monad (ApplyTrans ts m), HelperTransC ts m) => HelperTransC (t ': ts) m where
--   liftedAlgTransC = pushTransC . liftedAlgTransC . hmap peelTransC

instance
  ( AlgebraTrans t
  , AlgebraTrans t'
  , forall m. Monad m => Monad (t' m)
  ) => AlgebraTrans (ComposeC t t') where
  type Context (ComposeC t t') = Compose (Context t') (Context t)
  liftWithC f = ComposeC $
    liftWithC \ctx hdl ->
      liftWithC \ctx' hdl' ->
        fmap getCompose $
          f (Compose (ctx' $> ctx))
            (fmap Compose . hdl' . fmap hdl . getCompose . fmap runComposeC)


-- instance {-# overlappable #-} (Carrier m t, AlgebraTrans m t, Monad (t m)) => Algebra' (t m) where
--   type Sig (t m) = Eff t :+: Sig m
--   alg' (L e) = eff e
--   alg' (R o) = liftedAlg o



-- | The class of carriers (results) for algebras (effect handlers) over signatures (effects), whose actions are given by the 'alg' method.
--
-- @since 1.0.0.0
class (Monad m, HFunctor sig) => Algebra sig m | m -> sig where
  -- | Construct a value in the carrier for an effect signature (typically a sum of a handled effect and any remaining effects).
  alg :: sig m a -> m a


-- | Run an action exhausted of effects to produce its final result value.
--
-- @since 1.0.0.0
run :: Identity a -> a
run = runIdentity
{-# INLINE run #-}


-- | @m@ is a carrier for @sig@ containing @eff@.
--
-- Note that if @eff@ is a sum, it will be decomposed into multiple 'Member' constraints. While this technically allows one to combine multiple unrelated effects into a single 'Has' constraint, doing so has two significant drawbacks:
--
-- 1. Due to [a problem with recursive type families](https://gitlab.haskell.org/ghc/ghc/issues/8095), this can lead to significantly slower compiles when overused.
--
-- 2. It defeats @ghc@’s warnings for redundant constraints, and thus can lead to a proliferation of redundant constraints as code is changed.
--
-- @since 1.0.0.0
type Has eff sig m = (Members eff sig, Algebra sig m)

-- | Construct a request for an effect to be interpreted by some handler later on.
--
-- @since 0.1.0.0
send :: (Member eff sig, Algebra sig m) => eff m a -> m a
send = alg . inj
{-# INLINE send #-}


-- | Thread a stateless handler for a carrier through an effect and eliminate it with the carrier’s algebra.
--
-- This is useful for carriers taking some input but not modifying the output. When @m@ is coercible to @n@, 'handleCoercible' may be more appropriate.
--
-- @since 1.0.0.0
handleIdentity :: (Monad m, Weaves Identity eff, Member eff sig, Algebra sig n) => (forall x . m x -> n x) -> eff m a -> n a
handleIdentity f = fmap runIdentity . send . weave (Identity ()) (fmap Identity . f . runIdentity)
{-# INLINE handleIdentity #-}

-- | Thread a 'Coercible' carrier through an effect.
--
-- This is applicable whenever @m@ is 'Coercible' to @n@, e.g. simple @newtype@s.
--
-- @since 1.0.0.0
handleCoercible :: (Monad m, Weaves Identity eff, Member eff sig, Algebra sig n, Coercible m n) => eff m a -> n a
handleCoercible = handleIdentity coerce
{-# INLINE handleCoercible #-}


-- base

instance Algebra'  IO where
  type Sig IO = Lift IO
  alg' (LiftWith with k) = with (Identity ()) coerce >>= k . runIdentity

instance Algebra (Lift IO) IO where
  alg (LiftWith with k) = with (Identity ()) coerce >>= k . runIdentity

instance Algebra (Lift Identity) Identity where
  alg (LiftWith with k) = with (Identity ()) coerce >>= k . runIdentity

instance Algebra Choose NonEmpty where
  alg (Choose m) = m True S.<> m False

instance Algebra' NonEmpty where
  type Sig NonEmpty = Choose
  alg' (Choose m) = m True S.<> m False

instance Algebra Empty Maybe where
  alg Empty = Nothing

instance Algebra (Error e) (Either e) where
  alg (L (Throw e))     = Left e
  alg (R (Catch m h k)) = either (k <=< h) k m

instance Algebra (Reader r) ((->) r) where
  alg (Ask       k) r = k r r
  alg (Local f m k) r = k (m (f r)) r

instance Algebra NonDet [] where
  alg (L Empty)      = []
  alg (R (Choose k)) = k True ++ k False

instance Monoid w => Algebra (Writer w) ((,) w) where
  alg (Tell w (w', k))    = (mappend w w', k)
  alg (Listen (w, a) k)   = let (w', a') = k w a in (mappend w w', a')
  alg (Censor f (w, a) k) = let (w', a') = k a in (mappend (f w) w', a')


-- transformers

instance Monad m => Carrier m (Except.ExceptT e) where
  type Eff (Except.ExceptT e) = Error e
  eff (L (Throw e))     = Except.throwE e
  eff (R (Catch m h k)) = Except.catchE m h >>= k


instance AlgebraTrans (Except.ExceptT e) where
  type Context (Except.ExceptT e) = Either e
  liftWithC f = Except.ExceptT $ f (Right ()) (either (pure . Left) Except.runExceptT)


instance (Algebra sig m, Weaves (Either e) sig) => Algebra (Error e :+: sig) (Except.ExceptT e m) where
  alg (L (L (Throw e)))     = Except.throwE e
  alg (L (R (Catch m h k))) = Except.catchE m h >>= k
  alg (R other)             = Except.ExceptT $ alg (weave (Right ()) (either (pure . Left) Except.runExceptT) other)

instance Algebra sig m => Algebra sig (Identity.IdentityT m) where
  alg = Identity.IdentityT . handleCoercible

instance Algebra sig m => Algebra (Reader r :+: sig) (Reader.ReaderT r m) where
  alg (L (Ask       k)) = Reader.ask >>= k
  alg (L (Local f m k)) = Reader.local f m >>= k
  alg (R other)         = Reader.ReaderT $ \ r -> handleIdentity (flip Reader.runReaderT r) other

newtype RWSTF w s a = RWSTF { unRWSTF :: (a, s, w) }
  deriving (Functor)

toRWSTF :: Monoid w => w -> (a, s, w) -> RWSTF w s a
toRWSTF w (a, s, w') = RWSTF (a, s, mappend w w')
{-# INLINE toRWSTF #-}

instance (Algebra sig m, Weaves (RWSTF w s) sig, Monoid w) => Algebra (Reader r :+: Writer w :+: State s :+: sig) (RWS.Lazy.RWST r w s m) where
  alg (L (Ask       k))      = RWS.Lazy.ask >>= k
  alg (L (Local f m k))      = RWS.Lazy.local f m >>= k
  alg (R (L (Tell w k)))     = RWS.Lazy.tell w *> k
  alg (R (L (Listen m k)))   = RWS.Lazy.listen m >>= uncurry (flip k)
  alg (R (L (Censor f m k))) = RWS.Lazy.censor f m >>= k
  alg (R (R (L (Get   k))))  = RWS.Lazy.get >>= k
  alg (R (R (L (Put s k))))  = RWS.Lazy.put s *> k
  alg (R (R (R other)))      = RWS.Lazy.RWST $ \ r s -> unRWSTF <$> alg (weave (RWSTF ((), s, mempty)) (\ (RWSTF (x, s, w)) -> toRWSTF w <$> RWS.Lazy.runRWST x r s) other)

instance (Algebra sig m, Weaves (RWSTF w s) sig, Monoid w) => Algebra (Reader r :+: Writer w :+: State s :+: sig) (RWS.Strict.RWST r w s m) where
  alg (L (Ask       k))      = RWS.Strict.ask >>= k
  alg (L (Local f m k))      = RWS.Strict.local f m >>= k
  alg (R (L (Tell w k)))     = RWS.Strict.tell w *> k
  alg (R (L (Listen m k)))   = RWS.Strict.listen m >>= uncurry (flip k)
  alg (R (L (Censor f m k))) = RWS.Strict.censor f m >>= k
  alg (R (R (L (Get   k))))  = RWS.Strict.get >>= k
  alg (R (R (L (Put s k))))  = RWS.Strict.put s *> k
  alg (R (R (R other)))      = RWS.Strict.RWST $ \ r s -> unRWSTF <$> alg (weave (RWSTF ((), s, mempty)) (\ (RWSTF (x, s, w)) -> toRWSTF w <$> RWS.Strict.runRWST x r s) other)

instance (Algebra sig m, Weaves ((,) s) sig) => Algebra (State s :+: sig) (State.Lazy.StateT s m) where
  alg (L (Get   k)) = State.Lazy.get >>= k
  alg (L (Put s k)) = State.Lazy.put s *> k
  alg (R other)     = State.Lazy.StateT $ \ s -> swap <$> alg (weave (s, ()) (\ (s, x) -> swap <$> State.Lazy.runStateT x s) other)

instance (Algebra sig m, Weaves ((,) s) sig) => Algebra (State s :+: sig) (State.Strict.StateT s m) where
  alg (L (Get   k)) = State.Strict.get >>= k
  alg (L (Put s k)) = State.Strict.put s *> k
  alg (R other)     = State.Strict.StateT $ \ s -> swap <$> alg (weave (s, ()) (\ (s, x) -> swap <$> State.Strict.runStateT x s) other)

instance (Algebra sig m, Weaves ((,) w) sig, Monoid w) => Algebra (Writer w :+: sig) (Writer.Lazy.WriterT w m) where
  alg (L (Tell w k))     = Writer.Lazy.tell w *> k
  alg (L (Listen m k))   = Writer.Lazy.listen m >>= uncurry (flip k)
  alg (L (Censor f m k)) = Writer.Lazy.censor f m >>= k
  alg (R other)          = Writer.Lazy.WriterT $ swap <$> alg (weave (mempty, ()) (\ (s, x) -> swap . fmap (mappend s) <$> Writer.Lazy.runWriterT x) other)

instance (Algebra sig m, Weaves ((,) w) sig, Monoid w) => Algebra (Writer w :+: sig) (Writer.Strict.WriterT w m) where
  alg (L (Tell w k))     = Writer.Strict.tell w *> k
  alg (L (Listen m k))   = Writer.Strict.listen m >>= uncurry (flip k)
  alg (L (Censor f m k)) = Writer.Strict.censor f m >>= k
  alg (R other)          = Writer.Strict.WriterT $ swap <$> alg (weave (mempty, ()) (\ (s, x) -> swap . fmap (mappend s) <$> Writer.Strict.runWriterT x) other)
