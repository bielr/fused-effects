{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeApplications #-}
module State
( tests
, gen
, test
) where

import qualified Control.Carrier.State.Lazy as LazyStateC
import qualified Control.Carrier.State.Strict as StrictStateC
import Control.Effect.State
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWST
import qualified Control.Monad.Trans.RWS.Strict as StrictRWST
import qualified Control.Monad.Trans.State.Lazy as LazyStateT
import qualified Control.Monad.Trans.State.Strict as StrictStateT
import Data.Tuple (swap)
import Gen
import qualified Monad
import qualified MonadFix
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "State"
  [ testGroup "StateC (Lazy)"   $
    [ testMonad
    , testMonadFix
    , testState
    ] >>= ($ runC LazyStateC.runState)
  , testGroup "StateC (Strict)" $
    [ testMonad
    , testMonadFix
    , testState
    ] >>= ($ runC StrictStateC.runState)
  , testGroup "StateT (Lazy)"   $ testState (runC (fmap (fmap swap) . flip LazyStateT.runStateT))
  , testGroup "StateT (Strict)" $ testState (runC (fmap (fmap swap) . flip StrictStateT.runStateT))
  , testGroup "RWST (Lazy)"     $ testState (runC (runRWST LazyRWST.runRWST))
  , testGroup "RWST (Strict)"   $ testState (runC (runRWST StrictRWST.runRWST))
  ] where
  testMonad    run = Monad.test    (m (gen s)) a b c (atom "(,)" (,) <*> s <*> unit) run
  testMonadFix run = MonadFix.test (m (gen s)) a b   (atom "(,)" (,) <*> s <*> unit) run
  testState    run = State.test    (m (gen s)) a                         s           run
  runRWST f s m = (\ (a, s, ()) -> (s, a)) <$> f m s s


gen
  :: forall s m sig
  .  (Has (State s) sig m, Arg s, Show s, Vary s)
  => Gen s
  -> GenM m
  -> GenM m
gen s _ = GenM $ \ a -> choice
  [ label "gets" (gets @s) <*> fn a
  , infixL 4 "<$" (<$) <*> a <*> (label "put" put <*> s)
  ]


test
  :: (Has (State s) sig m, Arg s, Eq a, Eq s, Show a, Show s, Vary s)
  => GenM m
  -> Gen a
  -> Gen s
  -> Run ((,) s) ((,) s) m
  -> [TestTree]
test (GenM m) a s (Run runState) =
  [ testProperty "get returns the state variable" . forall (s :. fn (m a) :. Nil) $
    \ s k -> runState (s, get >>= k) === runState (s, k s)
  , testProperty "put updates the state variable" . forall (s :. s :. m a :. Nil) $
    \ s s' m -> runState (s, put s' >> m) === runState (s', m)
  ]
