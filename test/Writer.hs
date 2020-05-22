{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Writer
( tests
, gen0
, genN
, test
) where

import           Control.Arrow ((&&&))
import qualified Control.Carrier.Writer.Church as C.Writer.Church
import qualified Control.Carrier.Writer.Strict as C.Writer.Strict
import           Control.Effect.Writer
#if MIN_VERSION_transformers(0,5,6)
import qualified Control.Monad.Trans.RWS.CPS as T.RWS.CPS
#endif
import qualified Control.Monad.Trans.RWS.Lazy as T.RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict as T.RWS.Strict
#if MIN_VERSION_transformers(0,5,6)
import qualified Control.Monad.Trans.Writer.CPS as T.Writer.CPS
#endif
import qualified Control.Monad.Trans.Writer.Lazy as T.Writer.Lazy
import qualified Control.Monad.Trans.Writer.Strict as T.Writer.Strict
import           Data.Bifunctor (first)
import           Data.Tuple (swap)
import           Gen
import qualified Monad
import qualified MonadFix
import           Test.Tasty
import           Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Writer"
  [ testGroup "WriterC (Church)" $
    [ testMonad
    , testMonadFix
    , testWriter
    ] >>= ($ runL (C.Writer.Church.runWriter (curry pure)))
  , testGroup "WriterC (Strict)" $
    [ testMonad
    , testMonadFix
    , testWriter
    ] >>= ($ runL C.Writer.Strict.runWriter)
  , testGroup "(,)"              $ testWriter (runL pure)
#if MIN_VERSION_transformers(0,5,6)
  , testGroup "WriterT (CPS)"    $ testWriter (runL (fmap swap . T.Writer.CPS.runWriterT))
#endif
  , testGroup "WriterT (Lazy)"   $ testWriter (runL (fmap swap . T.Writer.Lazy.runWriterT))
  , testGroup "WriterT (Strict)" $ testWriter (runL (fmap swap . T.Writer.Strict.runWriterT))
#if MIN_VERSION_transformers(0,5,6)
  , testGroup "RWST (CPS)"       $ testWriter (runL (runRWST T.RWS.CPS.runRWST))
#endif
  , testGroup "RWST (Lazy)"      $ testWriter (runL (runRWST T.RWS.Lazy.runRWST))
  , testGroup "RWST (Strict)"    $ testWriter (runL (runRWST T.RWS.Strict.runRWST))
  ] where
  testMonad    run = Monad.test    (m (gen0 w) (genN w b)) a b c initial run
  testMonadFix run = MonadFix.test (m (gen0 w) (genN w b)) a b   initial run
  testWriter   run = Writer.test w (m (gen0 w) (genN w b)) a     initial run
  initial = identity <*> unit
  runRWST f m = (\ (a, _, w) -> (w, a)) <$> f m () ()


gen0 :: Has (Writer w) m => GenTerm w -> GenTerm a -> [GenTerm (m a)]
gen0 w a = [ infixL 4 "<$" (<$) <*> a <*> (label "tell" tell <*> w) ]

genN
  :: forall w b m a
  .  (Has (Writer w) m, Arg b, Arg w, Show b, Show w, Vary b, Vary w)
  => GenTerm w
  -> GenTerm b
  -> GenM m
  -> GenTerm a
  -> [GenTerm (m a)]
genN w b m a =
  [ atom "fmap" fmap <*> fn a <*> (label "listen" (listen @w) <*> m b)
  , subtermM (m a) (label "censor" censor <*> fn w <*>)
  ]


test
  :: (Has (Writer w) m, Arg w, Eq a, Eq w, Monoid w, Show a, Show w, Vary w, Functor f)
  => GenTerm w
  -> GenM m
  -> GenTerm a
  -> GenTerm (f ())
  -> Run f ((,) w) m
  -> [TestTree]
test w m a i (Run runWriter) =
  [ testProperty "tell appends a value to the log" . forall (i :. w :. m a :. Nil) $
    \ i w m -> runWriter ((tell w >> m) <$ i) === fmap (first (mappend w)) (runWriter (m <$ i))
  , testProperty "listen eavesdrops on written output" . forall (i :. m a :. Nil) $
    \ i m -> runWriter (listen m <$ i) === fmap (fst &&& id) (runWriter (m <$ i))
  , testProperty "censor revises written output" . forall (i :. fn w :. m a :. Nil) $
    \ i f m -> runWriter (censor f m <$ i) === fmap (first f) (runWriter (m <$ i))
  ]
