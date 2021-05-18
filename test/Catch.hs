{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Catch
( tests
, genN
, test
) where

import Control.Effect.Error
import Gen

tests :: TestTree
tests = testGroup "Catch"
  []


genN
  :: forall e m a
  .  (Has (Catch e) m, Arg e, Show e, Vary e)
  => GenTerm e
  -> GenM m
  -> GenTerm a
  -> [GenTerm (m a)]
genN _ m a = [ addLabel "catchError" $ subtermM (m a) (\ m' -> infixL 9 "`catchError`" catchError <*> m' <*> fn @e (m a)) ]


test
  :: (Has (Error e) m, Arg e, Eq a, Eq e, Show a, Show e, Vary e, Functor f)
  => GenTerm e
  -> GenM m
  -> GenTerm a
  -> GenTerm b
  -> GenTerm (f ())
  -> Run f (Either e) m
  -> [TestTree]
test e m a _ i (Run runCatch) =
  [ testProperty "catchError intercepts throwError" . forall (i :. e :. fn (m a) :. Nil) $
    \ i e h -> runCatch ((throwError e `catchError` h) <$ i) === runCatch (h e <$ i)
  ]
