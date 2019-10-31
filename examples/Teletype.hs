{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveFunctor, DeriveGeneric, DerivingStrategies, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Teletype
( example
, runTeletypeIO
) where

import Prelude hiding (read)

import Control.Algebra
import Control.Carrier.State.Strict
import Control.Carrier.Trans
import Control.Carrier.Writer.Strict
import Control.Monad.IO.Class
import Control.Monad.Trans.Identity
import GHC.Generics (Generic1)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

example :: TestTree
example = testGroup "teletype"
  [ testProperty "reads" . property $ do
    line <- forAll genLine
    run (runTeletypeRet [line] read) === ([], ([], line))

  , testProperty "writes" . property $ do
    input  <- forAll (Gen.list (Range.linear 0 10) genLine)
    output <- forAll genLine
    run (runTeletypeRet input (write output)) === ([output], (input, ()))

  , testProperty "writes multiple things" . property $ do
    input   <- forAll (Gen.list (Range.linear 0 10) genLine)
    output1 <- forAll genLine
    output2 <- forAll genLine
    run (runTeletypeRet input (write output1 >> write output2)) === ([output1, output2], (input, ()))
  ] where
  genLine = Gen.string (Range.linear 0 20) Gen.unicode

data Teletype m k
  = Read (String -> m k)
  | Write String (m k)
  deriving stock (Functor, Generic1)
  deriving anyclass (Weaves ctx)

read :: Has' Teletype m => m String
read = send (Read pure)

write :: Has' Teletype m => String -> m ()
write s = send (Write s (pure ()))


runTeletypeIO :: TeletypeIOC m a -> m a
runTeletypeIO = runIdentityT . runTeletypeIOC

newtype TeletypeIOC m a = TeletypeIOC { runTeletypeIOC :: IdentityT m a }
  deriving newtype (AlgebraTrans, Applicative, Functor, Monad, MonadIO)

instance MonadIO m => Carrier m TeletypeIOC where
  type Eff TeletypeIOC = Teletype

  eff (Read    k) = liftIO getLine      >>= k
  eff (Write s k) = liftIO (putStrLn s) >>  k


runTeletypeRet :: [String] -> TeletypeRetC m a -> m ([String], ([String], a))
runTeletypeRet i = runWriter . runState i . runComposeC . runTeletypeRetC

newtype TeletypeRetC m a = TeletypeRetC { runTeletypeRetC :: TransC '[StateC [String], WriterC [String]] m a }
  deriving newtype (Applicative, Functor, Monad)

instance AlgebraTrans TeletypeRetC where
  type Context TeletypeRetC = Context (TransC '[StateC [String], WriterC [String]])
  liftWithC = liftCoercibleWithC TeletypeRetC

instance (Monad m, Algebra' (StateC [String] (WriterC [String] m))) => Carrier m TeletypeRetC where
  type Eff TeletypeRetC = Teletype

  eff (Read    k) = do
    i <- TeletypeRetC (ComposeC get)
    case i of
      []  -> k ""
      h:t -> TeletypeRetC (ComposeC (put t)) *> k h
  eff (Write s k) = TeletypeRetC (ComposeC (tell [s])) *> k
