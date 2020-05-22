{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Teletype
( example
) where

import           Control.Algebra
import           Control.Carrier.State.Strict
import           Control.Carrier.Writer.Strict
import           Control.Monad.IO.Class
import           Data.Kind (Type)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Prelude hiding (read)
import           Test.Tasty
import           Test.Tasty.Hedgehog

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

data Teletype (m :: Type -> Type) k where
  Read  ::           Teletype m String
  Write :: String -> Teletype m ()


read :: Has Teletype m => m String
read = send Read

write :: Has Teletype m => String -> m ()
write s = send (Write s)


newtype TeletypeIOC m a = TeletypeIOC { runTeletypeIO :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra ctx m) => Algebra ctx (TeletypeIOC m) where
  type Sig (TeletypeIOC m) = Teletype :+: Sig m
  alg hdl sig ctx = case sig of
    L Read      -> (<$ ctx) <$> liftIO getLine
    L (Write s) -> ctx <$ liftIO (putStrLn s)
    R other     -> TeletypeIOC (alg (runTeletypeIO . hdl) other ctx)


runTeletypeRet :: [String] -> TeletypeRetC m a -> m ([String], ([String], a))
runTeletypeRet i = runWriter . runState i . runTeletypeRetC

type InnerTeletypeRetC m = StateC [String] (WriterC [String] m)

newtype TeletypeRetC m a = TeletypeRetC { runTeletypeRetC :: InnerTeletypeRetC m a }
  deriving (Applicative, Functor, Monad)

instance (Algebra ctx m, Algebra ctx (InnerTeletypeRetC m)) => Algebra ctx (TeletypeRetC m) where
  type Sig (TeletypeRetC m) = Teletype :+: Sig m
  alg hdl sig ctx = TeletypeRetC $ case sig of
    L Read      -> do
      i <- get
      case i of
        []  -> pure ("" <$ ctx)
        h:t -> h <$ ctx <$ put t
    L (Write s) -> ctx <$ tell [s]
    R other     -> alg (runTeletypeRetC . hdl) (R (R other)) ctx
