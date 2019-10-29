{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving #-}
module Control.Effect.Reader.Internal
( Reader(..)
) where

import Control.Effect.Class

-- | @since 0.1.0.0
data Reader r m k
  = Ask (r -> m k)
  | forall b . Local (r -> r) (m b) (b -> m k)

deriving instance Functor m => Functor (Reader r m)

instance Functor ctx => Weaves ctx (Reader r) where
  weave ctx handler (Ask k)       = Ask                          (handler . (<$ ctx) . k)
  weave ctx handler (Local f m k) = Local f (handler (m <$ ctx)) (handler . fmap k)
