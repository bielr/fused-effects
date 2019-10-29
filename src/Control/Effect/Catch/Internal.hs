{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving #-}
module Control.Effect.Catch.Internal
( Catch(..)
) where

import Control.Effect.Class

-- | 'Catch' effects can be used alongside 'Control.Effect.Throw.Throw' to provide recoverable exceptions.
--
-- @since 1.0.0.0
data Catch e m k
  = forall b . Catch (m b) (e -> m b) (b -> m k)

deriving instance Functor m => Functor (Catch e m)

instance Functor ctx => Threads ctx (Catch e) where
  thread ctx handler (Catch m h k) = Catch (handler (m <$ ctx)) (handler . (<$ ctx) . h) (handler . fmap k)
