{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

{- | Provides an effect to delimit backtracking in a given nondeterministic context. This effect is used in concert with 'Control.Effect.NonDet.NonDet'.

Computations that signal failure with 'cutfail' prevent backtracking within the nearest enclosing 'call'.

Predefined carriers:

* "Control.Carrier.Cut.Church"

@since 0.1.2.0
-}

module Control.Effect.Cut
( -- * Cut effect
  Cut(..)
, cutfail
, call
, cut
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import Control.Applicative (Alternative(..))

-- | 'Cut' effects are used with 'Control.Effect.Choose' to provide control over backtracking.
--
-- @since 0.1.2.0
data Cut m k where
  Cutfail ::        Cut m a
  Call    :: m a -> Cut m a


-- | Fail the current branch, and prevent backtracking within the nearest enclosing 'call' (if any).
--
--   Contrast with 'empty', which fails the current branch but allows backtracking.
--
-- @
-- 'cutfail' '>>=' k = 'cutfail'
-- @
-- @
-- 'cutfail' '<|>' m = 'cutfail'
-- @
--
-- @since 0.1.2.0
cutfail :: Has Cut m => m a
cutfail = send Cutfail
{-# INLINE cutfail #-}

-- | Delimit the effect of 'cutfail's, allowing backtracking to resume.
--
-- @
-- 'call' 'cutfail' '<|>' m = m
-- @
--
-- @since 0.1.2.0
call :: Has Cut m => m a -> m a
call m = send (Call m)
{-# INLINE call #-}

-- | Commit to the current branch, preventing backtracking within the nearest enclosing 'call' (if any) on failure.
--
-- @
-- 'cut' '>>' 'empty' = 'cutfail'
-- @
--
-- @since 0.1.2.0
cut :: (Alternative m, Has Cut m) => m ()
cut = pure () <|> cutfail
{-# INLINE cut #-}
