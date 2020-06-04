{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Monadic where

-- base
import Control.Arrow
import Data.Kind (Constraint)

-- transformers
import Control.Monad.Trans.Class

-- dunai
import qualified Data.MonadicStreamFunction as MSF
import Data.MonadicStreamFunction.InternalCore

class (forall m . Monad m => Arrow (arr m)) => ArrowMonadic arr where
  arrM
    :: Monad m
    => (a -> m   b)
    -> arr   m a b
  hoistA
    :: (Monad m1, Monad m2)
    => (forall x . m1 x -> m2 x)
    -> arr         m1           a b
    -> arr                 m2   a b

liftTransA
  :: (ArrowMonadic arr, Monad m, MonadTrans t, Monad (t m))
  => arr          m  a b
  -> arr       (t m) a b
liftTransA = hoistA lift

constM
  :: (Monad m, ArrowMonadic arr)
  =>        m   b
  -> arr    m a b
constM = arrM . const

instance ArrowMonadic MSF where
  arrM = MSF.arrM
  hoistA = MSF.morphS

class ArrowMonadic arr => ArrowRun arr where
  step
    :: arr m a b
    ->       a
    ->     m  (b, arr m a b)
  runArrow
    :: Monad m
    => arr m () ()
    -> m ()

instance ArrowRun MSF where
  runArrow = MSF.reactimate
  step = unMSF

class Arrow arr => ArrowFeedback arr where
  type StateConstraint arr c :: Constraint

  feedback
    :: StateConstraint arr c
    => c
    -> arr (a, c) (b, c)
    -> arr  a      b

instance Monad m => ArrowFeedback (MSF m) where
  type StateConstraint (MSF m) c = (() :: Constraint)
  feedback = MSF.feedback

class Arrow arr => ArrowConcat arr where
  concatA :: arr a [b] -> arr a b

instance Monad m => ArrowConcat (MSF m) where
  concatA msf = MSF $ tick msf []
    where
      tick msf' (b:bs) _ = return (b, MSF $ tick msf' bs)
      tick msf' []     a = do
        (bs, msf'') <- unMSF msf' a
        tick msf'' bs a
