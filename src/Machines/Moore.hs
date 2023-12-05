{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Moore Machines and related machinery.
module Machines.Moore
  ( Moore (..),
    Moore' (..),
    fixMoore,
    scanMoore,
    processMoore,
    processMoore',
    (/\),
    (/+\),
    (\/),
  )
where

--------------------------------------------------------------------------------

import Control.Applicative (liftA2)
import Control.Category.Cartesian (split)
import Data.Bifunctor (bimap)
import Data.Bifunctor.Monoidal qualified as Bifunctor
import Data.Either (fromLeft, fromRight)
import Data.Profunctor (Closed (..), Costrong (..), Profunctor (..))
import Data.Profunctor.Rep (Corepresentable (..), unfirstCorep)
import Data.Profunctor.Sieve (Cosieve (..))
import Data.These (These (..), these)
import Data.Trifunctor.Monoidal qualified as Trifunctor
import Data.Void (Void, absurd)

--------------------------------------------------------------------------------

-- | Moore Machine
--
--   * A finite set of states @S@
--   * An initial state @s : S@
--   * A finite set called the input @I@
--   * A finite set called the output @O@
--   * A function @transistion : S × I → S@
--   * A function @observe : S → O@
--
-- In this particular encoding we receive the initial state and
-- produce a tuple of the observation at the initial state and the
-- next state transition function.
newtype Moore s i o = Moore {runMoore :: s -> (o, i -> s)}
  deriving (Functor)

instance Trifunctor.Semigroupal (->) (,) (,) (,) (,) Moore where
  combine :: (Moore s i o, Moore t i' o') -> Moore (s, t) (i, i') (o, o')
  combine (Moore m1, Moore m2) =
    Moore $ \(s, t) ->
      let (o, transition) = m1 s
          (o', transition') = m2 t
       in ((o, o'), bimap transition transition')

instance Trifunctor.Semigroupal (->) (,) Either (,) (,) Moore where
  combine :: (Moore s i o, Moore t i' o') -> Moore (s, t) (Either i i') (o, o')
  combine (Moore m1, Moore m2) =
    Moore $ \(s, t) ->
      let (o, transition) = m1 s
          (o', transition') = m2 t
       in ((o, o'), either ((,t) . transition) ((s,) . transition'))

instance Trifunctor.Semigroupal (->) (,) These (,) (,) Moore where
  combine :: (Moore s i o, Moore t i' o') -> Moore (s, t) (These i i') (o, o')
  combine (Moore m1, Moore m2) =
    Moore $ \(s, t) ->
      let (o, transition) = m1 s
          (o', transition') = m2 t
       in ((o, o'), these ((,t) . transition) ((s,) . transition') (\i i' -> (transition i, transition' i')))

instance Trifunctor.Unital (->) () () () () Moore where
  introduce :: () -> Moore () () ()
  introduce () = Moore $ \() -> ((), const ())

instance Trifunctor.Unital (->) () Void () () Moore where
  introduce :: () -> Moore () Void ()
  introduce () = Moore $ \() -> ((), const ())

instance Trifunctor.Monoidal (->) (,) () (,) () (,) () (,) () Moore

instance Trifunctor.Monoidal (->) (,) () Either Void (,) () (,) () Moore

instance Trifunctor.Monoidal (->) (,) () These Void (,) () (,) () Moore

instance Profunctor (Moore s) where
  dimap :: (i' -> i) -> (o -> o') -> Moore s i o -> Moore s i' o'
  dimap f g (Moore moore) = Moore $ fmap (bimap g (lmap f)) moore

--------------------------------------------------------------------------------

-- | The fixed point of a 'Moore' Machine. By taking the fixpoint we
-- are able to hide the state parameter @s@.
newtype Moore' i o = Moore' {runMoore' :: (o, i -> Moore' i o)}
  deriving (Functor)

instance Applicative (Moore' i) where
  pure :: o -> Moore' i o
  pure o = let r = Moore' (o, const r) in r

  liftA2 :: (o -> o' -> o'') -> Moore' i o -> Moore' i o' -> Moore' i o''
  liftA2 f (Moore' (a, m1)) (Moore' (b, m2)) = Moore' (f a b, \i -> liftA2 f (m1 i) (m2 i))

instance Monad (Moore' i) where
  return :: o -> Moore' i o
  return = pure

  (>>=) :: Moore' i o -> (o -> Moore' i o') -> Moore' i o'
  m >>= f =
    let join' (Moore' (a, g)) = Moore' (fst $ runMoore' a, \i -> join' (($ i) . snd . runMoore' <$> g i))
     in join' (fmap f m)

instance Bifunctor.Semigroupal (->) (,) (,) (,) Moore' where
  combine :: (Moore' i o, Moore' i' o') -> Moore' (i, i') (o, o')
  combine (Moore' (o, m1), Moore' (o', m2)) = Moore' ((o, o'), \(i, i') -> Bifunctor.combine (m1 i, m2 i'))

instance Bifunctor.Semigroupal (->) Either (,) (,) Moore' where
  -- NOTE: I can't tell if this is legit or not:
  combine :: (Moore' i o, Moore' i' o') -> Moore' (Either i i') (o, o')
  combine (Moore' (o, transition), Moore' (o', transition')) =
    Moore' ((o, o'), either (\i -> dimap (fromLeft i) (,o') $ transition i) (\i' -> dimap (fromRight i') (o,) $ transition' i'))

instance Bifunctor.Semigroupal (->) These (,) (,) Moore' where
  -- NOTE: I can't tell if this is legit or not:
  combine :: (Moore' i o, Moore' i' o') -> Moore' (These i i') (o, o')
  combine (Moore' (o, transition), Moore' (o', transition')) =
    let f i = dimap (\case This i' -> i'; _ -> i) (,o') $ transition i
        g i' = dimap (\case That i -> i; _ -> i') (o,) $ transition' i'
     in Moore' ((o, o'), these f g $ \i i' -> Bifunctor.combine @_ @These @(,) @(,) (transition i, transition' i'))

instance Bifunctor.Unital (->) () () () Moore' where
  introduce :: () -> Moore' () ()
  introduce () = Moore' ((), Bifunctor.introduce)

instance Bifunctor.Unital (->) Void () () Moore' where
  introduce :: () -> Moore' Void ()
  introduce () = Moore' ((), absurd)

instance Bifunctor.Monoidal (->) (,) () (,) () (,) () Moore'

instance Bifunctor.Monoidal (->) Either Void (,) () (,) () Moore'

instance Bifunctor.Monoidal (->) These Void (,) () (,) () Moore'

instance Profunctor Moore' where
  dimap :: (i' -> i) -> (o -> o') -> Moore' i o -> Moore' i' o'
  dimap f g (Moore' serve) =
    Moore' $ bimap g (dimap f (dimap f g)) serve

instance Costrong Moore' where
  unfirst :: Moore' (i, d) (o, d) -> Moore' i o
  unfirst = unfirstCorep

instance Cosieve Moore' [] where
  cosieve :: Moore' i o -> [i] -> o
  cosieve (Moore' (o, _)) [] = o
  cosieve (Moore' (_, k)) (x : xs) = cosieve (k x) xs

instance Corepresentable Moore' where
  type Corep Moore' = []

  cotabulate :: (Corep Moore' i -> o) -> Moore' i o
  cotabulate f = Moore' (f [], \i -> cotabulate (f . (i :)))

instance Closed Moore' where
  closed :: Moore' i o -> Moore' (x -> i) (x -> o)
  closed m = cotabulate $ \fs x -> cosieve m (fmap ($ x) fs)

-- | Take the fixpoint of @Moore s i o@ by recursively constructing an
-- @s -> Moore' i o@ action and tupling it with the output observation
-- @o@ from its parent action.
fixMoore :: forall s o i. Moore s o i -> s -> Moore' o i
fixMoore (Moore moore) = go
  where
    go :: s -> Moore' o i
    go s = Moore' $ fmap go <$> moore s

--------------------------------------------------------------------------------

-- | Feed inputs into a 'Moore' Machine and extract the observation at
-- each state/input in a 'scan' style.
scanMoore :: s -> [i] -> Moore s i o -> [(o, s)]
scanMoore state' inputs machine =
  let (o, transition) = runMoore machine state'
   in case inputs of
        [] -> [(o, state')]
        i : xs -> (o, state') : scanMoore (transition i) xs machine

-- | Feed inputs into a 'Moore' Machine and then observe the final
-- result.
processMoore :: s -> [i] -> Moore s i o -> o
processMoore initialState inputs machine =
  let (o, transition) = runMoore machine initialState
   in case inputs of
        [] -> o
        i : xs -> processMoore (transition i) xs machine

processMoore' :: [i] -> Moore' i o -> o
processMoore' = flip cosieve

--------------------------------------------------------------------------------
-- Tensors

infixr 9 /\

(/\) :: Moore s i o -> Moore t i o' -> Moore (s, t) i (o, o')
(/\) m1 m2 = lmap split $ (Trifunctor.combine @_ @(,) @(,) @(,)) (m1, m2)

infixr 9 /+\

(/+\) :: Moore s i o -> Moore t i' o' -> Moore (s, t) (These i i') (o, o')
(/+\) m1 m2 = Trifunctor.combine @_ @(,) @These @(,) (m1, m2)

infixr 9 \/

(\/) :: Moore s i o -> Moore t i' o' -> Moore (s, t) (Either i i') (o, o')
(\/) m1 m2 = Trifunctor.combine @_ @(,) @Either @(,) (m1, m2)
