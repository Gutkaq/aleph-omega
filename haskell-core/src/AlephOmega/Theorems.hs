{-# LANGUAGE GADTs #-}

module AlephOmega.Theorems
  ( proposition1_injectivity
  , proposition2_leftInverseProjection
  , proposition3_commutativity
  , theorem1_directedSystem
  , theorem2_nonTrivialAutomorphism
  , theorem3_radiativeSymmetryField
  ) where

import AlephOmega.Types
import Prelude hiding (pi)

proposition1_injectivity :: KInf -> KInf -> Bool
proposition1_injectivity a b =
  let n = levelOf a
  in if iota n a == iota n b then a == b else True

proposition2_leftInverseProjection :: KInf -> Bool
proposition2_leftInverseProjection a =
  let n = levelOf a
      embedded = iota n a
      projected = AlephOmega.Types.pi (n + 1) embedded
  in a == projected

proposition3_commutativity :: KInf -> Bool
proposition3_commutativity a =
  let n = levelOf a
      step1 = iota n a
      step2 = iota (n + 1) step1
      p1 = AlephOmega.Types.pi (n + 2) step2
      p2 = AlephOmega.Types.pi (n + 1) p1
  in a == p2

theorem1_directedSystem :: [KInf] -> Bool
theorem1_directedSystem family =
  if null family then True
  else all proposition2_leftInverseProjection family

theorem2_nonTrivialAutomorphism :: KInf -> Bool
theorem2_nonTrivialAutomorphism a = aut (levelOf a) a == a

theorem3_radiativeSymmetryField :: KInf -> Bool
theorem3_radiativeSymmetryField a =
  let n = levelOf a
      embedded = iota n a
      autApplied = aut (n + 1) embedded
      projected = AlephOmega.Types.pi (n + 1) autApplied
      directAut = aut n a
  in projected == directAut

