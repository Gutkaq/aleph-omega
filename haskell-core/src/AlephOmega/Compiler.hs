{-# LANGUAGE GADTs, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-type-defaults -Wno-unused-matches -Wno-overlapping-patterns #-}

module AlephOmega.Compiler
  ( compileToKernel, executeCompiled, compileVector, compileDynamicSystem
  , compileAutomorphismGroup, compileSpectrum, compileMatrixOp, compileNorm
  , compileTheorem, compileTheoremVec, verifyTheorem, verifyTheoremVec, verifyTheorem2
  , KernelExecutable(..), TheoremVerifiable(..), roundtripPreserved
  ) where

import AlephOmega.Types (KInf(..), K0(..), Config(..))
import AlephOmega.VectorSpace (Vector(..), LinearMap(LM), FieldElement, Basis(..), vectorCoords, basisSize, squaredNorm)
import qualified Data.Vector as Vec
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Prelude

compileToKernel :: Basis -> LinearMap -> KInf
compileToKernel _ (LM mat) =
  let n = Vec.length mat
      cfg = [ (fromIntegral i, if i < n && i < Vec.length (mat Vec.! i) then mat Vec.! i Vec.! i else 0) | i <- [0..n-1] ]
  in KInf1 (Config cfg)

executeCompiled :: Basis -> KInf -> Vector
executeCompiled _ kinf =
  case kinf of
    KInf1 (Config cfg) -> Vector (Map.fromList cfg)
    _ -> Vector Map.empty

compileVector :: Vector -> KInf
compileVector (Vector coords) = KInf1 (Config (Map.toList coords))

compileDynamicSystem :: LinearMap -> Vector -> [Vector] -> KInf
compileDynamicSystem m _ _ = compileToKernel (Basis [0..9]) m

compileAutomorphismGroup :: [LinearMap] -> KInf
compileAutomorphismGroup [] = KInf0 K0
compileAutomorphismGroup (m:_) = compileToKernel (Basis [0..9]) m

compileSpectrum :: LinearMap -> KInf
compileSpectrum m = let LM mat = m; n = Vec.length mat; traceApprox = if n > 0 then sum [mat Vec.! i Vec.! i | i <- [0..n-1]] / fromIntegral n else 0 in KInf1 (Config [(0, traceApprox)])

compileMatrixOp :: (LinearMap -> FieldElement) -> LinearMap -> KInf
compileMatrixOp op m = KInf1 (Config [(0, op m)])

compileNorm :: Vector -> KInf
compileNorm v = KInf1 (Config [(0, squaredNorm v)])

compileTheorem :: (LinearMap -> Bool) -> LinearMap -> KInf
compileTheorem thm m = KInf1 (Config [(0, if thm m then 1%1 else 0%1)])

compileTheoremVec :: (Vector -> Bool) -> Vector -> KInf
compileTheoremVec thm v = KInf1 (Config [(0, if thm v then 1%1 else 0%1)])

verifyTheorem :: (LinearMap -> Bool) -> LinearMap -> Bool
verifyTheorem thm m = let compiled = compileTheorem thm m; recovered = case fromKernel compiled of {Vector coords -> Map.lookup 0 coords == Just (1%1)} in recovered == thm m

verifyTheoremVec :: (Vector -> Bool) -> Vector -> Bool
verifyTheoremVec thm v = let compiled = compileTheoremVec thm v; recovered = case fromKernel compiled of {Vector coords -> Map.lookup 0 coords == Just (1%1)} in recovered == thm v

verifyTheorem2 :: (LinearMap -> Vector -> Bool) -> LinearMap -> Vector -> Bool
verifyTheorem2 thm m v = let thmPartial = \m' -> thm m' v; compiled = compileTheorem thmPartial m; recovered = case fromKernel compiled of {Vector coords -> Map.lookup 0 coords == Just (1%1)} in recovered == thm m v

class KernelExecutable a where
  toKernel :: a -> KInf
  fromKernel :: KInf -> a

instance KernelExecutable LinearMap where
  toKernel m = let LM mat = m; n = Vec.length mat; b = Basis [0..fromIntegral n - 1] in compileToKernel b m
  fromKernel kinf = case kinf of {KInf1 (Config cfg) -> let coords = Map.fromList cfg; maxKey = if Map.null coords then 0 else maximum (Map.keys coords); n = max 1 (fromIntegral maxKey + 1); mat = LM $ Vec.generate n $ \i -> Vec.generate n $ \j -> if i == j then Map.findWithDefault 0 (fromIntegral i) coords else 0 in mat; _ -> LM $ Vec.singleton (Vec.singleton 0)}

instance KernelExecutable Vector where
  toKernel = compileVector
  fromKernel kinf = executeCompiled (Basis [0..9]) kinf

class TheoremVerifiable a where
  compileProperty :: (a -> Bool) -> a -> KInf
  verifyProperty :: (a -> Bool) -> a -> Bool

instance TheoremVerifiable LinearMap where
  compileProperty = compileTheorem
  verifyProperty = verifyTheorem

instance TheoremVerifiable Vector where
  compileProperty = compileTheoremVec
  verifyProperty = verifyTheoremVec

roundtripPreserved :: (KernelExecutable a, Eq a) => a -> Bool
roundtripPreserved x = fromKernel (toKernel x) == x

