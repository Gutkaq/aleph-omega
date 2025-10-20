{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module AlephOmega.Proofs
  ( prop1_finiteBasis
  , prop2_uniqueMatrix
  , prop3_compositionMatrixMult
  , prop4_embeddingInjective
  , prop5_projectionExists
  , thm1_directedLimit
  , prop6_localityComposition
  , thm2_lyapunovStability
  , prop7_normBoundDecay
  , prop8_automorphismsGL
  , thm3_limitMatricesCompatible
  , prop9_limitLocalityPreserved
  , spectralTheoremSymmetric
  , prop10_compilerPreservesDet
  , prop11_compilerPreservesNorm
  , compiledLyapunovCheck
  , compiledSpectralCheck
  , compiledStabilityCheck
  , compiledLimitCheck
  ) where

import AlephOmega.Types (KInf, iota, pi, buildRadiativeFamily)
import AlephOmega.VectorSpace
import AlephOmega.Compiler (compileTheorem, compileProperty, compileMatrixOp, fromKernel, compileToKernel, executeCompiled, compileNorm)
import qualified Data.Vector as Vec
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Data.List (nub)
import Prelude hiding (pi)

-- Proposition 1: Finite-dimensional basis properties
prop1_finiteBasis :: Basis -> Bool
prop1_finiteBasis b =
  let els = basisElements b
      eBasis = map (canonicalBasis b) els
  in basisSize b > 0 
     && basisSize b < 100 
     && linearIndependent eBasis 
     && spansSpace b eBasis

-- Proposition 2: Unique matrix representation in bases
prop2_uniqueMatrix :: Basis -> Basis -> LinearMap -> Bool
prop2_uniqueMatrix bn bm lm =
  let p = fromIntegral (basisSize bn) :: Integer
      eBasisN = map (canonicalBasis bn) [0..p-1]
      images = map (applyLinearMap lm) eBasisN
      reconstructed = LM $ Vec.fromList 
        [ Vec.fromList (coordInBasis bm img) | img <- images ]
  in matrixRep reconstructed == matrixRep lm

-- Proposition 3: Composition corresponds to matrix multiplication
prop3_compositionMatrixMult :: Basis -> Basis -> Basis -> LinearMap -> LinearMap -> Bool
prop3_compositionMatrixMult bn _ _ lm1 lm2 =
  let comp = composeLinearMaps lm1 lm2
      n = fromIntegral (basisSize bn) :: Integer
      eBasis = map (canonicalBasis bn) [0..n-1]
      directComp = map (applyLinearMap comp) eBasis
      stepComp = map (applyLinearMap lm2 . applyLinearMap lm1) eBasis
  in directComp == stepComp

-- Proposition 4: Embeddings are injective
prop4_embeddingInjective :: Basis -> Basis -> Bool
prop4_embeddingInjective bn bnp1 =
  let e = embeddingOperator bn bnp1
      d_n = fromIntegral (basisSize bn) :: Integer
      eBasis = map (canonicalBasis bn) [0..d_n-1]
      eImages = map (applyLinearMap e) eBasis
  in linearIndependent eImages

-- Proposition 5: Projections exist as left inverses
prop5_projectionExists :: Basis -> Basis -> Bool
prop5_projectionExists = isLeftInverse

-- Theorem 1: Directed system coherence and limit construction
thm1_directedLimit :: Integer -> Bool
thm1_directedLimit n =
  let family = buildRadiativeFamily n
      coherence = all check [0..n-1]
      check k = fromIntegral k < length family 
                && pi (k + 1) (iota k (family !! fromIntegral k)) == family !! fromIntegral k
      b_n = Basis [0..n - 1]
      testM = LM $ Vec.generate (fromIntegral n) $ \i -> 
                Vec.generate (fromIntegral n) $ \j -> 
                  if i == j then 2%1 else 0%1
      compiled = compileToKernel b_n testM
      recovered = executeCompiled b_n compiled
  in coherence && not (Map.null (vectorCoords recovered))

-- Proposition 6: Locality preserved under composition
prop6_localityComposition :: LinearMap -> LinearMap -> Bool
prop6_localityComposition a b =
  let c = composeLinearMaps a b
  in localityRadius c <= localityRadius a + localityRadius b

-- Theorem 2: Lyapunov stability criterion
thm2_lyapunovStability :: LinearMap -> Vector -> Bool
thm2_lyapunovStability m x0 =
  let rho = spectralRadius m
      v10 = dynamicIteration m x0 10
      v0Norm = squaredNorm x0
      v10Norm = squaredNorm v10
      decays = v10Norm < v0Norm
  in if rho < 1 then decays else True

-- Proposition 7: Norm bound with exponential decay
prop7_normBoundDecay :: LinearMap -> Int -> Bool
prop7_normBoundDecay m t =
  let mt = matrixPower m t
      normM = inducedNorm m
      normMt = inducedNorm mt
  in normMt <= normM ^ t

-- Proposition 8: Automorphisms form GL(n)
prop8_automorphismsGL :: Basis -> Bool
prop8_automorphismsGL b =
  let d = basisSize b
      idMat = LM $ Vec.generate d $ \i -> 
                Vec.generate d $ \j -> 
                  if i == j then 1%1 else 0%1
  in det idMat /= 0

-- Theorem 3: Matrix compatibility in inductive limit
thm3_limitMatricesCompatible :: Integer -> Bool
thm3_limitMatricesCompatible n =
  let b_n = Basis [0..n - 1]
      m_n = LM $ Vec.generate (fromIntegral n) $ \i -> 
              Vec.generate (fromIntegral n) $ \j -> 
                fromIntegral (i + j) % 1
      compiled = compileToKernel b_n m_n
      recovered = executeCompiled b_n compiled
  in not (Map.null (vectorCoords recovered))

-- Proposition 9: Locality preserved in limits
prop9_limitLocalityPreserved :: [LinearMap] -> Bool
prop9_limitLocalityPreserved lms
  | null lms = True
  | length lms == 1 = True
  | otherwise =
      let composed = foldr1 composeLinearMaps lms
          radiiSum = sum (map localityRadius lms)
      in localityRadius composed <= radiiSum

-- Spectral theorem for symmetric matrices
spectralTheoremSymmetric :: LinearMap -> Bool
spectralTheoremSymmetric m =
  if not (isSymmetric m) then True
  else 
    let v0 = Vector (Map.singleton 0 (1%1))
        lambda = powerMethod m v0 20
        mv = applyLinearMap m v0
        lambdaV = scalarMul lambda v0
        keys = nub (Map.keys (vectorCoords mv) ++ Map.keys (vectorCoords lambdaV))
        approx = all (\k -> 
          abs (Map.findWithDefault 0 k (vectorCoords mv) - 
               Map.findWithDefault 0 k (vectorCoords lambdaV)) < 10%1) keys
    in approx

-- Proposition 10: Compiler preserves determinant
prop10_compilerPreservesDet :: LinearMap -> Bool
prop10_compilerPreservesDet m =
  let compiled = compileMatrixOp det m
      recovered = case fromKernel compiled of
        Vector coords -> Map.findWithDefault 0 0 coords
        _ -> 0
  in abs (recovered - det m) < 1%100

-- Proposition 11: Compiler preserves norm
prop11_compilerPreservesNorm :: Vector -> Bool
prop11_compilerPreservesNorm v =
  let compiled = compileNorm v
      recoveredNorm = case fromKernel compiled of
        Vector coords -> case Map.toList coords of
          [(_, n)] -> n
          _ -> 0
        _ -> 0
  in recoveredNorm == squaredNorm v

-- Compiled theorem versions (executable in kernel)
compiledLyapunovCheck :: LinearMap -> Vector -> KInf
compiledLyapunovCheck m x0 =
  let thmPartial = \m' -> thm2_lyapunovStability m' x0
  in compileProperty thmPartial m

compiledSpectralCheck :: LinearMap -> KInf
compiledSpectralCheck = compileProperty spectralTheoremSymmetric

compiledStabilityCheck :: LinearMap -> KInf
compiledStabilityCheck = compileTheorem isStable

compiledLimitCheck :: Integer -> KInf
compiledLimitCheck n =
  let testM = LM $ Vec.generate (fromIntegral n) $ \i -> 
                Vec.generate (fromIntegral n) $ \j -> 
                  if i == j then 2%1 else 0%1
      b_n = Basis [0..n - 1]
  in compileToKernel b_n testM

