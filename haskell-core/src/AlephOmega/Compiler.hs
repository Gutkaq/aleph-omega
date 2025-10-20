{-# LANGUAGE GADTs #-}

module AlephOmega.Compiler
  ( compileToKernel
  , compileVectorToKernel
  , compileDynamicsToKernel
  , executeCompiled
  , executeVectorFromKernel
  , executeDynamicsFromKernel
  ) where

import Prelude hiding (pi)
import AlephOmega.Types (KInf(..), Config(..), iota, pi)
import AlephOmega.VectorSpace (LinearMap(..), Vector(..), Basis(..), basisSize, DynamicSystem(..), dynamicIteration)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vec

-- Compile LinearMap to KInf by encoding matrix as Config
compileToKernel :: Basis -> LinearMap -> KInf
compileToKernel b (LM mat) =
  let d = basisSize b
      configPairs = [ (fromIntegral (i*d + j), mat Vec.! i Vec.! j)
                    | i <- [0..d-1], j <- [0..d-1], mat Vec.! i Vec.! j /= 0 ]
      k1 = KInf1 (Config configPairs)
  in iota 1 k1

-- Compile Vector to KInf (Config directly represents coordinates)
compileVectorToKernel :: Vector -> KInf
compileVectorToKernel (Vector coords) =
  let configPairs = Map.toList coords
      k1 = KInf1 (Config configPairs)
  in k1

-- Compile DynamicSystem: Encode both matrix and initial vector
compileDynamicsToKernel :: Basis -> DynamicSystem -> (KInf, KInf)
compileDynamicsToKernel b (DS mat v0 _) =
  (compileToKernel b mat, compileVectorToKernel v0)

-- Execute compiled LinearMap: Extract matrix from KInf and recover first column as Vector
executeCompiled :: Basis -> KInf -> Vector
executeCompiled b k =
  case pi 2 k of
    KInf1 (Config pairs) ->
      let d = basisSize b
          coordMap = Map.fromListWith (+)
                     [ (fromIntegral j, val)
                     | (flatIdx, val) <- pairs,
                       let (_, j) = divMod (fromIntegral flatIdx) d ]
      in Vector coordMap
    _ -> Vector Map.empty

-- Execute compiled Vector: Direct extraction from KInf1
executeVectorFromKernel :: KInf -> Vector
executeVectorFromKernel (KInf1 (Config pairs)) = Vector (Map.fromList pairs)
executeVectorFromKernel _ = Vector Map.empty

-- Execute compiled Dynamics: Apply matrix to vector iteratively
executeDynamicsFromKernel :: Basis -> (KInf, KInf) -> Int -> Vector
executeDynamicsFromKernel b (kMat, kVec) steps =
  let mat = reconstructMatrix b kMat
      v0 = executeVectorFromKernel kVec
  in dynamicIteration mat v0 steps

-- Helper: Reconstruct LinearMap from compiled KInf
reconstructMatrix :: Basis -> KInf -> LinearMap
reconstructMatrix b k =
  case pi 2 k of
    KInf1 (Config pairs) ->
      let d = basisSize b
          mat = Vec.generate d $ \i -> Vec.generate d $ \j ->
                  let idx = fromIntegral (i*d + j)
                  in Map.findWithDefault 0 idx (Map.fromList pairs)
      in LM mat
    _ -> LM Vec.empty

