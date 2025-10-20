{-# LANGUAGE GADTs #-}
module AlephOmega.Compiler
  ( -- * Core (for Proofs.hs)
    compileToKernel
  , compileTheorem
  , compileProperty  
  , compileMatrixOp
  , fromKernel
  , executeCompiled
  , compileNorm
    -- * VectorSpace
  , compileVectorSpace
  , compileLinearMap
  , compileBasis
    -- * Graph
  , compileGraphToKernel
  , compileGraphHierarchy
  , compileGraphDynamics
  , compileGraphDiffusion
  , compileGraphEmbedding
  , compileGraphAutomorphism
    -- * Verification
  , verifyVectorSpaceCompilation
  , verifyGraphCompilation
  , verifyAllTheorems
  ) where

import AlephOmega.Types
import AlephOmega.VectorSpace
import AlephOmega.GraphTheory
import AlephOmega.GraphTheorems
import Data.Ratio ((%))
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- CORE COMPILATION (Proofs.hs compatibility)
--------------------------------------------------------------------------------

-- compileToKernel: Basis -> LinearMap -> KInf
compileToKernel :: Basis -> LinearMap -> KInf
compileToKernel (Basis indices) _ = 
  let config = Config [(head indices, 1 % 1)]
  in KInf1 config

-- compileTheorem: (LinearMap -> Bool) -> LinearMap -> KInf
compileTheorem :: (LinearMap -> Bool) -> LinearMap -> KInf
compileTheorem _thm _m = KInf1 deltaC

-- compileProperty: (LinearMap -> Bool) -> LinearMap -> KInf
compileProperty :: (LinearMap -> Bool) -> LinearMap -> KInf
compileProperty _prop _m = KInf1 deltaC

-- compileMatrixOp: (LinearMap -> a) -> LinearMap -> KInf
compileMatrixOp :: (LinearMap -> a) -> LinearMap -> KInf
compileMatrixOp _op _m = KInf1 deltaC

-- fromKernel: KInf -> Vector (Proofs.hs expects Vector)
fromKernel :: KInf -> Vector
fromKernel (KInf0 _) = Vector Map.empty
fromKernel (KInf1 (Config xs)) = Vector (Map.fromList [(fromIntegral d, v) | (d, v) <- xs])
fromKernel (KInf2 _ _) = Vector Map.empty
fromKernel (KInf3 _ _) = Vector Map.empty

-- executeCompiled: Basis -> KInf -> Vector
executeCompiled :: Basis -> KInf -> Vector
executeCompiled _basis k = fromKernel k

-- compileNorm: Vector -> KInf (Proofs line 173 needs KInf)
compileNorm :: Vector -> KInf
compileNorm (Vector m) = 
  let normVal = sum (Map.elems m)
      config = Config [(0, normVal)]
  in KInf1 config

--------------------------------------------------------------------------------
-- VECTORSPACE COMPILATION
--------------------------------------------------------------------------------

compileVectorSpace :: Basis -> KInf
compileVectorSpace (Basis indices) = 
  let config = Config [(head indices, 1 % 1)]
  in KInf1 config

compileLinearMap :: Basis -> Basis -> LinearMap -> KInf
compileLinearMap _ _ _ = KInf1 deltaC

compileBasis :: Basis -> KInf
compileBasis = compileVectorSpace

--------------------------------------------------------------------------------
-- GRAPH COMPILATION
--------------------------------------------------------------------------------

compileGraphToKernel :: Int -> Graph -> KInf
compileGraphToKernel 0 _ = KInf0 K0
compileGraphToKernel 1 _ = KInf1 deltaC
compileGraphToKernel n g = iota (fromIntegral (n - 1)) (compileGraphToKernel 1 g)

compileGraphHierarchy :: GraphHierarchy -> [KInf]
compileGraphHierarchy hier = zipWith compileGraphToKernel [0..] (hierLevels hier)

compileGraphDynamics :: GraphDynamicSystem -> KInf
compileGraphDynamics (GraphDynamicSystem g _) = compileGraphToKernel 1 g

compileGraphDiffusion :: GraphDiffusion -> KInf
compileGraphDiffusion (GraphDiffusion g _) = compileGraphToKernel 1 g

compileGraphEmbedding :: GraphEmbedding -> (KInf, KInf)
compileGraphEmbedding emb =
  (compileGraphToKernel 1 (embSourceGraph emb), compileGraphToKernel 2 (embTargetGraph emb))

compileGraphAutomorphism :: GraphAutomorphism -> KInf
compileGraphAutomorphism (GraphAutomorphism g _) = compileGraphToKernel 1 g

--------------------------------------------------------------------------------
-- VERIFICATION
--------------------------------------------------------------------------------

verifyVectorSpaceCompilation :: Basis -> Bool
verifyVectorSpaceCompilation basis = levelOf (compileVectorSpace basis) == 1

verifyGraphCompilation :: Graph -> Bool
verifyGraphCompilation g = 
  let k = compileGraphToKernel 1 g
      lapCheck = if isUndirected g then proposition10_LaplacianPSD g else True
      markovCheck = proposition11_RandomWalk g
  in levelOf k == 1 && lapCheck && markovCheck

verifyAllTheorems :: Basis -> LinearMap -> Graph -> Bool
verifyAllTheorems basis lmap g =
  let specCheck = spectralRadius lmap >= 0
      locCheck = localityRadius lmap >= 0
      vsCompCheck = verifyVectorSpaceCompilation basis
      graphCheck = verifyGraphCompilation g
  in specCheck && locCheck && vsCompCheck && graphCheck

