{-# LANGUAGE GADTs, DeriveGeneric #-}

module AlephOmega.VectorSpace
  ( FieldElement
  , Basis(..)
  , Vector(..)
  , LinearMap(..)
  , AutomorphismGroup(..)
  , Spectrum(..)
  , DynamicSystem(..)
  , basisSize
  , canonicalBasis
  , embeddingOperator
  , projectionOperator
  , composeLinearMaps
  , isLeftInverse
  , applyLinearMap
  , scalarMul
  , localityRadius
  , isLocal
  , permutationAutomorphism
  , automorphismGroup
  , spectrum
  , eigenspace
  , spectralRadius
  , dynamicIteration
  , isStable
  ) where

import AlephOmega.Types (Direction)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vec
import GHC.Generics (Generic)
import Data.Ratio ()

-- Definition 1: Field Element (F = ℚ)
type FieldElement = Rational

-- Definition 3: Finite Basis Selection
data Basis = Basis { basisElements :: [Direction] }
  deriving (Show, Eq, Generic)

basisSize :: Basis -> Int
basisSize = length . basisElements

-- Definition 4: Vector Space V_n = F^{B_n}
newtype Vector = Vector { vectorCoords :: Map.Map Direction FieldElement }
  deriving (Show, Eq, Generic)

instance Num Vector where
  v1 + v2 = Vector (Map.unionWith (+) (vectorCoords v1) (vectorCoords v2))
  v1 - v2 = Vector (Map.unionWith (-) (vectorCoords v1) (vectorCoords v2))
  negate (Vector m) = Vector (fmap negate m)
  (*) = undefined
  abs = undefined
  signum = undefined
  fromInteger i = Vector (Map.singleton 0 (fromIntegral i))

scalarMul :: FieldElement -> Vector -> Vector
scalarMul s (Vector m) = Vector (fmap (s *) m)

-- Definition 5: Canonical Basis
canonicalBasis :: Basis -> Direction -> Vector
canonicalBasis (Basis els) a
  | a `elem` els = Vector (Map.singleton a 1)
  | otherwise = Vector Map.empty

-- Definition 6-7: Linear Operator as Matrix
newtype LinearMap = LM { matrixRep :: Vec.Vector (Vec.Vector FieldElement) }
  deriving (Show, Eq, Generic)

-- Definition 8: Composition
composeLinearMaps :: LinearMap -> LinearMap -> LinearMap
composeLinearMaps (LM a) (LM b) =
    LM $ Vec.generate (Vec.length b) $ \i ->
         Vec.generate (Vec.length (a Vec.! 0)) $ \j ->
           sum [ (b Vec.! i Vec.! k) * (a Vec.! k Vec.! j) | k <- [0..Vec.length a - 1] ]

-- Definition 9: Embedding Operator E_n
embeddingOperator :: Basis -> Basis -> LinearMap
embeddingOperator bn bnp1 =
  let d_n = basisSize bn
      d_np1 = basisSize bnp1
      mat = Vec.generate d_np1 $ \i -> Vec.generate d_n $ \j -> if i == j && j < d_n then 1 else 0
  in LM mat

-- Definition 10: Projection Operator P_{n+1}
projectionOperator :: Basis -> Basis -> LinearMap
projectionOperator bnp1 bn =
  let d_n = basisSize bn
      d_np1 = basisSize bnp1
      mat = Vec.generate d_n $ \i -> Vec.generate d_np1 $ \j -> if i == j && j < d_n then 1 else 0
  in LM mat

-- Verify P ∘ E = id (Definition 11: Directed System property)
isLeftInverse :: Basis -> Basis -> Bool
isLeftInverse bn bnp1 =
  let e = embeddingOperator bn bnp1
      p = projectionOperator bnp1 bn
      d = basisSize bn
      idMat = Vec.generate d $ \i -> Vec.generate d $ \j -> if i == j then 1 else 0
  in matrixRep (composeLinearMaps e p) == idMat

-- Apply linear map to vector
applyLinearMap :: LinearMap -> Vector -> Vector
applyLinearMap (LM mat) (Vector coords) =
  let d_in = Vec.length (mat Vec.! 0)
      coordsList = [Map.findWithDefault 0 (fromIntegral i) coords | i <- [0..d_in - 1]]
      resultList = [ sum [ (mat Vec.! i Vec.! j) * (coordsList !! j) | j <- [0..d_in - 1] ] | i <- [0..Vec.length mat - 1] ]
  in Vector (Map.fromList (zip [0..] resultList))

-- Definition 13: Local Linear Operator
basisDistance :: Direction -> Direction -> Int
basisDistance d1 d2 = fromIntegral (abs (d1 - d2))

localityRadius :: LinearMap -> Int
localityRadius (LM mat) =
  let nonZeros = [ (i, j) | i <- [0..Vec.length mat - 1]
                           , j <- [0..Vec.length (mat Vec.! i) - 1]
                           , mat Vec.! i Vec.! j /= 0 ]
  in if null nonZeros then 0
     else maximum [basisDistance (fromIntegral i) (fromIntegral j) | (i, j) <- nonZeros]

isLocal :: LinearMap -> Int -> Bool
isLocal lm r = localityRadius lm <= r

-- Definition 14: Automorphism Group
newtype AutomorphismGroup = AutGroup [LinearMap]
  deriving (Show, Eq, Generic)

permutationAutomorphism :: [Direction] -> [Direction] -> LinearMap
permutationAutomorphism oldBasis sigma =
  let n = length oldBasis
      mat = Vec.generate n $ \i -> Vec.generate n $ \j ->
              if sigma !! j == oldBasis !! i then 1 else 0
  in LM mat

automorphismGroup :: Basis -> AutomorphismGroup
automorphismGroup b =
  let els = basisElements b
      idMap = LM $ Vec.generate (length els) $ \i -> Vec.generate (length els) $ \j -> if i == j then 1 else 0
  in AutGroup [idMap]

-- Definition 15: Spectral Data
newtype Spectrum = Spec [FieldElement]
  deriving (Show, Eq, Generic)

spectrum :: LinearMap -> Spectrum
spectrum (LM mat) =
  let n = Vec.length mat
      tr = sum [ mat Vec.! i Vec.! i | i <- [0..n-1] ]
  in Spec [tr / fromIntegral n]

eigenspace :: LinearMap -> FieldElement -> [Vector]
eigenspace _ _ = []

spectralRadius :: LinearMap -> FieldElement
spectralRadius lm =
  let Spec eigenvalues = spectrum lm
  in if null eigenvalues then 0 else maximum (map abs eigenvalues)

-- Definition 16: Dynamics
data DynamicSystem = DS
  { dsMatrix :: LinearMap
  , dsInitial :: Vector
  , dsHistory :: [Vector]
  } deriving (Show, Generic)

dynamicIteration :: LinearMap -> Vector -> Int -> Vector
dynamicIteration _ v 0 = v
dynamicIteration m v t = dynamicIteration m (applyLinearMap m v) (t - 1)

isStable :: LinearMap -> Bool
isStable lm = spectralRadius lm < 1

