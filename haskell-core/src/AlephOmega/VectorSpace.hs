{-# LANGUAGE GADTs, DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-type-defaults -Wno-incomplete-uni-patterns -Wno-unused-matches #-}

module AlephOmega.VectorSpace
  ( FieldElement, Basis(..), Vector(..), LinearMap(..), AutomorphismGroup(..)
  , Spectrum(..), DynamicSystem(..), basisSize, canonicalBasis, embeddingOperator
  , projectionOperator, composeLinearMaps, isLeftInverse, applyLinearMap, scalarMul
  , localityRadius, isLocal, permutationAutomorphism, automorphismGroup, spectrum
  , eigenspace, spectralRadius, dynamicIteration, isStable, linearIndependent
  , spansSpace, inducedNorm, rowReduce, det, squaredNorm, convergesToZero
  , isSymmetric, coordInBasis, matrixPower, powerMethod
  ) where

import AlephOmega.Types (Direction)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vec
import GHC.Generics (Generic)
import Data.Ratio ()
import Data.List (nub, transpose)
import Prelude hiding (pi)

type FieldElement = Rational
data Basis = Basis { basisElements :: [Direction] } deriving (Show, Eq, Generic)
basisSize :: Basis -> Int
basisSize = length . basisElements
newtype Vector = Vector { vectorCoords :: Map.Map Direction FieldElement } deriving (Show, Eq, Generic)

instance Num Vector where
  v1 + v2 = Vector (Map.unionWith (+) (vectorCoords v1) (vectorCoords v2))
  v1 - v2 = Vector (Map.unionWith (-) (vectorCoords v1) (vectorCoords v2))
  negate (Vector m) = Vector (fmap negate m)
  (*) = undefined; abs = undefined; signum = undefined
  fromInteger i = Vector (Map.singleton 0 (fromIntegral i))

scalarMul :: FieldElement -> Vector -> Vector
scalarMul s (Vector m) = Vector (fmap (s *) m)

canonicalBasis :: Basis -> Direction -> Vector
canonicalBasis (Basis els) a = if a `elem` els then Vector (Map.singleton a 1) else Vector Map.empty

newtype LinearMap = LM { matrixRep :: Vec.Vector (Vec.Vector FieldElement) } deriving (Show, Eq, Generic)

composeLinearMaps :: LinearMap -> LinearMap -> LinearMap
composeLinearMaps (LM a) (LM b) = LM $ Vec.generate (Vec.length b) $ \i -> Vec.generate (Vec.length (a Vec.! 0)) $ \j -> sum [ (b Vec.! i Vec.! k) * (a Vec.! k Vec.! j) | k <- [0..Vec.length a - 1] ]

embeddingOperator :: Basis -> Basis -> LinearMap
embeddingOperator bn bnp1 = let d_n = basisSize bn; d_np1 = basisSize bnp1 in LM $ Vec.generate d_np1 $ \i -> Vec.generate d_n $ \j -> if i == j && j < d_n then 1 else 0

projectionOperator :: Basis -> Basis -> LinearMap
projectionOperator bnp1 bn = let d_n = basisSize bn; d_np1 = basisSize bnp1 in LM $ Vec.generate d_n $ \i -> Vec.generate d_np1 $ \j -> if i == j && j < d_n then 1 else 0

isLeftInverse :: Basis -> Basis -> Bool
isLeftInverse bn bnp1 = let e = embeddingOperator bn bnp1; p = projectionOperator bnp1 bn; d = basisSize bn; idMat = Vec.generate d $ \i -> Vec.generate d $ \j -> if i == j then 1 else 0 in matrixRep (composeLinearMaps e p) == idMat

applyLinearMap :: LinearMap -> Vector -> Vector
applyLinearMap (LM mat) (Vector coords) = let d_in = if Vec.null mat then 0 else Vec.length (mat Vec.! 0); coordsList = [Map.findWithDefault 0 (fromIntegral i) coords | i <- [0..d_in - 1]]; resultList = [ sum [ (mat Vec.! i Vec.! j) * (coordsList !! j) | j <- [0..d_in - 1] ] | i <- [0..Vec.length mat - 1] ] in Vector (Map.fromList (zip [0..] resultList))

basisDistance :: Direction -> Direction -> Int
basisDistance d1 d2 = fromIntegral (abs (d1 - d2))

localityRadius :: LinearMap -> Int
localityRadius (LM mat) = let nonZeros = [ (i, j) | i <- [0..Vec.length mat - 1], j <- [0..Vec.length (mat Vec.! i) - 1], mat Vec.! i Vec.! j /= 0 ] in if null nonZeros then 0 else maximum [basisDistance (fromIntegral i) (fromIntegral j) | (i, j) <- nonZeros]

isLocal :: LinearMap -> Int -> Bool
isLocal lm r = localityRadius lm <= r

newtype AutomorphismGroup = AutGroup [LinearMap] deriving (Show, Eq, Generic)
permutationAutomorphism :: [Direction] -> [Direction] -> LinearMap
permutationAutomorphism oldBasis sigma = let n = length oldBasis in LM $ Vec.generate n $ \i -> Vec.generate n $ \j -> if sigma !! j == oldBasis !! i then 1 else 0

automorphismGroup :: Basis -> AutomorphismGroup
automorphismGroup b = let els = basisElements b; idMap = LM $ Vec.generate (length els) $ \i -> Vec.generate (length els) $ \j -> if i == j then 1 else 0 in AutGroup [idMap]

newtype Spectrum = Spec [FieldElement] deriving (Show, Eq, Generic)
spectrum :: LinearMap -> Spectrum
spectrum (LM mat) = let n = Vec.length mat; tr = if n == 0 then 0 else sum [ mat Vec.! i Vec.! i | i <- [0..n-1] ] in Spec [tr / fromIntegral (max 1 n)]

eigenspace :: LinearMap -> FieldElement -> [Vector]
eigenspace _ _ = []

spectralRadius :: LinearMap -> FieldElement
spectralRadius lm = let Spec eigenvalues = spectrum lm in if null eigenvalues then 0 else maximum (map abs eigenvalues)

data DynamicSystem = DS { dsMatrix :: LinearMap, dsInitial :: Vector, dsHistory :: [Vector] } deriving (Show, Generic)
dynamicIteration :: LinearMap -> Vector -> Int -> Vector
dynamicIteration _ v 0 = v
dynamicIteration m v t = dynamicIteration m (applyLinearMap m v) (t - 1)

isStable :: LinearMap -> Bool
isStable lm = spectralRadius lm < 1

squaredNorm :: Vector -> FieldElement
squaredNorm (Vector coords) = sum [ x * x | x <- Map.elems coords ]

inducedNorm :: LinearMap -> FieldElement
inducedNorm (LM mat) = if Vec.null mat then 0 else maximum [ sum (map abs (Vec.toList row)) | row <- Vec.toList mat ]

coordInBasis :: Basis -> Vector -> [FieldElement]
coordInBasis (Basis els) (Vector coords) = [ Map.findWithDefault 0 d coords | d <- els ]

det :: LinearMap -> FieldElement
det (LM mat) = detList (map Vec.toList (Vec.toList mat)) where
  detList m | null m = 1 | length m == 1 = if null (head m) then 0 else head (head m) | length m == 2 = let [[a,b],[c,d]] = take 2 (map (take 2) m) in a*d - b*c | otherwise = sum [ (if even j then 1 else -1) * (head m !! j) * detList (minor m j) | j <- [0..length (head m) - 1] ]; minor m j = [ [ m !! x !! y | y <- [0..length (head m) - 1], y /= j ] | x <- [1..length m - 1] ]

rowReduce :: LinearMap -> Int
rowReduce (LM mat) = let rows = map Vec.toList (Vec.toList mat); numRows = length rows; numCols = if numRows > 0 then length (head rows) else 0 in if numRows == 0 || numCols == 0 then 0 else gaussian rows 0 0 numRows numCols where gaussian rs row col n m | row >= n || col >= m = row | otherwise = case findPivot rs row col n of {Nothing -> gaussian rs row (col + 1) n m; Just p -> gaussian (eliminate rs col p n) (row + 1) (col + 1) n m}; findPivot rs r c n = if any (\i -> rs !! i !! c /= 0) [r..n-1] then Just (head [ i | i <- [r..n-1], rs !! i !! c /= 0 ]) else Nothing; eliminate rs c p n = let pivot = rs !! p !! c; normRow = map (/ pivot) (rs !! p) in [ if i == p then normRow else zipWith (\x y -> x - y * (rs !! i !! c)) (rs !! i) normRow | i <- [0..n-1] ]

linearIndependent :: [Vector] -> Bool
linearIndependent vecs | null vecs = True | otherwise = let els = nub (concatMap (Map.keys . vectorCoords) vecs); b = Basis els; matList = [ coordInBasis b v | v <- vecs ]; mat = LM $ Vec.fromList [ Vec.fromList row | row <- transpose matList ] in rowReduce mat == length vecs

spansSpace :: Basis -> [Vector] -> Bool
spansSpace b basisVecs = linearIndependent basisVecs && basisSize b == length basisVecs

convergesToZero :: LinearMap -> Vector -> Int -> Bool
convergesToZero m x0 steps = squaredNorm (dynamicIteration m x0 steps) == 0

isSymmetric :: LinearMap -> Bool
isSymmetric (LM mat) = let n = Vec.length mat in all (\(i,j) -> mat Vec.! i Vec.! j == mat Vec.! j Vec.! i) [ (i,j) | i <- [0..n-1], j <- [0..n-1] ]

matrixPower :: LinearMap -> Int -> LinearMap
matrixPower _ 0 = LM $ Vec.singleton (Vec.singleton 1)
matrixPower m 1 = m
matrixPower m e | even e = let half = matrixPower m (e `div` 2) in composeLinearMaps half half | otherwise = let half = matrixPower m (e `div` 2) in composeLinearMaps (composeLinearMaps half half) m

powerMethod :: LinearMap -> Vector -> Int -> FieldElement
powerMethod _ _ 0 = 0
powerMethod m v0 iters | squaredNorm v0 == 0 = 0 | otherwise = let v1 = applyLinearMap m v0; normSq = squaredNorm v1 in if normSq == 0 then 0 else let keys = nub (Map.keys (vectorCoords v1) ++ Map.keys (vectorCoords v0)); rayleigh = sum [ Map.findWithDefault 0 k (vectorCoords v1) * Map.findWithDefault 0 k (vectorCoords v0) | k <- keys ] / squaredNorm v0 in if iters == 1 then rayleigh else powerMethod m v1 (iters - 1)

