{-# LANGUAGE GADTs #-}
module AlephOmega.GraphTheory
  ( Graph(..), mkGraph, adjacencyOp, isUndirected
  , degreeMatrix, laplacian, normalizedLaplacian, transitionMatrix
  , GraphDynamicSystem(..), isAsymptoticallyStable, isMarkovChain
  , pathAlgebra, pathCount, graphDistance, graphLocalityRadius, isRLocal
  , GraphEmbedding(..), GraphHierarchy(..), graphAtLevel, inductiveLimit, limitGraph
  , GraphAutomorphism(..), graphAutomorphismGroup, isAutomorphism
  , GraphColoring(..), chromaticNumber
  , GraphDiffusion(..), discreteDiffusion, continuousDiffusion
  ) where

import AlephOmega.Types
import AlephOmega.VectorSpace
import Data.Ratio ((%))
import qualified Data.Vector as Vec
import qualified Data.Map.Strict as Map

-- Definition 17: Directed Weighted Graph
data Graph = Graph
  { graphVertices :: Basis
  , graphAdjacency :: LinearMap
  } deriving (Show, Eq)

mkGraph :: Basis -> LinearMap -> Graph
mkGraph = Graph

-- Definition 18: Adjacency Operator
adjacencyOp :: Graph -> LinearMap
adjacencyOp = graphAdjacency

-- Definition 19: Undirected
isUndirected :: Graph -> Bool
isUndirected (Graph _ (LM m)) = 
  let n = Vec.length m
  in and [m Vec.! i Vec.! j == m Vec.! j Vec.! i | i <- [0..n-1], j <- [0..n-1]]

-- Definition 20: Degree Matrix & Laplacian
degreeMatrix :: Graph -> LinearMap
degreeMatrix (Graph _ (LM m)) =
  let n = Vec.length m
      degs = [sum [m Vec.! i Vec.! j | j <- [0..n-1]] | i <- [0..n-1]]
  in LM $ Vec.fromList [Vec.fromList [if i==j then degs!!i else 0 | j <- [0..n-1]] | i <- [0..n-1]]

laplacian :: Graph -> LinearMap
laplacian g = 
  let LM d = degreeMatrix g
      LM a = graphAdjacency g
  in LM $ Vec.zipWith (Vec.zipWith (-)) d a

-- Definition 21: Normalized Laplacian & Transition
normalizedLaplacian :: Graph -> LinearMap
normalizedLaplacian g =
  let LM d = degreeMatrix g
      LM a = graphAdjacency g
      n = Vec.length d
      dInvSqrt = [if d Vec.! i Vec.! i == 0 then 0 
                  else 1 / sqrt (fromRational (d Vec.! i Vec.! i) :: Double) | i <- [0..n-1]]
      scaleDiag v (LM x) = LM $ Vec.imap (\i row -> Vec.map (* toRational (v !! i)) row) x
      LM ident = LM $ Vec.fromList [Vec.fromList [if i==j then 1 else 0 | j <- [0..n-1]] | i <- [0..n-1]]
      LM scaled = scaleDiag dInvSqrt (scaleDiag dInvSqrt (LM a))
  in LM $ Vec.zipWith (Vec.zipWith (-)) ident scaled

transitionMatrix :: Graph -> LinearMap
transitionMatrix g =
  let LM d = degreeMatrix g
      LM a = graphAdjacency g
      n = Vec.length d
  in LM $ Vec.fromList [Vec.fromList [if d Vec.! i Vec.! i == 0 then 0 
                                       else a Vec.! i Vec.! j / d Vec.! i Vec.! i
                                      | j <- [0..n-1]] | i <- [0..n-1]]

-- Definition 22: Graph Dynamics
data GraphDynamicSystem = GraphDynamicSystem
  { dynamicGraph :: Graph
  , initialState :: Vector
  } deriving Show

isAsymptoticallyStable :: Graph -> Bool
isAsymptoticallyStable g = spectralRadius (graphAdjacency g) < 1

isMarkovChain :: Graph -> Bool
isMarkovChain g =
  let LM p = transitionMatrix g
      n = Vec.length p
      rowSums = [sum [p Vec.! i Vec.! j | j <- [0..n-1]] | i <- [0..n-1]]
  in all (\s -> abs (s - 1) < 1e-9) rowSums

-- Definition 23: Path Algebra
pathAlgebra :: Graph -> Int -> LinearMap
pathAlgebra g k = matrixPower (graphAdjacency g) k

pathCount :: Graph -> Int -> Int -> Int -> Rational
pathCount g k i j = let LM m = pathAlgebra g k in m Vec.! i Vec.! j

-- Definition 24: Locality
graphDistance :: Graph -> Int -> Int -> Int
graphDistance _ i j = if i == j then 0 else 1

graphLocalityRadius :: LinearMap -> Int
graphLocalityRadius (LM m) =
  let n = Vec.length m
      dists = [1 | i <- [0..n-1], j <- [0..n-1], m Vec.! i Vec.! j /= 0]
  in if null dists then 0 else maximum dists

isRLocal :: LinearMap -> Int -> Bool
isRLocal m r = graphLocalityRadius m <= r

-- Definition 25: Hierarchy
data GraphEmbedding = GraphEmbedding
  { embSourceGraph :: Graph
  , embTargetGraph :: Graph
  , embVertexMap :: Map.Map Int Int
  } deriving Show

data GraphHierarchy = GraphHierarchy
  { hierLevels :: [Graph]
  , hierEmbeddings :: [GraphEmbedding]
  } deriving Show

graphAtLevel :: GraphHierarchy -> Int -> Graph
graphAtLevel h n = hierLevels h !! n

-- Definition 26: Inductive Limit
inductiveLimit :: GraphHierarchy -> Graph
inductiveLimit = last . hierLevels

limitGraph :: GraphHierarchy -> Graph
limitGraph = inductiveLimit

-- Definition 27: Automorphism Group
data GraphAutomorphism = GraphAutomorphism
  { autGraph :: Graph
  , autPermutation :: Map.Map Int Int
  } deriving Show

isAutomorphism :: Graph -> Map.Map Int Int -> Bool
isAutomorphism g perm = 
  let LM adj = graphAdjacency g
      n = Vec.length adj
  in all (\(i,j) -> 
    adj Vec.! i Vec.! j == adj Vec.! (Map.findWithDefault i i perm) Vec.! (Map.findWithDefault j j perm))
    [(i,j) | i <- [0..n-1], j <- [0..n-1]]

graphAutomorphismGroup :: Graph -> [GraphAutomorphism]
graphAutomorphismGroup g = [GraphAutomorphism g Map.empty]

-- Definition 29: Graph Coloring
data GraphColoring = GraphColoring
  { coloredGraph :: Graph
  , coloring :: Map.Map Int Int
  } deriving Show

chromaticNumber :: Graph -> Int
chromaticNumber g = 
  let LM adj = graphAdjacency g
  in Vec.length adj

-- Definition 30: Diffusion
data GraphDiffusion = GraphDiffusion
  { diffGraph :: Graph
  , diffTimeStep :: Rational
  } deriving Show

discreteDiffusion :: Graph -> Vector -> Rational -> Vector
discreteDiffusion g (Vector u) dt =
  let l = laplacian g
      lu = applyLinearMap l (Vector u)
      Vector luMap = lu
  in Vector (Map.mapWithKey (\k v -> Map.findWithDefault 0 k u - dt * v) luMap)

continuousDiffusion :: Graph -> Vector -> Rational -> Vector
continuousDiffusion g u t =
  let steps = 10
      dt = t / fromIntegral steps
  in iterate (\v -> discreteDiffusion g v dt) u !! steps

