import AlephOmega.Types
import AlephOmega.Theorems
import AlephOmega.VectorSpace
import AlephOmega.Compiler
import AlephOmega.Proofs
import Prelude hiding (pi)
import Data.Ratio ((%))
import qualified Data.Vector as Vec
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  let k0 = KInf0 K0
      k1a = KInf1 (Config [(0, 1 % 1)])
      k2a = iota 1 k1a
      fam2 = buildRadiativeFamily 2
      
      b1 = Basis [0]
      b2 = Basis [0, 1]
      b3 = Basis [0, 1, 2]
      
      mat1 = LM $ Vec.fromList [Vec.fromList [2%1, 0], Vec.fromList [0, 3%1]]
      mat2 = LM $ Vec.fromList [Vec.fromList [1%1, 1%1], Vec.fromList [0, 1%1]]
      local1 = LM $ Vec.fromList [Vec.fromList [1%1, 0], Vec.fromList [0, 1%1]]
      local2 = LM $ Vec.fromList [Vec.fromList [0, 1%1], Vec.fromList [1%1, 0]]
      
      stableM = LM $ Vec.fromList [Vec.fromList [1%2, 0], Vec.fromList [0, 1%2]]
      symM = LM $ Vec.fromList [Vec.fromList [1%1, 2%1], Vec.fromList [2%1, 3%1]]
      
      v0 = Vector (Map.singleton 0 (1%1))

  putStrLn "=== Core Kernel Tests ==="
  let coreTests = 
        [ proposition1_injectivity k0 k0
        , proposition2_leftInverseProjection k0
        , proposition2_leftInverseProjection k1a
        , proposition2_leftInverseProjection k2a
        , proposition3_commutativity k0
        , proposition3_commutativity k1a
        , theorem1_directedSystem fam2
        , theorem2_nonTrivialAutomorphism k1a
        , theorem3_radiativeSymmetryField k0
        ]
      corePassed = length (filter id coreTests)
  putStrLn $ "Core: " ++ show corePassed ++ "/9 passed"

  putStrLn "\n=== Vector Space Tests ==="
  let vsTests = 
        [ isLeftInverse b1 b2
        , isStable stableM
        , not (isStable mat1)
        ]
      vsPassed = length (filter id vsTests)
  putStrLn $ "VectorSpace: " ++ show vsPassed ++ "/3 passed"

  putStrLn "\n=== Compiler Tests ==="
  let compiled = compileToKernel b2 mat1
      exec = executeCompiled b2 compiled
      compilerTests = 
        [ not (Map.null (vectorCoords exec))
        , not (Map.null (vectorCoords (executeCompiled b2 (compileToKernel b2 mat2))))
        ]
      compilerPassed = length (filter id compilerTests)
  putStrLn $ "Compiler: " ++ show compilerPassed ++ "/2 passed"

  putStrLn "\n=== Mathematical Proofs (Detailed) ==="
  putStrLn $ "  Prop1 (Finite Basis):      " ++ show (prop1_finiteBasis b2)
  putStrLn $ "  Prop2 (Unique Matrix):     " ++ show (prop2_uniqueMatrix b2 b2 mat1)
  putStrLn $ "  Prop3 (Composition):       " ++ show (prop3_compositionMatrixMult b2 b2 b2 mat1 mat2)
  putStrLn $ "  Prop4 (Embedding Inj):     " ++ show (prop4_embeddingInjective b2 b3)
  putStrLn $ "  Prop5 (Projection):        " ++ show (prop5_projectionExists b2 b3)
  putStrLn $ "  Thm1  (Direct Limit):      " ++ show (thm1_directedLimit 2)
  putStrLn $ "  Prop6 (Locality):          " ++ show (prop6_localityComposition local1 local2)
  putStrLn $ "  Thm2  (Lyapunov):          " ++ show (thm2_lyapunovStability stableM v0)
  putStrLn $ "  Prop7 (Norm Decay):        " ++ show (prop7_normBoundDecay stableM 3)
  putStrLn $ "  Prop8 (Aut GL):            " ++ show (prop8_automorphismsGL b2)
  putStrLn $ "  Thm3  (Limit Compat):      " ++ show (thm3_limitMatricesCompatible 2)
  putStrLn $ "  Prop9 (Limit Local):       " ++ show (prop9_limitLocalityPreserved [local1, local2])
  putStrLn $ "  Spectral Theorem:          " ++ show (spectralTheoremSymmetric symM)

  let proofTests = 
        [ prop1_finiteBasis b2
        , prop2_uniqueMatrix b2 b2 mat1
        , prop3_compositionMatrixMult b2 b2 b2 mat1 mat2
        , prop4_embeddingInjective b2 b3
        , prop5_projectionExists b2 b3
        , thm1_directedLimit 2
        , prop6_localityComposition local1 local2
        , thm2_lyapunovStability stableM v0
        , prop7_normBoundDecay stableM 3
        , prop8_automorphismsGL b2
        , thm3_limitMatricesCompatible 2
        , prop9_limitLocalityPreserved [local1, local2]
        , spectralTheoremSymmetric symM
        ]
      proofsPassed = length (filter id proofTests)
  putStrLn $ "\nProofs Summary: " ++ show proofsPassed ++ "/13 passed"

  let totalPassed = corePassed + vsPassed + compilerPassed + proofsPassed
      totalTests = 9 + 3 + 2 + 13
  
  putStrLn $ "\n=== TOTAL: " ++ show totalPassed ++ "/" ++ show totalTests ++ " ===" 
  
  if totalPassed == totalTests
    then putStrLn "✅ ALL TESTS PASSED - Aleph-Omega kernel fully verified!"
    else putStrLn $ "❌ " ++ show (totalTests - totalPassed) ++ " tests failed - see details above"

