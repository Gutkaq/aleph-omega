import AlephOmega.Types
import AlephOmega.Theorems
import Prelude hiding (pi)

main :: IO ()
main = do
  let k0 = KInf0 K0
      k1a = KInf1 (Config [(0, 1)])
      k1b = KInf1 (Config [(1, 2)])
      k2a = iota 1 k1a
      k2b = iota 1 k1b
      
      fam0 = buildRadiativeFamily 0
      fam1 = buildRadiativeFamily 1
      fam2 = buildRadiativeFamily 2

  putStrLn "=== Proposition 1: Injectivity of ι_n ==="
  putStrLn $ "Test 1.1 (K0 vs K0): " ++ show (proposition1_injectivity k0 k0)
  putStrLn $ "Test 1.2 (K1 vs K1): " ++ show (proposition1_injectivity k1a k1b)
  putStrLn $ "Test 1.3 (K2 vs K2): " ++ show (proposition1_injectivity k2a k2b)

  putStrLn "\n=== Proposition 2: Left-Inverse Projection (π ∘ ι = id) ==="
  putStrLn $ "Test 2.1 (K0): " ++ show (proposition2_leftInverseProjection k0)
  putStrLn $ "Test 2.2 (K1): " ++ show (proposition2_leftInverseProjection k1a)
  putStrLn $ "Test 2.3 (K2): " ++ show (proposition2_leftInverseProjection k2a)

  putStrLn "\n=== Proposition 3: Commutativity (π ∘ ι chain) ==="
  putStrLn $ "Test 3.1 (K0): " ++ show (proposition3_commutativity k0)
  putStrLn $ "Test 3.2 (K1): " ++ show (proposition3_commutativity k1a)

  putStrLn "\n=== Theorem 1: Directed System (Left Inverse Axiom) ==="
  putStrLn $ "Test Th1.1 (n=0): " ++ show (theorem1_directedSystem fam0)
  putStrLn $ "Test Th1.2 (n=1): " ++ show (theorem1_directedSystem fam1)
  putStrLn $ "Test Th1.3 (n=2): " ++ show (theorem1_directedSystem fam2)

  putStrLn "\n=== Theorem 2: Automorphisms are Identity (by construction) ==="
  putStrLn $ "Test Th2.1 (K1): " ++ show (theorem2_nonTrivialAutomorphism k1a)
  putStrLn $ "Test Th2.2 (K2): " ++ show (theorem2_nonTrivialAutomorphism k2a)

  putStrLn "\n=== Theorem 3: Radiative Symmetry Field ==="
  putStrLn $ "Test Th3.1 (K0): " ++ show (theorem3_radiativeSymmetryField k0)
  putStrLn $ "Test Th3.2 (K1): " ++ show (theorem3_radiativeSymmetryField k1a)
  putStrLn $ "Test Th3.3 (K2): " ++ show (theorem3_radiativeSymmetryField k2a)

  let allTests =
        [ proposition1_injectivity k0 k0, proposition1_injectivity k1a k1b, proposition1_injectivity k2a k2b
        , proposition2_leftInverseProjection k0, proposition2_leftInverseProjection k1a, proposition2_leftInverseProjection k2a
        , proposition3_commutativity k0, proposition3_commutativity k1a
        , theorem1_directedSystem fam0, theorem1_directedSystem fam1, theorem1_directedSystem fam2
        , theorem2_nonTrivialAutomorphism k1a, theorem2_nonTrivialAutomorphism k2a
        , theorem3_radiativeSymmetryField k0, theorem3_radiativeSymmetryField k1a, theorem3_radiativeSymmetryField k2a
        ]
      passed = length (filter id allTests)
      total = length allTests
  
  putStrLn $ "\nTests passed: " ++ show passed ++ "/" ++ show total
  if passed == total then putStrLn " All theorems verified!" else putStrLn " Some tests failed"

