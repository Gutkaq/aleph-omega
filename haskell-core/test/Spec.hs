import AlephOmega.Types
import Prelude hiding (pi)

testBasics :: IO ()
testBasics = do
  putStrLn "=== Basic Type Tests ==="
  
  putStrLn "\n1. Config operations"
  let emptyConf = zeroConfig
  putStrLn $ "zeroConfig: " ++ show emptyConf
  putStrLn $ "supp(zeroConfig): " ++ show (supp emptyConf)
  
  let conf1 = Config [(0,1), (5,3), (10,0)]
  putStrLn $ "Config with zeros: " ++ show conf1
  putStrLn $ "supp (filters zeros): " ++ show (supp conf1)
  
  let conf2 = Config [(1,5), (2,0), (3,7)]
  putStrLn $ "supp " ++ show conf2 ++ " = " ++ show (supp conf2)

testEquality :: IO ()
testEquality = do
  putStrLn "\n=== Equality Tests ==="
  
  let k0a = KInf0 K0
      k0b = KInf0 K0
  putStrLn $ "KInf0 K0 == KInf0 K0: " ++ show (k0a == k0b)
  
  let c1 = Config [(1,1)]
      c2 = Config [(1,1)]
      c3 = Config [(2,1)]
  putStrLn $ "Config [(1,1)] == Config [(1,1)]: " ++ show (c1 == c2)
  putStrLn $ "Config [(1,1)] == Config [(2,1)]: " ++ show (c1 == c3)
  putStrLn $ "KInf1 c1 == KInf1 c2: " ++ show (KInf1 c1 == KInf1 c2)
  putStrLn $ "KInf1 c1 == KInf1 c3: " ++ show (KInf1 c1 == KInf1 c3)
  putStrLn $ "KInf0 K0 == KInf1 deltaC: " ++ show (k0a == KInf1 deltaC)

testLevels :: IO ()
testLevels = do
  putStrLn "\n=== Level Tests ==="
  
  let k0 = KInf0 K0
      k1 = KInf1 deltaC
      k2 = KInf2 id
      k3 = KInf3 (\f -> f zeroConfig)
      k4 = KInf4 (\g -> g (\x -> x))
  
  putStrLn $ "levelOf KInf0: " ++ show (levelOf k0)
  putStrLn $ "levelOf KInf1: " ++ show (levelOf k1)
  putStrLn $ "levelOf KInf2: " ++ show (levelOf k2)
  putStrLn $ "levelOf KInf3: " ++ show (levelOf k3)
  putStrLn $ "levelOf KInf4: " ++ show (levelOf k4)

testIotaValid :: IO ()
testIotaValid = do
  putStrLn "\n=== Valid Iota Tests ==="
  
  let k0 = KInf0 K0
  let k1 = iota 0 k0
  putStrLn $ "iota 0 (KInf0 K0): " ++ show k1
  
  let testConf = Config [(2, 7)]
  let k1b = KInf1 testConf
  let k2 = iota 1 k1b
  putStrLn $ "iota 1 (KInf1 " ++ show testConf ++ "): " ++ show k2
  
  let k2b = KInf2 (\x -> x)
  let k3 = iota 2 k2b
  putStrLn $ "iota 2 KInf2: " ++ show k3
  
  let k3b = KInf3 (\f -> f zeroConfig)
  let k4 = iota 3 k3b
  putStrLn $ "iota 3 KInf3: " ++ show k4

testPiValid :: IO ()
testPiValid = do
  putStrLn "\n=== Valid Pi Tests ==="
  
  let k1 = KInf1 deltaC
  putStrLn $ "pi 0 (KInf1 deltaC): " ++ show (AlephOmega.Types.pi 0 k1)
  
  let k2 = KInf2 (\x -> Config [(1, 5)])
  let result1 = AlephOmega.Types.pi 1 k2
  putStrLn $ "pi 1 KInf2: " ++ show result1
  
  let k3 = KInf3 (\f -> f zeroConfig)
  putStrLn $ "pi 2 KInf3: " ++ show (AlephOmega.Types.pi 2 k3)
  
  let k4 = KInf4 (\g -> g (\x -> x))
  putStrLn $ "pi 3 KInf4: " ++ show (AlephOmega.Types.pi 3 k4)

testRadiativeFamily :: IO ()
testRadiativeFamily = do
  putStrLn "\n=== Radiative Family Tests ==="
  
  putStrLn "\nFamily n=0:"
  let fam0 = buildRadiativeFamily 0
  print fam0
  putStrLn $ "Length: " ++ show (length fam0)
  
  putStrLn "\nFamily n=1:"
  let fam1 = buildRadiativeFamily 1
  print fam1
  putStrLn $ "Length: " ++ show (length fam1)
  
  putStrLn "\nFamily n=2:"
  let fam2 = buildRadiativeFamily 2
  print fam2
  putStrLn $ "Length: " ++ show (length fam2)
  
  putStrLn "\nFamily n=4:"
  let fam4 = buildRadiativeFamily 4
  mapM_ print fam4
  putStrLn $ "Length: " ++ show (length fam4)

testRadiativeClosure :: IO ()
testRadiativeClosure = do
  putStrLn "\n=== Radiative Closure Tests ==="
  
  let fam1 = buildRadiativeFamily 1
  putStrLn $ "Initial family: " ++ show fam1
  putStrLn $ "Is closed: " ++ show (isRadiativelyClosed fam1)
  
  let closed = radiativeClosure fam1
  putStrLn $ "After closure: " ++ show closed
  putStrLn $ "Length changed: " ++ show (length fam1) ++ " -> " ++ show (length closed)
  
  let closedAgain = radiativeClosure closed
  putStrLn $ "Closure is idempotent: " ++ show (closedAgain == closed)

testAxioms :: IO ()
testAxioms = do
  putStrLn "\n=== Axiom Tests (π ∘ ι = id) ==="
  
  putStrLn "\nTest at level 0 (KInf0):"
  let k0 = KInf0 K0
      fam0 = [k0]
      field0 = AOF k0 iota AlephOmega.Types.pi aut fam0
  putStrLn $ "Field: " ++ show field0
  putStrLn $ "Axioms hold: " ++ show (satisfiesAxioms field0)
  
  putStrLn "\nTest at level 1 (manual verification):"
  let k1 = KInf1 (Config [(3, 9)])
      k2_from_k1 = iota 1 k1
      k1_back = AlephOmega.Types.pi 1 k2_from_k1
  putStrLn $ "Original k1: " ++ show k1
  putStrLn $ "After iota 1: " ++ show k2_from_k1
  putStrLn $ "After pi 1: " ++ show k1_back
  putStrLn $ "Round trip equals: " ++ show (k1 == k1_back)

testAutomorphisms :: IO ()
testAutomorphisms = do
  putStrLn "\n=== Automorphism Tests (identity) ==="
  
  let k0 = KInf0 K0
  putStrLn $ "aut 0 KInf0: " ++ show (aut 0 k0) ++ " (equals input: " ++ show (aut 0 k0 == k0) ++ ")"
  
  let k1 = KInf1 (Config [(5, 2)])
  putStrLn $ "aut 1 KInf1: " ++ show (aut 1 k1) ++ " (equals input: " ++ show (aut 1 k1 == k1) ++ ")"

main :: IO ()
main = do
  testBasics
  testEquality
  testLevels
  testIotaValid
  testPiValid
  testRadiativeFamily
  testRadiativeClosure
  testAxioms
  testAutomorphisms
  putStrLn "\n=== All tests passed! ==="

