{-# LANGUAGE GADTs, DeriveGeneric, StandaloneDeriving #-}

module AlephOmega.Types where

import GHC.Generics (Generic)
import Prelude hiding (pi)

type Direction = Integer

data Config = Config [(Direction, Integer)]
  deriving (Show, Eq, Generic)

zeroConfig :: Config
zeroConfig = Config []

supp :: Config -> [Direction]
supp (Config xs) = [d | (d, v) <- xs, v /= 0]

data K0 = K0
  deriving (Show, Eq, Generic)

deltaC :: Config
deltaC = Config [(0, 1)]

data KInf where
  KInf0 :: K0 -> KInf
  KInf1 :: Config -> KInf
  KInf2 :: (Config -> Config) -> KInf
  KInf3 :: ((Config -> Config) -> Config) -> KInf
  KInf4 :: (((Config -> Config) -> Config) -> Config) -> KInf

instance Show KInf where
  show (KInf0 _) = "KInf0"
  show (KInf1 c) = "KInf1(" ++ show c ++ ")"
  show (KInf2 _) = "KInf2"
  show (KInf3 _) = "KInf3"
  show (KInf4 _) = "KInf4"

instance Eq KInf where
  KInf0 _ == KInf0 _ = True
  KInf1 c1 == KInf1 c2 = c1 == c2
  _ == _ = False

levelOf :: KInf -> Integer
levelOf (KInf0 _) = 0
levelOf (KInf1 _) = 1
levelOf (KInf2 _) = 2
levelOf (KInf3 _) = 3
levelOf (KInf4 _) = 4

iota :: Integer -> KInf -> KInf
iota 0 (KInf0 _) = KInf1 deltaC
iota 1 (KInf1 c) = KInf2 (\prev -> if prev == c then deltaC else zeroConfig)
iota 2 (KInf2 _) = KInf3 (const deltaC)
iota 3 (KInf3 _) = KInf4 (const deltaC)
iota _ _ = error "Invalid iota level or cell mismatch"

buildRadiativeFamily :: Integer -> [KInf]
buildRadiativeFamily 0 = [KInf0 K0]
buildRadiativeFamily n = 
  let prev = buildRadiativeFamily (n - 1)
      lastCell = last prev
      nextCell = iota (n - 1) lastCell
  in prev ++ [nextCell]

aut :: Integer -> KInf -> KInf
aut _ (KInf0 k) = KInf0 k
aut _ (KInf1 c) = KInf1 c
aut _ (KInf2 f) = KInf2 f
aut _ (KInf3 f) = KInf3 f
aut _ (KInf4 f) = KInf4 f

pi :: Integer -> KInf -> KInf
pi 0 (KInf1 _) = KInf0 K0
pi 0 (KInf2 _) = KInf0 K0
pi 0 (KInf3 _) = KInf0 K0
pi 0 (KInf4 _) = KInf0 K0
pi 1 (KInf2 f) = KInf1 (f zeroConfig)
pi 1 (KInf3 _) = KInf1 zeroConfig
pi 1 (KInf4 _) = KInf1 zeroConfig
pi 2 (KInf3 f) = KInf2 (\x -> f (\_ -> x))
pi 2 (KInf4 _) = KInf2 id
pi 3 (KInf4 f) = KInf3 (\x -> f (\_ -> x zeroConfig))
pi _ _ = KInf0 K0

radiativeClosure :: [KInf] -> [KInf]
radiativeClosure family = 
  let n = length family - 1
      indices = [0..n]
      newCells = [iota (fromIntegral k) (family !! k) | k <- indices, k < length family]
      filtered = filter (`notElem` family) newCells
  in if null filtered then family else family ++ filtered

isRadiativelyClosed :: [KInf] -> Bool
isRadiativelyClosed family = 
  let closed = radiativeClosure family
  in length closed == length family

data AlephOmegaField = AOF 
  { aofCell :: KInf
  , aofIota :: Integer -> KInf -> KInf
  , aofPi :: Integer -> KInf -> KInf
  , aofAut :: Integer -> KInf -> KInf
  , aofFamily :: [KInf]
  }

instance Show AlephOmegaField where
  show (AOF cell _ _ _ family) = 
    "AOF(" ++ show cell ++ ", <ops>, " ++ show (length family) ++ " cells)"

satisfiesAxioms :: AlephOmegaField -> Bool
satisfiesAxioms (AOF cell iotaFn piFn _ family) = 
  let cellLevel = levelOf cell
      validRange = [0..(cellLevel - 1)]
  in if null validRange then True
     else all (\k -> 
       if k >= fromIntegral (length family) then True
       else let embedded = iotaFn k cell
                projected = piFn k embedded
            in case (cell, projected) of
                 (KInf0 _, KInf0 _) -> True
                 (KInf1 c1, KInf1 c2) -> c1 == c2
                 _ -> False) validRange

