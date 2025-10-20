{-# LANGUAGE GADTs, DeriveGeneric, StandaloneDeriving #-}

module AlephOmega.Types
  ( Direction
  , Config(..)
  , zeroConfig
  , supp
  , K0(..)
  , deltaC
  , KInf(..)
  , levelOf
  , iota
  , buildRadiativeFamily
  , aut
  , pi
  , radiativeClosure
  , isRadiativelyClosed
  , AlephOmegaField(..)
  , satisfiesAxioms
  ) where

import GHC.Generics (Generic)
import Data.Ratio ((%))
import Prelude hiding (pi)

type Direction = Integer

data Config = Config [(Direction, Rational)] deriving (Show, Eq, Generic)

zeroConfig :: Config
zeroConfig = Config []

supp :: Config -> [Direction]
supp (Config xs) = [d | (d, v) <- xs, v /= 0]

data K0 = K0 deriving (Show, Eq, Generic)

deltaC :: Config
deltaC = Config [(0, 1 % 1)]

data KInf where
  KInf0 :: K0 -> KInf
  KInf1 :: Config -> KInf
  KInf2 :: KInf -> (KInf -> Config) -> KInf
  KInf3 :: KInf -> (KInf -> Config) -> KInf

instance Show KInf where
  show (KInf0 _) = "KInf0"
  show (KInf1 c) = "KInf1(" ++ show c ++ ")"
  show (KInf2 k _) = "KInf2(" ++ show k ++ ")"
  show (KInf3 k _) = "KInf3(" ++ show k ++ ")"

instance Eq KInf where
  KInf0 _ == KInf0 _ = True
  KInf1 c1 == KInf1 c2 = c1 == c2
  KInf2 k1a _ == KInf2 k1b _ = k1a == k1b
  KInf3 k2a _ == KInf3 k2b _ = k2a == k2b
  _ == _ = False

levelOf :: KInf -> Integer
levelOf (KInf0 _) = 0
levelOf (KInf1 _) = 1
levelOf (KInf2 _ _) = 2
levelOf (KInf3 _ _) = 3

iota :: Integer -> KInf -> KInf
iota 0 (KInf0 _) = KInf1 deltaC
iota 1 (k1@(KInf1 _)) = KInf2 k1 (\prev -> if prev == k1 then deltaC else zeroConfig)
iota 2 (k2@(KInf2 _ _)) = KInf3 k2 (\prev -> if prev == k2 then deltaC else zeroConfig)
iota _ k = error $ "Invalid iota level for cell: " ++ show k

buildRadiativeFamily :: Integer -> [KInf]
buildRadiativeFamily 0 = [KInf0 K0]
buildRadiativeFamily n = 
  let prev = buildRadiativeFamily (n - 1)
      lastCell = last prev
      nextCell = iota (n - 1) lastCell
  in prev ++ [nextCell]

aut :: Integer -> KInf -> KInf
aut _ k = k

pi :: Integer -> KInf -> KInf
pi 1 (KInf1 _) = KInf0 K0
pi 2 (KInf2 k1 _) = k1
pi 3 (KInf3 k2 _) = k2
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
satisfiesAxioms (AOF cell iotaFn piFn _ _) = 
  let cellLevel = levelOf cell
      validRange = [0..(cellLevel - 1)]
  in if null validRange then True
     else all (\k -> 
       let embedded = iotaFn k cell
           projected = piFn k embedded
       in cell == projected) validRange

