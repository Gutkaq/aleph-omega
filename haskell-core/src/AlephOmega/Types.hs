module AlephOmega.Types
  ( -- * Core Types
  Nat
  , Direction
  , Config(..)
  -- * INDUCTIVE KERNEL FAMILY
  , Co -- k_0: base case (constant zero from empty)
  , K (..)  -- K_n: cell in inductive family {K_n | n (- N}
  , mkK -- Make K_n cell (inductive constructor)
  , buildKernels -- Build N+1 kernels: [K_0, .... , K_N]
  , buildConfigs -- Genarate N+1 configs from kernels
  -- * Base constructor/operations
  , zeroConfig -- Zero in C_inf
  , zeroCo -- Zero K_0 cell
  , applyK -- Apply K_n to previous [K_{n-1}]
  ) where

  -- Natural numbers N for cordinates (0, 1, 2, ....)
type Nat = Integer

  -- Direction labels for counatable infinite dimensions ("dim_0", "x", etc..)
type Direction = String

newtype Config = Config { unConfig :: [(Direction, Nat)] }
  deriving (Show, Eq)

  -- K_0 = Co : base cell constant {} -> zero Config
newtype Co = Co { unCo :: Config}
  deriving (Show, Eq)

  
  -- | K: cell in inductive family K_n
  -- K_n = functions from family of K_{n-1} cells to C_∞  
  -- Wrapped as newtype; unK takes [previous K_{n-1}] -> config
  -- Full kernel: lim K_inf = ∪ K_n (radiative inductive limit)
  -- From math: K is family of cells, each maps previous to C_inf
newtype K = K { unK :: [K] -> Config }

  -- mkK Inductive constructor for cell K_n
  -- Simple : Ignores inputs and outputs delta config in "dim_n" (unit 1)
  -- From math : explicit construction f: K_{n-1} -> C_inf; here constant delta
mkK :: Int -> K 
mkK n = K(const (Config [("dim_" ++ show n, 1)]))

  -- zeroConfig: origin in C_inf (all dims are 0, sparse empty list)
  -- From math: Default/zero function output 
zeroConfig :: Config
zeroConfig = Config []

  -- zeroCo: base k0 = Co ( constant zero config)
zeroCo :: Co 
zeroCo = Co zeroConfig

  -- ApplyK : run k_n on list of previous [K_{n-1}]
applyK :: K -> [K] -> Config
applyK k prev = unK k prev 

  -- | buildKernels: inductive family [K_0, K_1, ..., K_N]
  -- K_0 = constant zero; K_n = mkK n for n≥1
-- General for any N; simulates inductive process up to finite N
buildKernels :: Int -> [K]
buildKernels n =
  let k0 = K (const zeroConfig)  -- K_0 as constant zero
  in take (n+1) (k0 : [mkK i | i <- [1..]])

  -- | buildConfigs: [c_0, ..., c_N] where c_i = apply K_i to previous kernels
  -- Generates N+1 configs from the family; each c_n (- C_inf
buildConfigs :: Int -> [Config]
buildConfigs n =
  let kernels = buildKernels n
  in [applyK k (take i kernels) | (k, i) <- zip kernels [0..n]]
