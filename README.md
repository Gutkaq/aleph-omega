ℵω (Aleph-Omega)

Computational system with provable correctness. Proofs in Haskell, execution in Rust.
Quick Start

Test proofs:

bash
cd haskell-core && cabal test

Run VM:

bash
cd kinf-vm && cargo run --example simple

Architecture
Layer	Language	Purpose	Status
Haskell Proofs	Haskell	Compile-time verification, proof extraction	✅ Complete
Bytecode Compiler	Haskell	Haskell → VM bytecode	🚧 Partial
VM Executor	Rust	Zero-overhead runtime execution	✅ Complete
Event Log	Rust	Distributed state management	🚧 Planned
Structure

What It Does

Haskell: KInf hierarchy (K₀ → K₁ → K₂ → Kω), vector spaces, graph theory, 14 verified propositions
Rust VM: Stack-based bytecode executor for Add, Mul, LevelUp operations
Status

✅ Haskell proofs complete
✅ VM core working
🚧 Bytecode compiler (next)
