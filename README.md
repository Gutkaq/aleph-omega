# aleph-omega

Packet-based VM with transfinite math kernel and NoC runtime.

## What

- **Haskell**: Math core for infinite-dimensional configs, code generation
- **Rust**: NoC simulator (mesh/ring topologies, packet routing)
- **Goal**: Transfinite computation via self-routing packets

No OS. No scheduling. Just packets and math.

## Components

| Component | Language | Purpose |
|-----------|----------|---------|
| Math Core | Haskell | Aleph-Ω kernel, dimensions, discrete dynamics |
| VM Core | Haskell | Packet-based VM (registers, ALU) |
| Code Gen | Haskell | Translate configs to packet streams |
| NoC Runtime | Rust | Packet routing, cores, memory (software emulation) |
| Toolchain | Both | Assembler, linker, simulator |

## Status

🚧 Early development - setting up infrastructure

- **GHC**: 9.6.7
- **rustc**: 1.90.0
- **OS**: Debian (or compatible Linux)

## Build
## Haskell

cd haskell-core

cabal build

cabal test

## Rust

cd rust-runtime
cargo build --release
cargo test
