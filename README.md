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

## Build

Haskell

cd haskell-core
stack build
Rust

cd rust-runtime
cargo build

text

## Contributing

PRs welcome. See `docs/CONTRIBUTING.md` for standards.

## License

MIT
