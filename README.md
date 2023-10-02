# Plutus NFT Marketplace Tutorial

#### Frontend in Mesh.js Can be found [here](https://github.com/safi-ullah08/nft-martketplace-frontend)

## Setup Instructions

Follow these steps to set up the environment for the Plutus NFT Marketplace:

### System Requirements

- Architecture: x86 (Recommended)
  - Note: There have been issues with setting up the environment on aarch64 M1 Mac, even within a Ubuntu VM on the same chip. The `scrypt` component fails due to its current support being limited to x86. More details can be found in this [GitHub issue](https://github.com/informatikr/scrypt/issues/8).

### Configuration Files

- A tutorial on building Plutus projects by Embargo using Nix can be found [here](https://www.youtube.com/live/AgkAwiyxSxA?si=Mdb1Lo4_Sd6GA8dU).
- The build files mentioned in the tutorial (included in this repo) [here](https://gist.github.com/iburzynski/6936c6d75a4c8838a5e94559d1143ba2).
- Best way to install nix [here](https://zero-to-nix.com/)

### Entering the Nix Shell

Execute the following commands in your terminal:

```bash
nix shell nixpkgs#haskell.compiler.ghc8107 nixpkgs#cabal-install
nix develop
```

Once the above commands are done 

```bash
cabal repl 
```
Or to run unit tests

```bash
cabal repl plutus-test-test
```



