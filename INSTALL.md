# Installation information

If you are having difficulty installing the PureScript compiler, feel free to ask for help! The best places are the [PureScript Discord](https://purescript.org/chat) or [PureScript Discourse](https://discourse.purescript.org).

## Requirements

The PureScript compiler is built using GHC 9.8.4, and should be able to run on any operating system supported by GHC 9.8.4. In particular:

* for Windows users, versions predating Vista are not officially supported,
* for macOS / OS X users, versions predating Mac OS X 10.7 (Lion) are not officially supported.

See also <https://www.haskell.org/ghc/download_ghc_9_8_4.html> for more details about the operating systems which GHC 9.8.4 supports.

## Official prebuilt binaries

Each [release](https://github.com/Lesslie-Tech/purserl/releases) comes with prebuilt binary bundles for Linux (x86-64, static) and macOS (arm64). Users of other operating systems or architectures will need to build the compiler from source; see below.

To install a binary bundle, simply extract it and place the `purs` executable somewhere on your PATH.

## Compiling from source

The easiest way is to use stack:

```
$ git clone https://github.com/Lesslie-Tech/purserl.git
$ cd purserl
$ stack install --flag purescript:RELEASE
```

This will then copy the compiler executable (`purs`) into `~/.local/bin`.

If you don't have stack installed, please see the [stack install documentation](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md).
