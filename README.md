<img src="logo.png" alt="PureScript" width="392">

A small strongly typed programming language with expressive types that compiles to JavaScript, written in and inspired by Haskell.

[![Hackage](https://img.shields.io/hackage/v/purescript.svg)](http://hackage.haskell.org/package/purescript) [![Build Status](https://github.com/purescript/purescript/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/purescript/purescript/actions/workflows/ci.yml)

## Purserl local builds
Mac build, for machines with lots of ram:
```
stack install --work-dir .stack-work-O2 --ghc-options="-O2 -with-rtsopts=\"-N -A2048m -n128m\""; purs +RTS --info 
```

Mac build, for machines with less ram, guessed ok values
```
stack install --work-dir .stack-work-O2 --ghc-options="-O2 -with-rtsopts=\"-N -A256m -n16m\""; purs +RTS --info 
```

x86 build, sadly defaults are as good as it gets:
```
stack install --work-dir .stack-work-O2 --ghc-options="-O2 -with-rtsopts=\"-N\""; purs +RTS --info 
```

### Profiling build

Build `purs` with GHC profiling instrumentation:
```
stack build --profile purescript:exe:purs
```
`--profile` is shorthand for `--library-profiling --executable-profiling --ghc-options="-fprof-auto"`, so it also
auto-adds cost centers to every local binding without needing to annotate anything by hand. The first profiling
build is slow since it also has to build every dependency's profiling libraries from scratch; incremental rebuilds
afterwards are normal speed. It writes to a separate part of `.stack-work` from a normal build, so this doesn't
disturb your regular dev binary — the two coexist.

The resulting binary is a `purs` that understands `+RTS ... -RTS` profiling flags in addition to the normal
`purs` CLI flags (`purescript.cabal` already sets `-rtsopts`, so these aren't locked down). Run it with:
```
stack exec --profile -- purs compile ... +RTS <profiling flags> -RTS
```
or find the binary directly with `stack path --profile --local-install-root` (append `/bin/purs`) and invoke it
without going through `stack exec`.

Useful `+RTS` flags on a profiling build:
- `-p` — cost-center time/allocation profile. Writes `purs.prof`: a table of which named function/cost-center
  consumed what % of runtime and allocation, with a call-tree breakdown. The go-to flag for "where is time going".
- `-h` (or `-hc`/`-hd`/`-hy`/`-hr`) — heap profile broken down by cost-center (`-hc`, the default), closure
  description (`-hd`), type (`-hy`), or retainer (`-hr`, useful for tracking down space leaks). Writes `purs.hp`;
  render it with `hp2ps purs.hp` (from GHC's toolchain) to get a `.ps`/PDF graph of live heap over time.
  Combine with `-p` (i.e. `-hc -p`) to line the heap profile up with the same cost centers as the time profile.
- `-pj` — same cost-center profile as `-p`, but written as JSON (`purs.prof`) instead of the plaintext table;
  drag it into [speedscope.app](https://www.speedscope.app/) for an interactive flamegraph.
- `-s` — a plain runtime stats summary (total allocation, GC time vs. mutator time, number of GCs, etc.) printed
  to stderr on exit. Doesn't need cost centers, so it also works on a normal (non-profiling) `-rtsopts` build.
- `-N<n>` — as with a normal build, how many capabilities (OS threads) to run on; combine freely with the above,
  e.g. `+RTS -N4 -p -RTS`.

Cost centers add real overhead (allocation and time both change under `-p`/`-h`), so profiling numbers are useful
for relative comparisons between compiler changes, not as absolute wall-clock numbers — use a normal, non-profiling
build for real perf measurement.

## Language info

- [PureScript home](http://purescript.org)
- [Releases & changelog](https://github.com/purescript/purescript/releases)
- [Contributing to PureScript](https://github.com/purescript/purescript/blob/master/CONTRIBUTING.md)

## Resources

- [PureScript book](https://book.purescript.org/)
- [Documentation](https://github.com/purescript/documentation)
- [Try PureScript](http://try.purescript.org)
- [Pursuit Package Index](http://pursuit.purescript.org/)

## Help!

### Community Spaces

The following spaces are governed by the [PureScript Community Code of Conduct](https://github.com/purescript/governance/blob/master/CODE_OF_CONDUCT.md). The majority of PureScript users use these spaces to discuss and collaborate on PureScript-related topics:
- [PureScript Discord](https://purescript.org/chat)
- [PureScript Discourse](https://discourse.purescript.org/)

### Unaffiliated Spaces

Some PureScript users also collaborate in the below spaces. These do not fall under the code of conduct linked above. They may have no code of conduct or one very different than the one linked above.
- [PureScript Matrix](https://matrix.to/#/#purescript:matrix.org)
- [PureScript on StackOverflow](http://stackoverflow.com/questions/tagged/purescript)
- [The `#purescript` channel on Libera.Chat](https://libera.chat/)
