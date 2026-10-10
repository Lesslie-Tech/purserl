bin_dir = bin
build_dir = .build
package = purescript
exe_target = purs
stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack

.DEFAULT_GOAL := help

$(bin_dir)/hlint: ci/install-hlint.sh
	BIN_DIR=$(bin_dir) BUILD_DIR=$(build_dir) $<
	touch $@

clean: ## Remove build artifacts
	rm -fr $(bin_dir)
	rm -fr $(build_dir)

help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

ghcid: ## Run ghcid to quickly reload code on save.
	ghcid --command "stack ghci purescript:exe:purs purescript:lib purescript:test:tests --main-is purescript:exe:purs --ghci-options -fno-code"

ghcid-test: ## Run ghcid to quickly reload code and run tests on save.
	ghcid --command "stack ghci purescript:lib purescript:test:tests --ghci-options -fobject-code" \
	    --test "Main.main"

build: ## Build the package.
	$(stack) build $(package)

build-dirty: ## Force recompilation of the entire package.
	$(stack) build --ghc-options=-fforce-recomp $(package)

run: ## Run the compiler.
	$(stack) build --fast && $(stack) exec -- $(exe_target)

install: ## Install the executables to stack's path
	$(stack) install

install-opt: ## Install the executables to stack's path
	$(stack) install --flag=purescript:RELEASE

ghci: ## Open GHCi with the PureScript library
	$(stack) ghci $(package):lib

test: ## Run the tests.
	$(stack) test --fast $(package)

test-ghci: ## Open GHCi with the test suite loaded.
	$(stack) ghci $(package):test:tests

# If you want to profile a particular test, such
# as LargeSumType.purs, add -p to the test arguments like so:
# stack test --executable-profiling --ta '-p LargeSum +RTS -pj -RTS'

# Also, you'll need flamegraph.pl and ghc-prof-aeson-flamegraph
# (cf. dev-deps), I git cloned the FlameGraph repository and
# symlinked the Perl script into my path.
# Open the SVG with your browser, you can reload the browser when you
# rerun the profiled test run.
test-profiling: ## Run the tests, with profiling enabled. Also builds a flamegraph of the test.
	$(stack) test --executable-profiling --ta '+RTS -pj -RTS' $(package)
	cat tests.prof | stack exec ghc-prof-aeson-flamegraph | flamegraph.pl > tests.svg

# RTS tuning of the compiler itself, measured with `purs compile ... +RTS -s`
# on a clean ~1500 module build (32 cores, build at d945b0fa, Oct 2026).
# GC is about half of the wall time with the default 4MB allocation area, but
# -with-rtsopts is left at -N for now since peak memory matters more than speed.
#   options            wall     peak RSS
#   -N (current)       45.0 s   3.2 GB
#   -N -A32m           36.3 s   4.3 GB
#   -N -A64m           33.4 s   5.2 GB
#   -N -A64m -qn8      32.4 s   5.1 GB   (GC CPU time 160 s -> 54 s)
#   -N8 -A128m -qn8    33.5 s   4.1 GB
#   -N -A128m -qn8     30.2 s   6.9 GB
# Mutator time stays at ~24 s whatever the core count, so beyond ~8 cores
# only the GC settings matter.

bench: ## Run benchmarks for PureScript
	$(stack) bench $(package)

# if you want these to be globally available run it outside of purescript
# but incompatibilities might arise between ghcid and the version of GHC
# you're using to build PureScript.
dev-deps: ## Install helpful development tools.
	stack install ghcid ghc-prof-aeson-flamegraph

license-generator: ## Update dependencies in LICENSE
	$(stack) ls dependencies purescript --flag purescript:RELEASE | stack license-generator/generate.hs > LICENSE

lint: lint-hlint ## Check project adheres to standards

lint-hlint: $(bin_dir)/hlint ## Check project adheres to hlint standards
	$< --git

.PHONY : build build-dirty run install ghci test test-ghci test-profiling ghcid dev-deps license-generator clean lint lint-hlint
