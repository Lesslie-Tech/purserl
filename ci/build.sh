#!/bin/bash

set -ex

# Provides expanders that group console output in GitHub Actions
# See https://docs.github.com/en/actions/reference/workflow-commands-for-github-actions#grouping-log-lines
(echo "::group::Initialize variables") 2>/dev/null

# This is the main CI build script. It is intended to run on all platforms we
# run CI on: linux and mac os. It makes use of the following
# environment variables:
#
# - CI_RELEASE
#
#   If set to "true", passes the RELEASE flag to the compiler, and enables
#   optimizations. Otherwise, we disable optimizations (to speed builds up).
#
# = Source distributions
#
# During a normal build, we create a source distribution with `stack sdist`,
# and then compile and run tests inside that. The reason for this is that it
# helps catch issues arising from forgetting to list files which are necessary
# for compilation or for tests in our package.yaml file (these sorts of issues
# don't test to get noticed until after releasing otherwise).

# We test with --haddock because haddock generation can fail if there is invalid doc-comment syntax,
# and these failures are very easy to miss otherwise.
STACK="stack --no-terminal --haddock --jobs=4"

#STACK_OPTS="--test"
if [ "$CI_RELEASE" = "true" ]
then
  STACK_OPTS="$STACK_OPTS --flag=purescript:RELEASE"
else
  STACK_OPTS="$STACK_OPTS --fast"
fi
if [ "$CI_STATIC" = "true" ]
then
  STACK_OPTS="$STACK_OPTS --flag=purescript:static"
fi

(echo "::endgroup::"; echo "::group::Set version number for build") 2>/dev/null

package_version=$(node -pe 'require("./npm-package/package.json").version')
package_release_version=${package_version%%-*}
package_prerelease_suffix=${package_version#$package_release_version}

if ! grep -q "\"install-purescript --purs-ver=${package_version//./\\.}\"" npm-package/package.json
then
  echo "Version in npm-package/package.json doesn't match version in install-purescript call"
  exit 1
fi

if ! grep -q "^version:\\s*${package_release_version//./\\.}$" purescript.cabal
then
  echo "Version in npm-package/package.json doesn't match version in purescript.cabal"
  exit 1
fi

if ! grep -q "^prerelease = \"${package_prerelease_suffix//./\\.}\"$" app/Version.hs
then
  echo "Version in npm-package/package.json doesn't match prerelease in app/Version.hs"
  exit 1
fi

(echo "::endgroup::"; echo "::group::Install snapshot dependencies") 2>/dev/null

# Install snapshot dependencies (since these will be cached globally and thus
# can be reused during the sdist build step)
$STACK build --only-snapshot $STACK_OPTS

(echo "::endgroup::"; echo "::group::Build source distributions") 2>/dev/null

## Test in a source distribution (see above)
$STACK sdist . --tar-dir sdist-test;
tar -xzf sdist-test/purescript-*.tar.gz -C sdist-test --strip-components=1

(echo "::endgroup::"; echo "::group::Build and test PureScript") 2>/dev/null

#pushd sdist-test
# Haddock -Werror goes here to keep us honest but prevent failing on
# documentation errors in dependencies
$STACK build $STACK_OPTS

(echo "::endgroup::") 2>/dev/null
