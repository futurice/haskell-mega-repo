#!/bin/sh

# This script is used to build binaries inside the docker
# See mega-repo-tool -h

set -e

# This matters
unset POSIXLY_CORRECT

GREEN='\033[0;32m'
PURPLE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

echo_green() {
  echo "$GREEN###" "$@" "$NC"
}

echo_cyan() {
  echo "$CYAN===" "$@" "$NC"
}

run() {
  echo "$PURPLE>>>" "$@" "$NC"
  "$@"
}

# Configuration
#############################################################################

# Root directory. In docker it's /app/src
if [ "x$DOCKER" = "xYES" ]; then
    echo "Building in Docker"

    ROOTDIR=/app/src
else
    ROOTDIR=$(pwd)
fi

# We already change the dir here.
cd "$ROOTDIR"

# GHC version
GHCVER=${GHCVER-8.8.4}
export PATH=/opt/ghc/$GHCVER/bin:$PATH
HC=ghc-$GHCVER

# Use different BUILDDIR
BUILDDIR=dist-newstyle-prod

# Concurrency
CONCURRENCY=${CONCURRENCY-2}

# Current git hash.
GITHASH=$(git log --pretty=format:'%h' -n 1 --abbrev=8)

echo_green "Current git hash: $GITHASH"

# Build
#############################################################################

echo_cyan "Checking workdir is clean, and production data is in place"
run make check-dirty
run make check-checksums

echo_cyan "Versions"
run lsb_release -a
run "$HC" --version
run cabal --version

if [ "x$DOCKER" = "xYES" ]; then
    echo_cyan "Updating system packages in docker env"
    run yum -y update
fi

echo_cyan "Update cabal"
run cabal update

echo_cyan "Building Haskell stuff"
run cabal new-build "-j$CONCURRENCY" -w "$HC" --builddir="$BUILDDIR" all:exes

echo_cyan "Copying artifacts to build/"
LAMBDAS_ARG=""
if [ "x$PACKAGE_LAMBDAS" = "xNO" ]; then
	LAMBDAS_ARG="--no-lambdas"
fi
# We put binaries in separate directories to speed-up docker image creation
run cabal new-run -w "$HC" --builddir="$BUILDDIR" mega-repo-tool -- copy-artifacts $LAMBDAS_ARG --rootdir "$ROOTDIR" --builddir "$BUILDDIR"

echo_cyan "Writing git-hash.txt"
echo "$GITHASH" > "$ROOTDIR/build/git-hash.txt"

echo_cyan "Final cleanup"
if [ ! -z "$ORIGINAL_UID" ]; then
	run chown -R "$ORIGINAL_UID" "$ROOTDIR/build"
fi
run rm -f .ghc.environment.*
