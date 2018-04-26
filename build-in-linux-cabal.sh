#!/bin/sh

# This script is used to build binaries inside the docker
# See mega-repo-tool -h

set -e

# This matters
unset POSIXLY_CORRECT

# Root directory. In docker it's /app/src
if [ "x$DOCKER" = "xYES" ]; then
    echo "Building in Docker"

    ROOTDIR=/app/src
else
    ROOTDIR=$(pwd)
fi

cd $ROOTDIR

make check-dirty
make check-checksums

# GHC version
GHCVER=${GHCVER-8.2.2}
export PATH=/opt/ghc/$GHCVER/bin:$PATH
HC=ghc-$GHCVER

# Print versions
lsb_release -a
${HC} --version
cabal --version

# Update things
if [ "x$DOCKER" = "xYES" ]; then
    apt-get update && apt-get upgrade -y
fi

# Use different BUILDDIR
BUILDDIR=dist-newstyle-prod

# Concurrency
CONCURRENCY=${CONCURRENCY-2}

# Update cabal
cabal update

# Perform build
cabal new-build -j$CONCURRENCY -w $HC --builddir=$BUILDDIR all:exes

# write current git hash, so we know where we are
GITHASH=$(git log --pretty=format:'%h' -n 1 --abbrev=8)

# Copy binaries to ./build/exe/exe
# We put binaries in separate directories to speed-up docker image creation
cabal new-run mega-repo-tool -- copy-artifacts --rootdir "$ROOTDIR" --builddir "$BUILDDIR"

echo "$GITHASH" > "$ROOTDIR/build/git-hash.txt"
