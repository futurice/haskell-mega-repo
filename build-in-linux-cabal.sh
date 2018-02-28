#!/bin/sh

# This script is used to build binaries inside the docker
# See mega-repo-tool -h

set -ex

# This matters
unset POSIXLY_CORRECT

# Root directory. In docker it's /app/src
if [ "x$DOCKER" = "xYES" ]; then
    ROOTDIR=/app/src
else
    ROOTDIR=$(pwd)
fi

cd $ROOTDIR

# Check that we don't have cabal.project.local
if [ -f cabal.project.local ]; then
	echo "Cannot do production build with cabal.project.local"
	exit 1
fi

# Check that we have somewhat clean working dir
if [ ! -z "$(git status --porcelain)" ]; then
    echo "DIRTY WORKINGDIR"
    exit 1
fi

# Check that we have proper data files in place
#
# To update:
#
# $ sha256sum futurice-constants/constants.json futurice-tribes/tribes.json hc-app/early-caring.template > data.sha256sums
#
if sha256sum -c data.sha256sums; then
    echo "Data files OK"
else
    echo "Invalid datafiles"
    exit 1
fi

# GHC version
GHCVER=${GHCVER-8.2.2}
export PATH=/opt/ghc/$GHCVER/bin:$PATH
HC=ghc-$GHCVER

# Use different BUILDDIR
BUILDDIR=dist-newstyle-prod

# Concurrency
CONCURRENCY=${CONCURRENCY-1}

# Update cabal
cabal update

# Perform build
cabal new-build -j$CONCURRENCY -w $HC --builddir=$BUILDDIR all:exes

# write current git hash, so we know where we are
GITHASH=$(git log --pretty=format:'%h' -n 1 --abbrev=8)

# Copy binaries to ./build/exe/exe
# We put binaries in separate directories to speed-up docker image creation
mkdir -p $ROOTDIR/build
for fullexe in $(cabal-plan --builddir=$BUILDDIR list-bins|grep ':exe:'|awk '{print $2}'); do
    if [ $(echo $fullexe | sed "s:^$ROOTDIR/.*$:matches:") = "matches" ]; then
        exe=$(basename $fullexe)
        mkdir -p  $ROOTDIR/build/$exe
        cp $fullexe $ROOTDIR/build/$exe/$exe
        strip $ROOTDIR/build/$exe/$exe
        echo $GITHASH > $ROOTDIR/build/$exe/git-hash.txt
    else
        echo "Skipping $fullexe"
    fi
done

echo $GITHASH > $ROOTDIR/build/git-hash.txt
