#!/bin/sh

PACKAGES=$(cabal-plan --hide-builtin --hide-global topo \
    | sed "s,\x1B\[[0-9;]*[a-zA-Z],,g" \
    | awk '{ print $1 }' \
    | sort -u \
    | sed -r 's/-[0-9]+(\.[0-9]+)*$//' \
    | grep -vE '^(amazonka|plotlyhs|lucid-extras|dashdo|Chart|Chart-diagrams)$' \
    )

if [ -f cabal.project.local ]; then
    echo "cabal.project.local already exists..."
    exit 1
else
    for pkg in $PACKAGES; do
        echo "package $pkg"           >> cabal.project.local
        echo "  ghc-options: -Werror" >> cabal.project.local
        echo ""                       >> cabal.project.local
    done
fi
