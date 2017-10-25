# haskell-mega-repo

> Sometimes you need to make a change across the package boundaries

![dependency graph](https://raw.githubusercontent.com/futurice/haskell-mega-repo/master/deps.png)

## Getting started

1. Clone the repository `git clone --recursive git@github.com:futurice/haskell-mega-repo.git`
    - If you didn't clone recursively, fetch the submodules with `git submodule update --init`
2. Install native dependencies:
    - macOS:
        - `brew install fftw pkg-config`
    - Ubuntu: `apt-get install libfftw3-dev libpq-dev`
3. Install [haskell stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
    - macOS:
        - `stack setup`
        - `stack install alex happy`
        - https://postgresapp.com/ and `source env-postgres-osx.sh`
4. Build and run *theme-app*
    - theme-app is a small app without external integrations.
    - `stack build --fast theme-app`
    - `stack exec -- theme-app-server`

## Alternative cabal new-build process

1. As above
2. As above
3. Install cabal and ghc
    - Ubuntu: See https://launchpad.net/~hvr/+archive/ubuntu/ghc
        - `apt-get install ghc-8.2.1 cabal-install-head`
    - macOS:
        - get `cabal` from https://haskell.futurice.com/
        - GHC: download from https://www.haskell.org/ghc/download_ghc_8_2_1.html#binaries
        - `sudo mkdir -p /usr/local/ghc/8.2.1 && sudo chown -R $(logname):admin /usr/local/ghc/`
        - `./configure --prefix=/usr/local/ghc/8.2.1/ && make install`
        - Add `echo /usr/local/ghc/8.2.1/bin > sudo tee /etc/paths.d/ghc`
4. `cabal new-run theme-app-server`

## Building docker images

Install `mega-repo-tool` from the repo.

`mega-repo-tool build-docker <app-name>`, for example
`mega-repo-tool build-docker checklist`.
If the tool instructs you to run some docker commands, then do it and re-run the build-docker command.

## Maintaining

### Tracking the master

Update external gitmodules:

```
git submodule foreach git checkout master
git submodule foreach git pull
```

### Package dependencies

```
mega-repo-tool packdeps
```

### Rough stats

```
mega-repo-tool stats
```

### Update deps graph

```
ega-repo-tool dot
```

### LTS version bumps

*outdated*

```
git submodule foreach git checkout master
vim -p $(find . -name stack-lts-3.yaml)
STACK_YAML=stack-lts-3.yaml stack test --pedantic
# Or if really want to test each package separately
export STACK_YAML=stack-lts-3.yaml
for i in favicon-app flowdock-grep; do echo $i; cd $i; stack test --pedantic; cd ..; done
# Commit each package changes...
```

## Futurice Haskell Guidelines

### Definitions

The packages can be divided into two groups: *applications* and *libraries*.
The characteristics of these groups differ, so we apply different guidelines

### Style guide

We adhere to [Johan Tibell's styleguide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md).

We use [stylish-haskell](https://github.com/jaspervdj/stylish-haskell).
[Example `stylish-haskell.yaml`](https://github.com/futurice/haskell-servant-status/blob/master/.stylish-haskell.yaml).

Packages have to be buildable with `-Wall -Werror` with some of the supported
GHCs (currently usually *8.0.2*).

### GHC Extensions

See [ghccaniuse](http://damianfral.github.io/ghcaniuse/). Don't use extensions,
which aren't supported by three or four GHC releases, i.e. using `DataKinds`
and other type-system extensions are ok (required by e.g. `servant` anyway).

For applications, new and straight-forward extensions like `DeriveAnyClass`,
`PartialTypeSignatures`, or upcoming `ApplicativeDo`, `StrictData` or
`SignatureSections` are ok as well. However, their usage isn't an end in
itself.

Also of `TupleSections` and `RecordWildCards` are ok.

In fact we (should) use following set of `default-extensions`:

```yaml
  default-extensions:
    - DeriveDataTypeable
    - DeriveFoldable
    - DeriveFunctor
    - DeriveGeneric
    - DeriveTraversable
    - NoImplicitPrelude
    - ScopedTypeVariables
```

Don't use `UnicodeSyntax`.

Use `CPP` sparingly. If you can get away with compat package (e.g.
[`base-compat`](http://hackage.haskell.org/package/base-compat)).  Some
warnings with *other* GHCs are ok.

### Three release policy

> Everything should be compilable with the last three releases of GHC.

We don't apply the three release policy for the applications. It's enough that
we can compile applications with some recent GHC version (currently *8.0.2*).

For libraries we aim to support the last three GHC versions, but this
doesn't apply to this repository, only for submodules.
