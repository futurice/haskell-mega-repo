# haskell-mega-repo

> Sometimes you need to make a change across the package boundaries

![dependency graph](https://raw.githubusercontent.com/futurice/haskell-mega-repo/master/deps.png)

## Getting started

1. Clone the repository `git clone --recursive git@github.com:futurice/haskell-mega-repo.git`
    - If you didn't clone recursively, fetch the submodules with `git submodule update --init`
2. Install native dependencies:
    - macOS:
        - `brew install fftw pkg-config python3`
    - Ubuntu: `sudo make install ubuntu-dependencies`
3. Install cabal and ghc
    - Ubuntu: See https://launchpad.net/~hvr/+archive/ubuntu/ghc
        - `apt-get install ghc-8.4.4 cabal-install-head`
    - macOS:
	- From [haskell.futurice.com](https://haskell.futurice.com/): `curl -sL https://haskell.futurice.com/haskell-on-macos.py | python3 - --make-dirs --paths.d --ghc-alias=8.4.4 --cabal-alias=head install ghc-8.2.2 ghc-8.4.4 cabal-install-head`
4. `cabal new-run theme-app-server`

We use `new-` commands, e.g.
- `cabal new-repl theme-app` to run GHCi repl in the `theme-app` library.
- `cabal new-test dynmap-cache` to run tests of `dynmap-cache`.

## Goodies

Haskell's `optparse-applicative` based cli interfaces support auto completion.
If you use *oh my zsh*, then you have most things set-up already, you only need to

```zsh
mega-repo-tool --zsh-completion-script $(which mega-repo-tool) > $HOME/.oh-my-zsh/completions/_mega-repo-tool
```

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
