set -ex

STEP=$1
BUILD=$2

STACK="stack +RTS -N2 -RTS --no-terminal --system-ghc --skip-ghc-check"

CONCURRENCY=-j2
# if [ "$HC" = "ghc-8.2.2" ]; then CONCURRENCY=-j1; fi

timed () {
    JOB_CURR_TIME=$(date +%s)
    JOB_DURR=$((JOB_START_TIME + 4800 - JOB_CURR_TIME))
    echo time to run $JOB_DURR
    if [ $JOB_DURR -lt 600 ]; then
        echo "Less than 10 minutes to go, aborting"
        exit 1
    else
        timeout "$JOB_DURR" "$@"
    fi
}

case $STEP in
prepare)
    echo "$(ghc --version) [$(ghc --print-project-git-commit-id 2> /dev/null || echo '?')]"
    mkdir -p ~/.local/bin

    case $BUILD in
    stack)
        # Download and unpack the stack executable

        # if [ ! -e ~/.local/bin/stack ]; then
        #    travis_retry curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack';
        #  fi
        if [ ! -e ~/.local/bin/stack ]; then
            curl -L http://oleg.fi/stack.xz | xz --decompress --stdout - > ~/.local/bin/stack; chmod a+x ~/.local/bin/stack
        fi

        stack +RTS -N1 -RTS --version
        ;;

    cabal)
        cabal --version

        cabal update -v
        sed -i 's/^jobs:/-- jobs:/' "$HOME/.cabal/config"
        rm -fv cabal.project.local
        rm -fv cabal.project.freeze
        ;;

    esac
    ;;

install)

    # common data
    cp futurice-constants/constants.sample.json futurice-constants/constants.json
    cp futurice-tribes/tribes.sample.json futurice-tribes/tribes.json
    cp futurice-tribes/cost-centers.sample.json futurice-tribes/cost-centers.json
    cp hc-app/early-caring.template.sample hc-app/early-caring.template

    case $BUILD in
    stack)
        timed "$STACK" build --test --only-snapshot -j2 --ghc-options=-j2 futurice-prelude
        timed "$STACK" build --test --only-snapshot -j2 --ghc-options=-j2 servant-Chart
        timed "$STACK" build --test --only-snapshot -j2 --ghc-options=-j2
        ;;

    cabal)
        # Install doctest
        scripts/cabal-new-install.py doctest doctest ~/.local/bin
        doctest --version

        # Install some stuff already in install phase
        timed cabal new-build --enable-tests $CONCURRENCY futurice-prelude
        timed cabal new-build --enable-tests $CONCURRENCY servant-Chart
        ;;

    esac
    ;;

build)

    case $BUILD in
    stack)
        if [ "$PEDANTIC" = "YES" ]; then
            STACKOPTS=--pedantic
        fi

        timed "$STACK" build --test $STACKOPTS -j1 --ghc-options=-j2
        ;;

    cabal)
        timed cabal new-build --enable-tests  $CONCURRENCY all
        timed cabal new-test --enable-tests all

        # Prepare environment
        for envfile in .ghc.environment.*; do
            mv $envfile .ghc.environment.tmp
            grep -vE 'package-id base-compat-[0-9]' .ghc.environment.tmp > $envfile
        done

        # Run doctest on selected packages
        doctest --fast fum-types/src
        doctest --fast futurice-logo/src
        ;;

    esac
    ;;
esac
