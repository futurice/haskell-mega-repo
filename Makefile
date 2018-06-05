.PHONY : all deps.png packdeps \
	install-ubuntu-dependencies \
	install-tools \
	install-mega-repo-tool \
	check-dirty check-checksums generate-checksums \
	doctest

CONCURRENCY?=2

HC:=ghc-8.2.2
LOCALBIN:=${HOME}/.local/bin

SECRETJSONS:=futurice-constants/constants.json futurice-tribes/tribes.json futurice-tribes/cost-centers.json futurice-tribes/offices.json futurice-tribes/companies.json hc-app/early-caring.template reports-app/missing-hours-email.template reports-app/missing-hours-sms.template

# Build everything
all :
	time nice cabal new-build -w $(HC) -j$(CONCURRENCY) --enable-tests all

# Install tools (from Hackage)
install-tools : $(LOCALBIN)

# Install mega-repo-tool
install-mega-repo-tool : $(LOCALBIN)
	cabal new-build -w $(HC) mega-repo-tool -j$(CONCURRENCY)
	cp `cabal-plan list-bin mega-repo-tool` $(LOCALBIN)

$(LOCALBIN) :
	mkdir -p $(LOCALBIN)

# Dependency graph
deps.png :
	cabal-plan --hide-builtin --hide-global dot --tred --tred-weights | dot -Tpng -o$@

packdeps :
	# Ignore base(-4.11) and time(-1.9)
	# Also don't fail
	packdeps -q -e base -e time */*.cabal || true

# Checks
check-dirty :
	@if [ -f cabal.project.local ]; then echo "Cannot do production build with cabal.project.local"; false; fi
	@if [ ! -z "$$(git status --porcelain)" ]; then echo "Dirty WORKINGDIR (git status)"; git status; false; fi

# Data files
check-checksums : data.sha256sums
	sha256sum -c data.sha256sums

generate-checksums :
	sha256sum $(SECRETJSONS) > data.sha256sums

copy-samples :
	for datafile in ${SECRETJSONS}; do echo $${datafile}; if [ -f $${datafile} ]; then echo "exists!"; else cp $$(echo $${datafile} | sed -E 's/.(json|template)/.sample.\1/') $${datafile}; fi; done

# Doctest - ~works
doctest :
	doctest --fast fum-types/src
	doctest --fast futurice-logo/src

# Linux
install-ubuntu-dependencies :
	apt install libfftw3-dev libpq-dev liblzma-dev zlib1g-dev g++ pkg-config apt-transport-https ca-certificates curl software-properties-common
