all : Kleisli.zip
	@echo "Now you can upload Kleisli.zip to AWS Lambda!"

.PHONY : all build build-haskell build-python clean example
.PHONY : docker docker-setup install-ubuntu-dev

clean :
	rm -rf Kleisli.zip build dist-newstyle haskell

build : build-python

build-haskell : kleisli.cabal src/Kleisli.hs
	# Build Haskell
	cabal new-build .
	# Make directories
	mkdir -p haskell/include
	mkdir -p haskell/lib
	# Copy header and .so library
	cp $$(find dist-newstyle/build -type f -name Kleisli_stub.h) haskell/include/kleisli_haskell.h
	cp $$(find dist-newstyle/build -type f -name libkleisli-haskell.so) haskell/lib

build-python : build-haskell kleisli_native.c setup.py
	python2.7 setup.py build

Kleisli.zip : build-python Kleisli.py
	# Remove old
	rm -f $@
	# Add files
	cd build/lib.linux-x86_64-2.7 && zip -9 ../../$@ *
	cd haskell/lib && zip -9 ../../$@ *
	ldd haskell/lib/libkleisli-haskell.so | python2.7 collect-shared-libs.py $@
	zip -9 $@ Kleisli.py
	# Test
	zip -Tv $@
	# Print (with size) size
	ls -lh $@

# Run an example locally
example :
	PYTHONPATH=build/lib.linux-x86_64-2.7 LD_LIBRARY_PATH=haskell/lib python example.py

# Install some dependencies
# We assume you have ghc-8.2.2 and cabal-install-head installed from hvr-ppa
# https://launchpad.net/~hvr/+archive/ubuntu/ghc
install-ubuntu-dev :
	apt install python2.7-dev zip

docker :
	docker run --entrypoint=bash --rm -ti -v $$(pwd):/app -w /app phadej/ghc:8.2.2-ubuntu

docker-setup :
	apt-get update
	apt-get install cabal-install-head
	/opt/cabal/head/bin/cabal update
	make install-ubuntu-dev
	@echo 'export PATH=/opt/cabal/head/bin:$$PATH'
