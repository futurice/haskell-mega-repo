# Kleisli: Haskell on AWS Lambda

> AWS Lambda functions aren't pure ones.

## Synopsis

On Linux

```hs
make
```

will create an uploadable `Kleisli.zip` with a simple AWS lambda function.

![Screenshot](https://raw.githubusercontent.com/futurice/haskell-aws-lambda-kleisli/master/screenshot.png)

This is a "boilerplate" to *lift* `handler :: (String -> IO ()) -> Value -> IO Value` into AWS Lambda execution context! (first argument is a logging function).

## How it works

There three layers:

- AWS sees a lambda function written in Python ([`Kleisli.py`](https://github.com/futurice/haskell-aws-lambda-kleisli/blob/master/Kleisli.py))
- Python function uses a C extension ([`kleisli_native.c`](https://github.com/futurice/haskell-aws-lambda-kleisli/blob/master/kleisli_native.c))
- which uses Haskell foreign-library ([`src/Kleisli.hs`](https://github.com/futurice/haskell-aws-lambda-kleisli/blob/master/src/Kleisli.hs))

`Makefile` ties all that together.

## Notes

### Why?

- Everyone talks about AWS Lambda (or is it already an old thing?)
- Haskell is cool
- Cabal supports `foreign-library`
- I had an excuse to write ~60 LOC of C

### Why not child process?

Going through `Python` and using shared object seems to be much cleaner
approach than spawning a child process.

### Assumptions

- Ubuntu 16.04 (binaries seems to work ok on Amazon Linux AMI)
- Python-2.7 (system default, `apt install python-dev`)
- GHC-8.2.2 and cabal-install-head (from [HVR PPA](https://launchpad.net/~hvr/+archive/ubuntu/ghc))

Some paths are hardcoded, grep for 2.7 and 8.2.2.

### Package size

The resulting Zip file is 14M large (50M is the maximum package size, see [Limits](https://docs.aws.amazon.com/lambda/latest/dg/limits.html)), it's using `lens`.

### Logging

AWS Lambda environment seems to hook `print` somehow to enable logging.  Printing to `stdout` (e.g. `print`, `putStrLn` etc.) doesn't put anything into logs.  Therefore there is a small "workaround": we pass Python's `print` as an argument to Haskell's handler.

### ctypes

Using `ctypes` to load Haskell dynamic library could also work. Not sure how to make logging work then though. See e.g. [Building AWS Lambda function in Rust](http://blog.hde.co.jp/entry/2016/02/18/160646) for an example.

### Related work

- https://github.com/seek-oss/serverless-haskell, maturer? uses NodeJS + child process

### Links

- [Python: Extending Python with C or C++](https://docs.python.org/2/extending/extending.html)
- [Python: Building C and C++ Extensions with distutils](https://docs.python.org/2/extending/building.html#building)
- [Cabal: foreign libraries](http://cabal.readthedocs.io/en/latest/developing-packages.html#foreign-libraries)
- [AWS: Lambda Function Handler (Python)](https://docs.aws.amazon.com/lambda/latest/dg/python-programming-model-handler-types.html)
- [AWS: Lambda Execution Environment and Available Libraries](https://docs.aws.amazon.com/lambda/latest/dg/current-supported-versions.html)
- [AWS: Creating a Deployment Package (Python)](https://docs.aws.amazon.com/lambda/latest/dg/lambda-python-how-to-create-deployment-package.html)
- [AWS: Example deployment](https://docs.aws.amazon.com/lambda/latest/dg/with-s3-example-upload-deployment-pkg.html)
