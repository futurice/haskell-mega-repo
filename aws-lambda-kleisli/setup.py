from distutils.core import setup, Extension

kleisli_module = Extension(
    "kleisli_native",
    include_dirs = ["/opt/ghc/8.2.2/lib/ghc-8.2.2/include"],
    libraries = ["kleisli-haskell"],
    library_dirs = ["haskell/lib"],
    sources = ["kleisli_native.c"])

setup(
    name="kleisli_native",
    version="1.0.0",
    description="Run Haskell function on AWS Lambda",
    author="Oleg Grenrus",
    author_email="oleg.grenrus@iki.fi",
    url="https://github.com/futurice/haskell-aws-lambda-kleisli",
    ext_modules=[kleisli_module])
