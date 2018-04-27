from distutils.core import setup, Extension

module = Extension(
    "{{nativeModuleName}}",
    include_dirs = ["/opt/ghc/8.2.2/lib/ghc-8.2.2/include"],
    libraries = ["{{foreignLib}}"],
    library_dirs = ["."],
    sources = ["{{nativeModuleName}}.c"])

setup(
    name="{{nativeModuleName}}",
    version="1.0.0",
    description="Run Haskell function on AWS Lambda",
    ext_modules=[module])
