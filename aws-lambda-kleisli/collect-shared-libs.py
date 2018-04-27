# This is a script to add references shared objects into zip file.
# It gets output of `ldd libkleisli-haskell.so` and adds referenced
# dynamic libraries to the zip file (`Kleisli.zip`).
from __future__ import print_function

import os.path
import re
import subprocess
import sys

# Zip file is first argument
zipFile = os.path.abspath(sys.argv[1])

def addToZip(fp):
    soName = os.path.basename(fp)
    dirName = os.path.dirname(fp)
    print("Zipping", soName, "from", dirName)
    p = subprocess.Popen(['/usr/bin/zip', '-9', zipFile, soName], cwd=dirName)
    p.wait()


# Libraries which exist in Amazon Linux AMI
skipLibs = [
    "librt",
    "libutil",
    "libdl",
    "libpthread",
    "libm",
    "libc",
    "linux-vdso",
]

# Libraries which we know aren't in Amazon Linux AMI
copyLibs = [
    "libgmp",
    "libffi",
]

# matching: somelib.so => /path/to/somelib.so
lineRe = re.compile(r"\s*(([a-zA-Z0-9\.\-_]+)\.so(:?\.[0-9\.]+)?)\s*=>\s*([a-zA-Z0-9\.\-_/]*)\s*\(")

# accumulator for found shared libraries
toAdd = []

for line in sys.stdin:
    m = lineRe.match(line)

    if m is not None:
        [unused, libName, ver, fp] = m.groups()

        if libName[0:5] == "libHS":
            toAdd.append(fp)
        elif libName in copyLibs:
            print("Found 'native' library to copy %s" % libName)
            toAdd.append(fp)
        elif libName in skipLibs:
            print("Found library which doesn't need to copy: %s" % libName)
        else:
            # If we find unknown library, fail.
            raise Exception("Unknown library: %s in %s" % (libName, fp))
    else:
        # This is usedul to debugging the script,
        # (almost) all lines should match

        if "/lib64/ld-linux-x86-64.so.2" in line or "linux-vdso.so" in line:
            pass
        else:
            raise Exception(line)

# Add found shared libraries to the zip file
for fp in toAdd:
    addToZip(fp)
