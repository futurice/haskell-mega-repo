import os
import sys
import time

for k, v in os.environ.items():
    print("%s=%s" % (k, v))

for i in range(0,10):
    print(i)
    sys.stdout.flush()
    time.sleep(1)

print("END")
