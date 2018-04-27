from __future__ import print_function

import atexit
import json
import kleisli_native

# Init Haskell RTS
kleisli_native.hs_init(["ignored-command-name", "opt1", "opt2", "+RTS", "-A32m", "-T"])

# Register Haskell RTS exit
def cleanup():
    print("Cleanup...")
    kleisli_native.hs_exit()

atexit.register(cleanup)

# Call handler and print the result.
input = json.dumps("Sphinx of black quartz, judge my vow")
output = kleisli_native.handler(input, None, print)
print(output)
