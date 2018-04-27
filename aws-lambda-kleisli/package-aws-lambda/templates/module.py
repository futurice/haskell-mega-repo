from __future__ import print_function

import atexit
import json
import {{nativeModuleName}}

# We init Haskell RTS once when the module is loaded.
{{nativeModuleName}}.hs_init(["haskell-on-aws", "+RTS", "-T"])

# Not sure, if need to cleanup Haskell RTS.
def cleanup():
    {{nativeModuleName}}.hs_exit()

atexit.register(cleanup)

{{#handlers}}
def {{py}}(event, context):
    return json.loads({{nativeModuleName}}.{{py}}(json.dumps(event), context, print))
{{/handlers}}
