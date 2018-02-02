# planmill-client

> Bindings to [PlanMill API 1.5](https://online.planmill.com/pmtrial/schemas/v1_5/index.html).

[![Build Status](https://travis-ci.org/futurice/haskell-planmill-client.svg?branch=master)](https://travis-ci.org/futurice/haskell-planmill-client)
[![Hackage](https://img.shields.io/hackage/v/planmill-client.svg)](http://hackage.haskell.org/package/planmill-client)
[![Stackage LTS 2](http://stackage.org/package/planmill-client/badge/lts-2)](http://stackage.org/lts-2/package/planmill-client)
[![Stackage LTS 3](http://stackage.org/package/planmill-client/badge/lts-3)](http://stackage.org/lts-3/package/planmill-client)
[![Stackage Nightly](http://stackage.org/package/planmill-client/badge/nightly)](http://stackage.org/nightly/package/planmill-client)

## Test usage

```
$ cabal new-repl
:m *PlanMill.Test PlanMill PlanMill.Endpoints PlanMill.Types.Cfg PlanMill.Types.Identifier Data.Time.TH
```

```hs
let cfg = Cfg (Ident 42) "secret" mockEndpoint
ts <- evalPlanMillIO cfg $ timereportsFromInterval $ ResultInterval IntervalStart $(mkUTCTime "2015-12-01T00:00:00+03:00") $(mkUTCTime "2016-01-01T00:00:00+03:00")
print ts
```

### Paged get

```hs
ps <- evalPlanMillIO cfg projects
```

### Post request

```hs
import qualified Data.Vector as V
let Timereports ts' = ts
now <- getCurrentTime
let tr = ts' V.! 0
let ntr = NewTimereport (trTask tr) now (7*60 + 30) "Todays hours"
evalPlanMillIO cfg $ addTimereport tr
```

## Integration tests

```
travis encrypt -r futurice/haskell-planmill-client 'INTEGRATION_PARAMS="uid apikey endpoint"'
```

## Development tips

```sh
# HLint source
hlint --cpp-file=$(stack path --dist-dir)/build/autogen/cabal_macros.h src

# stylish-haskell changed file
git ls-files -m|grep '\.hs$'|xargs stylish-haskell --inplace
```
