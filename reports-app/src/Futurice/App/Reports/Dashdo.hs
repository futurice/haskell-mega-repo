module Futurice.App.Reports.Dashdo (makeDashdoServer) where

import Dashdo.Rdash          (rdash)
import Dashdo.Servant
import Futurice.Prelude
import Futurice.Servant      (Cache)
import Graphics.Plotly.Lucid (plotlyCDN)
import Prelude ()
import Servant               (Server)

import Futurice.App.Reports.BalancesDashdo
import Futurice.App.Reports.Config
import Futurice.App.Reports.MissingHoursDashdo
import Futurice.App.Reports.GithubReposDashdo

type Ctx = (Cache, Manager, Logger, Config)

makeDashdoServer :: Ctx -> IO (Server DashdoAPI)
makeDashdoServer ctx = do
    let html = rdash dashdos plotlyCDN
    dashdoServer id html dashdos
  where
    dashdos =
        [ missingHoursRDashdo ctx
        , balancesRDashdo ctx
        , githubReposRDashdo ctx
        ]
