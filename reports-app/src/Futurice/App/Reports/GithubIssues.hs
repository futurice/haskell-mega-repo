{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
#endif
-- | GitHub issues in some repositories.
module Futurice.App.Reports.GithubIssues (
    -- * Report
    IssueReport,
    issueReport,
    -- * Types
    GitHubRepo (..),
    IssueInfo (..),
    -- * Lenses
    ghRepoOwner, ghRepoName,
    issueNumber, issueTitle, issueUrl, issueCreated,
    ) where

import Prelude ()
import Futurice.Prelude
import Futurice.Generics
import Futurice.Integrations
import Futurice.Lucid.Foundation
import Futurice.Report.Columns

import qualified Data.Csv          as Csv
import qualified Data.Tuple.Strict as S
import qualified Data.Vector       as V
import qualified GitHub            as GH

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

-- /TODO/ move to own module or even to futurice-integrations
data GitHubRepo = GitHubRepo
    { _ghRepoOwner :: !(GH.Name GH.Owner)
    , _ghRepoName  :: !(GH.Name GH.Repo)
    }
  deriving (Eq, Ord, Show, Typeable, Generic)
  deriving anyclass (NFData)

makeLenses ''GitHubRepo
deriveGeneric ''GitHubRepo
deriveVia [t| ToJSON GitHubRepo `Via` Sopica GitHubRepo |]
instance ToSchema GitHubRepo where declareNamedSchema = sopDeclareNamedSchema

instance ToColumns GitHubRepo where
    type Columns GitHubRepo = '[GH.Name GH.Owner, GH.Name GH.Repo]
    toColumns (GitHubRepo o r) = [I o :* I r :* Nil]

instance ReportValue GitHubRepo where
    reportValueHtml (GitHubRepo o r) = do
        a_ [ href_ ownerLink ] $ toHtml o'
        " / "
        a_ [ href_ repoLink ] $ toHtml r'
      where
         o' = GH.untagName o
         r' = GH.untagName r
         ownerLink = "https://github.com/" <> o'
         repoLink = ownerLink <> "/" <> r'



data IssueInfo = IssueInfo
    { _issueNumber  :: !Int
    , _issueTitle   :: !Text
    , _issueCreated :: !UTCTime
    , _issueUrl     :: !Text
    }
  deriving (Eq, Ord, Show, Generic, Typeable)
  deriving anyclass (NFData)

makeLenses ''IssueInfo
deriveGeneric ''IssueInfo
deriveVia [t| ToJSON IssueInfo `Via` Sopica IssueInfo |]
instance ToSchema IssueInfo where declareNamedSchema = sopDeclareNamedSchema

-- | 'IssueInfo' is just wrapped into column. The creation date is extracted
-- into additional column though.
--
-- This is purely for convenience, we could have a another type as well.
instance ToColumns IssueInfo where
    type Columns IssueInfo = '[IssueInfo, UTCTime]
    columnNames _ = K "issue" :* K "created" :* Nil
    toColumns i = [I i :* I (_issueCreated i) :* Nil]

instance Csv.ToField IssueInfo where
    toField i = Csv.toField $
        "#" <> textShow (_issueNumber i) <>
        ": " <> _issueTitle i

instance ReportValue IssueInfo where
    reportValueHtml i = a_ [href_ (_issueUrl i) ] $ toHtml $
        "#" <> textShow (_issueNumber i) <>
        ": " <> _issueTitle i

-------------------------------------------------------------------------------
-- Report
-------------------------------------------------------------------------------

type IssueReport = Report
    "GitHub issues"
    ReportGenerated
    (Vector :$ StrictPair GitHubRepo :$ Vector :$ IssueInfo)

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

issueReport
    :: forall m. (MonadTime m, MonadGitHub m)
    => [GitHubRepo]
    -> m IssueReport
issueReport repos = do
    now <- currentTime
    Report (ReportGenerated now) . V.fromList <$> traverse fetch repos
  where
    fetch :: GitHubRepo -> m (StrictPair GitHubRepo (Vector IssueInfo))
    fetch ghr@(GitHubRepo owner repo) = (S.:!:) ghr . fmap t <$>
       githubReq (GH.issuesForRepoR owner repo opts GH.FetchAll)

    opts = GH.stateOpen

    t pr = IssueInfo
        { _issueNumber  = GH.issueNumber pr
        , _issueTitle   = GH.issueTitle pr
        , _issueCreated = GH.issueCreatedAt pr
        , _issueUrl     = maybe "" GH.getUrl $ GH.issueHtmlUrl pr
        }
