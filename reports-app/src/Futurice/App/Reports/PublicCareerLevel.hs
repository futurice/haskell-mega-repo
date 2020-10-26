{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Reports.PublicCareerLevel where

import Futurice.CareerLevel      (careerLevelToText)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()

import qualified Personio as P

newtype PublicCareerLevelData = PublicCareerLevelData [PublicCareerLevel] deriving (Generic, NFData, ToSchema)

data PublicCareerLevel = PublicCareerLevel
  { name        :: !Text
  , careerLevel :: !Text
  } deriving (Generic, NFData, ToSchema)

publicCareerLevelData :: (MonadPersonio m) => m PublicCareerLevelData
publicCareerLevelData = do
    emps <- P.personio P.PersonioEmployees
    let emps' = filter (\e -> e ^. P.employeeStatus == P.Active) emps
    return $ PublicCareerLevelData $ catMaybes $ toCareerLevel <$> emps'
  where
    toCareerLevel emp = PublicCareerLevel
      <$> (Just $ emp ^. P.employeeFullname)
      <*> (careerLevelText . careerLevelToText =<< (emp ^. P.employeePublicCareerLevel))
    careerLevelText lvl | lvl == "" = Nothing
                        | otherwise = Just lvl

-------------------------------------------------------------------------------
-- Html
-------------------------------------------------------------------------------

instance ToHtml PublicCareerLevelData where
    toHtmlRaw = toHtml
    toHtml = toHtml . renderPublicCareerLevelData

renderPublicCareerLevelData :: PublicCareerLevelData -> HtmlPage "public-career-level"
renderPublicCareerLevelData (PublicCareerLevelData levels) = page_ "Public career levels" $ do
    fullRow_ $ h1_ "Public career levels"
    fullRow_ $ sortableTable_ $ do
        thead_ $ do
            th_ "Name"
            th_ "Public Career Level"
        tbody_ $ do
            for_ (sortOn name levels) $ \(PublicCareerLevel name' level) -> tr_ $ do
                td_ $ toHtml name'
                td_ $ toHtml level
