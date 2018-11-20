{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Library.Pages.BoardGameInformationPage where

import Data.List        (partition)
import Data.Maybe       (isJust)
import Futurice.IdMap   (IdMap)
import Futurice.Prelude
import Prelude ()
import Servant

import Futurice.App.Library.API
import Futurice.App.Library.Logic  (LoanData (..), idToName)
import Futurice.App.Library.Markup
import Futurice.App.Library.Types

import qualified Data.Map  as M
import qualified Data.Text as T
import qualified Personio  as P

boardGameInformationPage :: BoardGameInformationResponse -> [LoanData] -> IdMap P.Employee -> HtmlPage ("boardgameinformation")
boardGameInformationPage boardGameResponse ls employees = page_ "Board Game details" (Nothing :: Maybe Nav) $ do
    fullRow_ $ do
        div_ [] $ do
            table_ $ do
                vertRow_ "Name" $ toHtml $ boardGameResponse ^. boardGameResponseName
                vertRow_ "Publisher" $ toHtml $ fromMaybe "" $ boardGameResponse ^. boardGameResponsePublisher
                vertRow_ "Published" $ toHtml $ maybe "" show $ boardGameResponse ^. boardGameResponsePublished
                vertRow_ "Designer" $ toHtml $ fromMaybe "" $ boardGameResponse ^. boardGameResponseDesigner
                vertRow_ "Artist" $ toHtml $ fromMaybe "" $ boardGameResponse ^. boardGameResponseArtist
            a_ [class_ "button small", href_ $ linkToText $ fieldLink editBoardGamePageGet (boardGameResponse ^. boardGameResponseInformationId)] "Edit board game information"
    fullRow_ $ do
        h2_ "Board games"
        for_ officeMap $ \(lib, bs) -> do
            case lib of
              OfficeLibrary library -> do
                  h3_ $ T.pack $ show $ toHtml library
                  unless (null $ (snd . partitionByLoan) bs) $ do
                      span_ [style_ "padding-left: 10px; padding-right: 10px;"] $ toHtml $ (show . length . snd . partitionByLoan) bs <> " board games available"
                      -- HOX! We currently don't want to loan boardgames, but this can be opened up later
                      -- button_ [class_ "button tiny",
                      --          data_ "futu-id" "loan-item",
                      --          data_ "item-id" (T.pack $ show binfoid),
                      --          data_ "library" (T.pack $ show $ toHtml library)] $ toHtml ("Borrow" :: Text)
                  unless (null $ fst (partitionByLoan bs)) $ table_ $ do
                      thead_ $ tr_ $ th_ $ toHtml $ idT $ "Copies on loan "
                      tbody_ $ for_ (fst $ partitionByLoan bs) $ \b ->
                        for_ (loanMap ^.at (_boardGamesBoardGameId b)) $ \(_, day, person) -> tr_ $ do
                          td_ $ toHtml $ (idToName employees $ person)
                          td_ $ toHtml $ T.pack $ show day
              _ -> pure ()
  where
    idT :: Text -> Text
    idT = id
    officeMap = M.toList $ M.fromListWith (++) $ (\x -> (_boardGamesLibrary x, [x])) <$> (boardGameResponse ^. boardGameResponseGames)
    loanMap = M.fromList $ (\(LoanData lid day person iid) -> (iid, (lid, day, person))) <$> ls
    partitionByLoan = partition (\x -> isJust (loanMap ^.at (_boardGamesBoardGameId x)))
