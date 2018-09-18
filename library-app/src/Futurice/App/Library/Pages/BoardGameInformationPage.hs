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
            a_ [class_ "button small", href_ $ linkToText $ fieldLink editBoardGamePageGet (boardGameResponse ^. boardgameResponseInformationId)] "Edit board game information"
            table_ $ do
                tr_ $ do
                    th_ "Name"
                    td_ $ toHtml $ boardGameResponse ^. boardgameResponseName
                tr_ $ do
                    th_ "Publisher"
                    td_ $ toHtml $ fromMaybe "" $ boardGameResponse ^. boardgameResponsePublisher
                tr_ $ do
                    th_ "Published"
                    td_ $ toHtml $ maybe "" show $ boardGameResponse ^. boardgameResponsePublished
                tr_ $ do
                    th_ "Designer"
                    td_ $ toHtml $ fromMaybe "" $ boardGameResponse ^. boardgameResponseDesigner
                tr_ $ do
                    th_ "Artist"
                    td_ $ toHtml $ fromMaybe "" $ boardGameResponse ^. boardgameResponseArtist
    fullRow_ $ do
        h2_ "Board games"
        for_ officeMap $ \(lib, bs) -> do
            case lib of
              OfficeLibrary library -> do
                  h3_ $ T.pack $ show $ toHtml library
                  for_ (listToMaybe $ (snd . partitionByLoan) bs) $ \_ -> do
                      span_ [style_ "padding-left: 10px; padding-right: 10px;"] $ toHtml $ (show . length . snd . partitionByLoan) bs <> " board games available"
                      -- HOX! We currently don't want to loan boardgames, but this can be opened up later
                      -- button_ [class_ "button tiny",
                      --          data_ "futu-id" "loan-item",
                      --          data_ "item-id" (T.pack $ show binfoid),
                      --          data_ "library" (T.pack $ show $ toHtml library)] $ toHtml ("Borrow" :: Text)
                  for_ (listToMaybe $ fst (partitionByLoan bs)) $ \_ -> table_ $ do
                      thead_ $ tr_ $ th_ $ toHtml $ idT $ "Copies on loan "
                      tbody_ $ for_ (fst $ partitionByLoan bs) $ \b ->
                        for_ (loanMap ^.at (_boardGamesBoardGameId b)) $ \(_, day, person) -> tr_ $ do
                          td_ $ toHtml $ (idToName employees $ person)
                          td_ $ toHtml $ T.pack $ show day
              _ -> pure ()
  where
    idT :: Text -> Text
    idT = id
    officeMap = M.toList $ M.fromListWith (++) $ (\x -> (_boardGamesLibrary x, [x])) <$> (boardGameResponse ^. boardgameResponseGames)
    loanMap = M.fromList $ (\(LoanData lid day person iid) -> (iid, (lid, day, person))) <$> ls
    partitionByLoan = partition (\x -> isJust (loanMap ^.at (_boardGamesBoardGameId x)))
