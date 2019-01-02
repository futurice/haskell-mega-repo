{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Futuroom.IndexPage where

import Data.Time
import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Lucid.Navigation (Navigation (..), pageParamsWithCSS, page_)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Futuroom.API
import Futurice.App.Futuroom.Types

import qualified Clay      as C
import qualified Data.Map  as M
import qualified Data.Text as T

data Nav = NavHome
    deriving (Eq, Enum, Bounded, Ord)

meetingRoomCss :: C.Css
meetingRoomCss = C.body C.? do
    ".meeting-room" C.? do
        C.float C.floatLeft
        C.width (C.px 200.0)
    ".event" C.? do
        C.position C.absolute
        C.width (C.px 200.0)
    ".timeline" C.? do
        C.position C.relative
    ".timeline" C.? C.ul C.? C.li C.? do
        C.height (C.px 240)
    ".timeline" C.? C.ul C.? C.li C.# C.before C.? do
        C.content $ C.stringContent ""
        C.display C.block
        C.height (C.px 1.0)
        C.backgroundColor C.black
    ".row" C.? do
        C.maxWidth C.none

instance Navigation Nav where
    serviceTitle _ = "Futuroom"

    navLink NavHome    = (recordHref_ indexPageGet Nothing, "Futuroom Home")

    pageParams = pageParamsWithCSS meetingRoomCss

indexPage :: Day -> Map MeetingRoom [Reservation] -> HtmlPage "indexpage"
indexPage day reservations = page_ "Futuroom" (Nothing :: Maybe Nav) $ do
    div_ [class_ "schedule"] $ do
        form_ [recordAction_ indexPageGet Nothing] $ do
            largemed_ 3 $ input_ [type_ "date", name_ "date", placeholder_ "Pick a date"]
            largemed_ 2 $ button_ [class_ "button", type_ "submit"] "Get events for day"
            largemed_ 1 $ span_ ""
        fullRow_ $ do
            h4_ $ toHtml $ formatTime defaultTimeLocale "%e %b %Y" day
            void $ flip M.traverseWithKey reservations $ \mname mreservations -> do
                div_ [class_ "meeting-room"] $ do
                    div_ [class_ "meeting-room-names"] $ span_ $ toHtml $ mrName mname
                    for_ mreservations $ \res -> do
                        case (resStartTime res, resEndTime res) of
                          (Just startTime, Just endTime) -> div_ [class_ "card event", style_ (cardStyle startTime endTime)] $ do
                              div_ [class_ "card-divider"] $ do
                                  span_ (toHtml $ toTime startTime <> " - " <> toTime endTime)
                              div_ [class_ "card-section"] $ do
                                  em_ (toHtml $ fromMaybe "" $ resTitle res)
                          _ -> pure ()
        div_ [class_ "timeline"] $ do
            ul_ [class_ "no-bullet"]$ do
                li_ $ span_ "08:00"
                li_ $ span_ "09:00"
                li_ $ span_ "10:00"
                li_ $ span_ "11:00"
                li_ $ span_ "12:00"
                li_ $ span_ "13:00"
                li_ $ span_ "14:00"
                li_ $ span_ "15:00"
                li_ $ span_ "16:00"
                li_ $ span_ "17:00"
                li_ $ span_ "18:00"
  where
      minimumTime [] currentMinimum = currentMinimum
      minimumTime res currentMinimum = minimum [minimum $ utctDayTime <$> catMaybes (resStartTime <$> res), currentMinimum]
      startingHour :: DiffTime
      startingHour = foldr minimumTime 21600 reservations
      cardStartPos = 234 :: Int -- Start of schedule area
      pixelsPerHalfHour = 120
      cardStyle startTime endTime = T.pack ("top: " <> (show . eventStart $ startTime) <> "px;" <> " height: " <> (show $ eventWidth startTime endTime) <> "px;")
      eventStart startTime = cardStartPos + round (((utctDayTime startTime - startingHour) / (30 * 60)) * pixelsPerHalfHour)
      eventWidth startTime endTime = eventStart endTime - eventStart startTime
      toTime :: UTCTime -> String
      toTime t = formatTime defaultTimeLocale "%R" $ utcToHelsinkiTime t
