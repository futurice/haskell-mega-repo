{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FlowdockProxy.IndexPage (indexPage) where

import Data.Bits        (shiftR, xor, (.&.))
import Data.Maybe       (isNothing)
import Data.Ord         (comparing)
import Data.Word        (Word64)
import Futurice.Prelude
import Prelude ()

import qualified Chat.Flowdock.REST    as FD
import qualified CMarkGFM
import qualified Data.Map.Strict       as Map
import qualified Data.Text             as T
import qualified Data.Text.Short       as TS
import qualified Text.HTML.SanitizeXSS as XSS

import Futurice.App.FlowdockProxy.DB
import Futurice.App.FlowdockProxy.Markup

-- comment to force template-hasksell recompilation
indexPage
    :: FD.Organisation           -- ^ flowdock organisation
    -> Map FD.FlowId (Int, Text) -- ^ known flows
    -> Maybe Text
    -> Maybe FD.FlowId
    -> [(FD.FlowId, Row)]
    -> HtmlPage "index-page"
indexPage org flows mneedle mflow rows = page_ "Flowdock text search" (Just NavHome) $ do
    form_ [ action_ "#", method_ "GET" ] $ do
        div_ [ class_ "row" ] $ do
            div_ [ class_ "columns medium-5" ] $ input_ [ name_ "needle", type_ "text", placeholder_ "Search for...", value_ $ fromMaybe "" mneedle ]
            div_ [ class_ "columns medium-5" ] $ select_ [ name_ "flow" ] $ do
                optionSelected_ (Nothing == mflow) [ value_ "all" ] $ i_ "All flows"
                for_ (sortBy (comparing (snd . snd)) $ Map.toList flows) $ \(flowId, (_, flowName)) ->
                    optionSelected_ (Just flowId == mflow) [ value_ $ view packed $ FD.getIdentifier flowId ] $ toHtml $ flowName
            div_ [ class_ "columns medium-2" ] $ input_ [ type_ "submit", class_ "button", value_ "Search" ]

    if | isNothing mneedle -> mempty
       | null rows         -> hr_ [] >> i_ "No matches"
       | otherwise         -> do
            p_ "Showing at most 1000 most recent case-insensitive matches"

            table_ $ do
                thead_ $ do
                    tr_ $ do
                        th_ "Date"
                        when (isNothing mflow) $ th_ "Flow"
                        th_ "Nick"
                        th_ [ width_ "80%" ] "Message"
                        th_ "Link"

                tbody_ $ for_ rows $ \(flowId, row) ->
                    tr_ $ do
                        td_ $ do
                              let stamp = formatHumanHelsinkiTime $ rowCreatedAt row
                              span_ [ class_ "nowrap", title_ stamp ] $ toHtml $ T.take 10 stamp
                        when (isNothing mflow) $ td_ $ case flows ^? ix flowId of
                            Nothing         -> shorten $ T.pack $ FD.getIdentifier flowId
                            Just (_, fname) -> shorten fname
                        td_ $ userIdToHtml $ rowUser row
                        td_ [ class_ "message" ] $ toHtmlRaw $ XSS.sanitizeBalance $ CMarkGFM.commonmarkToHtml [] [] $ TS.toText $ rowText row
                        td_ $ a_ [ href_ $ msgUrl flowId row ] "###"
  where
    shorten :: Monad m => Text -> HtmlT m  ()
    shorten t
        | T.length t > 15 = span_ [ title_ t ] $ toHtml $ T.take 12 t <> "..."
        | otherwise       = toHtml t

    msgUrl :: FD.FlowId -> Row -> Text
    msgUrl flowId row = mconcat
        [ "https://flowdock.com/app/"
        , org ^. FD.orgParamName . getter FD.getParamName . packed
        , "/"
        , FD.getIdentifier flowId ^. packed
        , "/messages/"
        , textShow $ FD.getIdentifier $ rowMessageId row
        ]

    users :: Map FD.UserId Text
    users = Map.fromList
        [ (orgUser ^. FD.userId, orgUser ^. FD.userNick)
        | orgUser <- org ^.. FD.orgUsers . folded
        ]

    userIdToHtml :: Monad m => FD.UserId -> HtmlT m ()
    userIdToHtml uid = case users ^? ix uid of
        Nothing   -> i_ [ class_ cls ] $ toHtml $ textShow $ FD.getIdentifier uid
        Just nick -> b_ [ class_ cls ] $ shorten nick
      where
        uid' = FD.getIdentifier uid
        clsN = mix64variant13 (fromIntegral uid') .&. 0x7
        cls  = "nick-" <> textShow clsN

mix64variant13 :: Word64 -> Word64
mix64variant13 z0 =
   -- Better Bit Mixing - Improving on MurmurHash3's 64-bit Finalizer
   -- http://zimbry.blogspot.fi/2011/09/better-bit-mixing-improving-on.html
    let z1 = shiftXorMultiply 30 0xbf58476d1ce4e5b9 z0 -- MurmurHash3 mix constants
        z2 = shiftXorMultiply 27 0x94d049bb133111eb z1
        z3 = shiftXor 31 z2
    in z3

shiftXor :: Int -> Word64 -> Word64
shiftXor n w = w `xor` (w `shiftR` n)

shiftXorMultiply :: Int -> Word64 -> Word64 -> Word64
shiftXorMultiply n k w = shiftXor n w * k
