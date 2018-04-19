{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Personio where

import Control.Lens              (Getting)
import Data.Time                 (diffDays)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()

import Futurice.App.Checklist.Types

import qualified Personio as P

personioCheck
    :: (Monad m, Eq a)
    => IntegrationData
    -> Employee
    -> Getting a Employee a
    -> (P.Employee -> Maybe a)
    -> (a -> HtmlT m ())
    -> HtmlT m ()
personioCheck integrationData employee l f render = for_ my $ \y ->
    unless (x == y) $ do
        " "
        span_ [class_ "label alert"] $ do
            " ≠ "
            render y
  where
    x  = employee ^. l
    my = do
        pid <- employee ^. employeePersonio
        p   <- integrationData ^. personioData . at pid
        f p

-------------------------------------------------------------------------------
-- Extraction
-------------------------------------------------------------------------------

contractType :: P.Employee -> Maybe ContractType
contractType e = case e ^. P.employeeEmploymentType of
    Nothing -> Nothing
    Just P.External -> Just ContractTypeExternal
    Just P.Internal -> case e ^. P.employeeContractType of
        Nothing                      -> Nothing
        Just P.PermanentAllIn -> Just ContractTypePermanent
        Just P.FixedTerm      ->
            Just $ maybe ContractTypeFixedTerm fixedTermContract contractLength
        Just P.Permanent
            | e ^. P.employeeWeeklyHours >= 37 -> Just ContractTypePermanent
            | otherwise                        -> Just ContractTypePartTimer
  where
    contractLength = do
        hire <- e ^. P.employeeHireDate
        end  <- e ^. P.employeeEndDate
        return (diffDays end hire)

    fixedTermContract n
        | n > 180   = ContractTypeFixedTerm
        | otherwise = ContractTypeSummerWorker
