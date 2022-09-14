{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

--This is one file with code written by me, but it is part of a project with a larger group of programmers.

module Handler.ImportExportSource where
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Text (Text)
import Handler.Xlsx
import Internet.Source.Types
  ( FullImportSource (..),
    InternetProvider (..),
    ImportSourceIdentification (..),
    ImportSourceUUID (..),
  )

import Prelude
-- Internet Source type without Group Id for parsing
data ParsedImportSource = ParsedImportSource
  { parsedImportSourceUUID :: ImportSourceUUID,
    parsedImportSourceProvider :: InternetProvider,
    parsedImportSourceIdentification :: [ImportSourceIdentification],
    parsedImportSourceName :: Text,
    parsedImportSourceCompanyIdRef :: CompanyIdRef
  }
  deriving (Show, Read, Eq, Ord)
instance XlsxRow ParsedImportSource where
  toXlsxRow value rowIndex = do
    Map.fromList $
      zipWith
        ( \colIndex makeCell -> do
            ((rowIndex, colIndex), makeCell value)
        )
        [1 ..]
        [ toXlsxCell . parsedImportSourceUUID,
          toXlsxCell . parsedImportSourceProvider,
          toXlsxCell . parsedImportSourceIdentification,
          toXlsxCell . parsedImportSourceName,
          toXlsxCell . parsedImportSourceCompanyIdRef
        ]
  fromXlsxRow cellMap rowIndex = do
    ParsedImportSource <$> parseWithDefault rowIndex 1 (ImportSourceUUID "")
      <*> parseCell rowIndex 2
      <*> parseWithDefault rowIndex 3 []
      <*> parseCell rowIndex 4
      <*> parseCell rowIndex 5
    where
      parseWithDefault rowIndex colIndex defaultVal = do
        case Map.lookup (rowIndex, colIndex) cellMap of
          Nothing -> Right $ defaultVal
                    Just cell ->
            case fromXlsxCellSeparated cell of
              Nothing -> Right $ defaultVal
              Just cellVal ->
                case fromXlsxCellValue cellVal of
                  Nothing -> Left $ "Invalid cell value: " ++ show cellVal
                  Just value -> Right $ value

      parseCell rowIndex colIndex = do
        case Map.lookup (rowIndex, colIndex) cellMap of
          Nothing -> Left $ "Unable to parse cell " ++ show (rowIndex, colIndex)
          Just cell ->
            case fromXlsxCell cell of
              Nothing -> Left $ "Unable to parse cell " ++ show (rowIndex, colIndex) ++ " with cell " ++ show cell
              Just value -> Right $ value
instance XlsxSheet ParsedImportSource where
  xlsxSheetHeaders =
    [ "UUID",
      "Provider",
      "Identification",
      "Name",
      "Company ID"
    ]
  styleXlsx = "./spreadsheetTemplates/ImportSourceExportTemplate.xlsx"
parsedToFullImportSource :: GroupId -> ParsedImportSource -> FullImportSource
parsedToFullImportSource
  groupId
  ( ParsedImportSource
      { parsedImportSourceUUID,
        parsedImportSourceProvider,
        parsedImportSourceIdentification,
        parsedImportSourceName,
        parsedImportSourceCompanyIdRef
      }
    ) =
    FullImportSource
      parsedImportSourceUUID
      parsedImportSourceProvider
      parsedImportSourceIdentification
      parsedImportSourceName
      groupId
      parsedImportSourceCompanyIdRef
fullToParsedImportSource :: FullImportSource -> ParsedImportSource
fullToParsedImportSource
  ( FullImportSource
      { fullImportSourceUUID,
        fullImportSourceProvider,
        fullImportSourceIdentification,
        fullImportSourceName,
        fullImportSourceCompanyIdRef
      }
    ) =
    ParsedImportSource
      fullImportSourceUUID
      fullImportSourceProvider
      fullImportSourceIdentification
      fullImportSourceName
      fullImportSourceCompanyIdRef
checkAllowableEmpty :: ParsedImportSource -> Bool
checkAllowableEmpty (ParsedImportSource {parsedImportSourceProvider, parsedImportSourceIdentification}) =
  if parsedImportSourceIdentification == []
    then case parsedImportSourceProvider of
      UserProvided -> True
      _ -> False
    else True
exportFullImportSource :: [FullImportSource] -> IO (Either String LBS.ByteString)
exportFullImportSource fullSources = do
  let sources = map fullToParsedImportSource fullSources
  bytes <- toXlsxBytes $ sources
  let parsedSources = fromXlsxBytes bytes
  case parsedSources of
    Left err -> do
      return $ Left $ "Internal Error - Unable to parse the generated xlsx file: " ++ err
    Right parsedSources' -> do
      if sources == parsedSources'
        then return $ Right bytes
        else do
          return $ Left $ "Internal Error - The generated file did not match the existing parameters: " ++ show (sources, parsedSources')
importFullImportSource :: GroupId -> LBS.ByteString -> Either String [FullImportSource]
importFullImportSource groupId bytes = do
  case fromXlsxBytes @ParsedImportSource bytes of
    Left err -> Left $ "Failed to parse XLSX file, " <> err
    Right importedSourceSources ->
      if all checkAllowableEmpty importedSourceSources
        then
          Right $
            parsedToFullImportSource
              groupId
              `map` importedSourceSources
        else Left $ "Error: Empty Internet Identification is only allowable for User Provided Internet Provider"