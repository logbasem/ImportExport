--This file contains pieces of code written by me, but it is part of a project with a larger group of programmers.

--------------------
-- Export Handler --
--------------------
-- | The String parameter is a very important piece to this Handler
--   When this route gets called, the string becomes the name of the returned file
--   This is how you specify the return file as *.xlsx
-- | Exports all existing import sources to an xlsx file
getExportFullimportSourceR :: String -> Handler TypedContent
getExportFullimportSourceR _ = do
  userIdentVal <- getUserIdent
  aid <- requireAuthId

  -- Get the user's 'owned' groups
  sul <- getSuperUserList
  let ownedGroupIds = entityKey <$> getOwnedGroups sul
  memberGroupIds <- runHaxlInHandler userIdentVal $ getGroupIdListHaxl aid
  let permissionGroupIds = nub $ ownedGroupIds ++ memberGroupIds

  -- Make a request to the import-source-server through Haxl
  eResp <- tryRunHaxlInHandler userIdentVal $ importSource.listimportSourcesByGroupIdsHaxl permissionGroupIds

  -- Handle result
  case eResp of
    Left importSourceError -> sendResponseStatus status500 (toJSON @importSourceError importSourceError)
    Right sources -> do
      eitherBytes <- liftIO $ exportFullimportSource sources
      case eitherBytes of
        Left err -> sendResponseStatus status500 $ Text.pack err
        Right bytes -> return $ TypedContent "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" $ toContent (LByteS.toStrict bytes)

--------------------
-- Import Handler --
--------------------
-- | Imports import sources from an xlsx file
postImportFullimportSourceR :: Handler ()
postImportFullimportSourceR = do
  _ <- requireAuthId
  -- import Source Group Id
  gidRequired <- Foundation.importSourceGroupId . appConfig <$> getYesod
  -- Expected a form with (FileInfo)
  ((mresult, _), _) <- runFormPostNoToken form
  case mresult of
    FormMissing -> sendResponseStatus status400 $ toJSON ("Failed to get file from form submission, FormMissing" :: Text.Text)
    FormFailure _ -> sendResponseStatus status400 $ toJSON ("Failed to get file from form submission, FormFailure" :: Text.Text)
    FormSuccess fileInfo -> do
      -- Get the bytes from the form submission
      fileBytes <- runResourceT $ fileSource fileInfo `connect` DCB.sinkLbs
      -- Parse the bytes into a list of 'FullimportSource's
      case importFullimportSource gidRequired fileBytes of
        Left err -> sendResponseStatus status400 $ toJSON $ Text.pack err
        Right importedSources -> do
          liftIO $ print importedSources
          -- Handle the creation and updating of the imported sources
          updateMultipleimportSources importedSources
  where
    form :: Foundation.Form (FileInfo)
    form =
      renderDivs $ fileAFormReq "File"