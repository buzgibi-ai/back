{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}

module Buzgibi.Job.Survey (makeReport, SurveyCfg (..)) where

import Buzgibi.Statement.User.Survey (getSurveyForReport, saveReport, SurveyForReportItem (..), insertStat)
import Buzgibi.Statement.File (NewFile (..), save)
import Buzgibi.Transport.Id (Id (..))
import Katip
import Control.Lens ((&), (?~))
import Control.Concurrent (threadDelay)
import Buzgibi.Job.Utils (withElapsedTime)
import Control.Monad (forever, unless)
import qualified Hasql.Connection as Hasql
import Data.Pool (Pool)
import qualified Network.Minio as Minio
import Database.Transaction
import qualified Control.Concurrent.Async as Async
import Data.Aeson (eitherDecode, encode)
import BuildInfo (location)
import Data.Traversable (for)
import Data.Foldable (for_)
import Data.Either.Combinators (whenLeft)
import Codec.Xlsx.Types (Worksheet, CellValue(CellText), Xlsx)
import Control.DeepSeq (force)
import Codec.Xlsx.Lens (cellValueAt, atSheet)
import Data.Default.Class (def)
import Network.Minio
import Hash (mkHash)
import Codec.Xlsx.Writer (fromXlsx)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.String.Conv (toS)
import Control.Monad.IO.Class
import qualified Data.Text as T
import Network.Mime (defaultMimeLookup)
import Data.Coerce (coerce)
import Data.Conduit.Combinators (sourceLazy)
import Data.Bifunctor (second)

data SurveyCfg =
     SurveyCfg 
     { logger :: Severity -> LogStr -> IO (), 
       pool :: Pool Hasql.Connection,
       minio :: (Minio.MinioConn, T.Text),
       jobFrequency :: Int
     }

makeReport :: SurveyCfg -> IO ()
makeReport SurveyCfg {..} = forever $ do 
  threadDelay (jobFrequency * 10 ^ 6)
  withElapsedTime logger ($location <> "(makeReport)") $ do

    -- 1 fetch data from db: survey id, phones, SA result
    xs <- transaction pool logger $ statement getSurveyForReport ()
    -- 2 prepare a file
    surveyXs <- Async.forConcurrently xs $ \(survIdent, user, ys) -> do
      logger InfoS $ logStr $ $location <> " ---> report is about to be made for " <> show survIdent
      let xse = sequence $ map (eitherDecode @SurveyForReportItem . encode) ys
      logger DebugS $ logStr $ $location <> "  ---> report: users " <> show xse <> " for survey " <> show survIdent
      res <- for xse $ \xs -> do
        let xlsxFile = makeFile 1 (def @Worksheet) xs
        liftIO $ logger DebugS $ logStr $ $location <> "  ---> report: xslx file " <> show xlsxFile
        runMinioWith (fst minio) $ do
          file <- commitToMinio user (snd minio) xlsxFile
          liftIO $ logger DebugS $ logStr $ $location <> "  ---> report: minio file " <> show file

          ids <- liftIO $ transaction pool logger $ statement save [file]
          for_ ids $ \id -> liftIO $ transaction pool logger $ statement saveReport (survIdent, coerce id)
      whenLeft res $ \error -> 
        logger ErrorS $ logStr $ 
          $location <> " cannot fetch phones data for report " <> show survIdent <> ", error: " <> error
      return $ second (const survIdent) res
    for_ (sequence surveyXs) $ transaction pool logger . statement insertStat

makeFile _ sheet [] = def @Xlsx & atSheet "phones" ?~ sheet
makeFile !idx sheet (x:xs) =
  let newSheet =
        force $
          sheet 
          & cellValueAt (idx, 1) ?~ 
            CellText (surveyForReportItemPhone x)
          & cellValueAt (idx, 2) ?~ 
            CellText (surveyForReportItemResult x)
  in makeFile (idx + 1) newSheet xs

commitToMinio ident prefix xlsx = do
  tm <- liftIO getPOSIXTime
  let hash = mkHash $ show xlsx <> show tm
  let newBucket = prefix <> "." <>  "user" <> toS (show ident) <> "." <> "survey"
  exist <- bucketExists newBucket
  unless exist $ makeBucket newBucket Nothing
  let opts = 
        defaultPutObjectOptions
        { pooContentType = 
          Just (toS (defaultMimeLookup ".xlsx")) }
  putObject newBucket (toS hash) (sourceLazy (fromXlsx tm xlsx)) Nothing opts *> pure 
    NewFile { newFileHash = toS hash, newFileName = "report", newFileMime = toS (defaultMimeLookup ".xlsx"), newFileBucket = newBucket, newFileExts = ["xlsx"] }