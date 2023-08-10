{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Buzgibi.Job.Report (makeDailyReport, evalStateT, ReportCfg (..)) where

import Cfg.SendGrid (configure) 
import Buzgibi.Api.CallApi (Api, callApi, ApiCfg (..), methodGet)
import Buzgibi.Job.Utils (withElapsedTime)
import Buzgibi.Transport.Model.Telnyx (BalanceResponseWrapper (..), BalanceResponse (..))
import Buzgibi.EnvKeys (Telnyx (..), Sendgrid (..), Person (..), identity, persons, key, url)
import Buzgibi.Api.CallApi.Instance ()
import Buzgibi.Statement.User.Survey (getDailyPhoneStat, DailyPhoneStat (..))
import Control.Monad.State.Strict (StateT, get, modify', evalStateT)
import Data.Time.Clock (UTCTime (utctDay), getCurrentTime)
import qualified Hasql.Connection as Hasql
import Katip
import Data.Pool (Pool)
import BuildInfo (location)
import Control.Monad (when, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import qualified Network.HTTP.Client as HTTP
import Data.Coerce (coerce)
import Request (retry)
import Data.Either (isLeft)
import Data.Traversable (for)
import Database.Transaction (transaction, statement)
import Data.Aeson (eitherDecode, encode)
import Data.String.Conv (toS)
import Data.Bifunctor (first)
import Control.DeepSeq (force)
import Codec.Xlsx.Lens (cellValueAt, atSheet)
import Data.Default.Class (def)
import Codec.Xlsx.Types (Worksheet, CellValue(CellText), Xlsx)
import Control.Lens
import Data.Either.Combinators (whenLeft)
import qualified OpenAPI.Common as Sendgrid (runWithConfiguration)
import OpenAPI.Operations.POSTMailSend
  ( mkPOSTMailSendRequestBody,
    mkPOSTMailSendRequestBodyPersonalizationssendgrid,
    pOSTMailSend,
    pOSTMailSendRequestBodyPersonalizationssendgridSendAt,
    pOSTMailSendRequestBodyPersonalizationssendgridSubject,
    pOSTMailSendRequestBodyAttachments,
    mkPOSTMailSendRequestBodyAttachmentssendgrid,
    mkPOSTMailSendRequestBodyContentsendgrid
  )
import OpenAPI.Types.FromEmailObject (mkFromEmailObject, fromEmailObjectName)
import OpenAPI.Types.ToEmailArray (mkToEmailArrayItem)
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Codec.Xlsx.Writer (fromXlsx)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.ByteString.Base64 as B64
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Client (responseBody, responseStatus)
import Network.HTTP.Types.Status (accepted202, ok200)
import Data.Aeson.Encode.Pretty (encodePretty)

data ReportCfg = 
     ReportCfg 
     { logger :: Severity -> LogStr -> IO (), 
       pool :: Pool Hasql.Connection,
       telnyxCfg :: Telnyx,
       manager :: HTTP.Manager,
       sendgridCfg :: Sendgrid
     }

type instance Api "balance" () BalanceResponseWrapper = ()

makeDailyReport :: ReportCfg -> StateT UTCTime IO ()
makeDailyReport ReportCfg {..} = forever $ do
  liftIO $ threadDelay (3600 * 10 ^ 6)
  
  stateTM <- get
  currTM <- liftIO getCurrentTime
  when (utctDay stateTM == utctDay currTM) $ do 
    modify' $ const currTM
    liftIO $ withElapsedTime logger ($location <> "(makeDailyReport)") $ do
      balanceResp <- retry 30 (pure . isLeft) $ 
        callApi @"balance" @() @BalanceResponseWrapper 
          (ApiCfg telnyxCfg manager logger) 
            (Left Nothing) 
              methodGet 
                mempty 
                  Left 
                    (Right . coerce . fst)
    
      phoneStat <- fmap (first toS . eitherDecode @[DailyPhoneStat] . encode) $ transaction pool logger $ statement getDailyPhoneStat ()

      res <- for ((,) <$> balanceResp <*> phoneStat) $ 
        \(balance@BalanceResponse {..}, xs) -> do
          let makePhonesSheet _ sheet [] = sheet
              makePhonesSheet !idx sheet (DailyPhoneStat{..}:xs) =
                let newSheet =
                      force $
                       sheet 
                       & cellValueAt (idx, 1) ?~
                         CellText dailyPhoneStatPhone
                       & cellValueAt (idx, 2) ?~ 
                         CellText dailyPhoneStatTranscription
                       & cellValueAt (idx, 3) ?~ 
                         CellText dailyPhoneStatResult
                in makePhonesSheet (idx + 1) newSheet xs
          tm <- fmap (fromIntegral . systemSeconds) $ liftIO $ getSystemTime
          posix <- liftIO getPOSIXTime   
          let phoneSheet = makePhonesSheet 1 (def @Worksheet) xs
          let file = def @Xlsx & atSheet "phones" ?~ phoneSheet
          let content = decodeUtf8 $ B64.encode $ BL.toStrict $ fromXlsx posix file
          let body = 
                (mkPOSTMailSendRequestBody 
                [mkPOSTMailSendRequestBodyContentsendgrid "text/plain" (decodeUtf8 (BL.toStrict (encodePretty balance)))]
                ((mkFromEmailObject (sendgridCfg^.identity)) { fromEmailObjectName = Just "admin"})
                [ ( ( mkPOSTMailSendRequestBodyPersonalizationssendgrid
                    (map (mkToEmailArrayItem . coerce . personEmail) (sendgridCfg^.persons))
                    )
                    { pOSTMailSendRequestBodyPersonalizationssendgridSendAt = Just tm,
                      pOSTMailSendRequestBodyPersonalizationssendgridSubject = Just "daily report"
                    }
                  )
                ]
                "daily report")
                { pOSTMailSendRequestBodyAttachments = 
                    Just [mkPOSTMailSendRequestBodyAttachmentssendgrid content "report.xlsx" ] }
          resp <- Sendgrid.runWithConfiguration (configure (sendgridCfg^.url) (sendgridCfg^.key)) $ pOSTMailSend $ Just body
          let handleResp resp =
                if responseStatus resp == ok200
                    || responseStatus resp == accepted202
                then return ()
                else logger ErrorS $ logStr $ $location <> " sendgrid error ---> " <> show (responseBody resp)
          handleResp resp
      whenLeft res $ \error -> logger CriticalS $ logStr $ $location <> " ---> daily report fails, " <> error