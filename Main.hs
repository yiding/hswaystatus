{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import BatteryWidget
import ClockWidget
import Control.Monad.IO.Class (MonadIO (liftIO))
import DBus.Client (connectSystem)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Default (def)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Time (defaultTimeLocale, utcToLocalZonedTime)
import Data.Time.Format (formatTime)
import Reflex
import Reflex.Host.Headless (runHeadlessApp)
import Swaybar
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)

type SwaybarWidget = Dynamic BodyBlock

writeStatusLine :: [BodyBlock] -> IO ()
writeStatusLine blocks = do
  LBS.putStr (Aeson.encode blocks)
  LBS.putStr ",\n"
  hFlush stdout

statusLineUpdate blocks = do
  let combinedStatusLine = mconcat (map (fmap (: [])) blocks)
  performEvent_ (liftIO . writeStatusLine <$> updated combinedStatusLine)

main :: IO ()
main = do
  systemDbusClient <- connectSystem
  LBS.putStrLn $ Aeson.encode (def @Header)
  hFlush stdout
  LBS.putStrLn "["
  runHeadlessApp $ do
    timeWidgetD <- clockWidget 1
    batteryWidgetD <- batteryWidget systemDbusClient 2
    statusLineUpdate
      [ (\b -> b {separator = Just True}) <$> batteryWidgetD,
        timeWidgetD
      ]
    return never
