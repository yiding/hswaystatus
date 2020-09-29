{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module BatteryWidget (batteryWidget) where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import DBus.Client (Client)
import Data.Default (Default (def))
import Data.Int (Int64)
import Data.Text.Format (Only (..), build, format, prec)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder)
import Data.Time (DiffTime, NominalDiffTime, defaultTimeLocale, formatTime, secondsToDiffTime)
import Reflex
import Swaybar
import UPower

batteryWidget ::
  ( PostBuild t m,
    PerformEvent t m,
    TriggerEvent t m,
    MonadIO (Performable m),
    MonadHold t m,
    MonadFix m
  ) =>
  Client ->
  NominalDiffTime ->
  m (Dynamic t BodyBlock)
batteryWidget systemDbusClient updateRate = do
  tickE <- tickLossyFromPostBuildTime updateRate
  propertiesE <- queryDisplayDeviceProperties systemDbusClient tickE
  batteryTextD <- holdUniqDyn =<< holdDyn "" (toStrict . renderWidgetText <$> propertiesE)
  return $ (\t -> def {full_text = t}) <$> batteryTextD

renderWidgetText
  DisplayDeviceProperties
    { prop_state,
      prop_percentage,
      prop_timeToFull,
      prop_timeToEmpty
    } =
    case prop_state of
      StateFullyCharged -> format "{}%" (Only roundPercent)
      StateCharging ->
        format
          "{}% ↑{}"
          (roundPercent, secondsToHM prop_timeToFull)
      StateDischarging ->
        format
          "{}% ↓{}"
          (roundPercent, secondsToHM prop_timeToEmpty)
      _ -> ""
  where
    roundPercent :: Int
    roundPercent = round prop_percentage

secondsToHM :: Int64 -> Builder
secondsToHM s = build "{}:{}" (s `div` 3600, s `mod` 3600 `div` 60)

queryDisplayDeviceProperties systemDbusClient trigger =
  performEventAsync (onTrigger <$> trigger)
  where
    onTrigger _ setResult = liftIO . void . forkIO $ do
      result <- getDisplayDeviceProperties systemDbusClient
      setResult result