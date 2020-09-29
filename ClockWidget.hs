{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module ClockWidget (clockWidget) where

import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Data.Default (Default (def))
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Time (NominalDiffTime, defaultTimeLocale, formatTime, utcToLocalZonedTime)
import Reflex
import Swaybar 
import System.IO.Unsafe (unsafePerformIO)

clockWidget ::
  ( PostBuild t m,
    PerformEvent t m,
    TriggerEvent t m,
    MonadIO (Performable m),
    MonadHold t m,
    MonadFix m
  ) =>
  NominalDiffTime ->
  m (Dynamic t BodyBlock)
clockWidget updateRate = do
  currentTimeE <- tickLossyFromPostBuildTime updateRate
  let renderedTimeE = (unsafePerformIO . tickInfoToRenderedTime) <$> currentTimeE
  renderedTimeD <- holdUniqDyn =<< holdDyn "" renderedTimeE
  return $ blockWithText <$> renderedTimeD
  where
    blockWithText text = def {full_text = text}

tickInfoToRenderedTime :: TickInfo -> IO Text
tickInfoToRenderedTime tickInfo = do
  let utcTime = _tickInfo_lastUTC tickInfo
  localZonedTime <- utcToLocalZonedTime utcTime
  return $ fromString $ formatTime defaultTimeLocale "%F %R" localZonedTime
