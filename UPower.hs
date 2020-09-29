{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module UPower
  ( DisplayDeviceProperties (..),
    State(..),
    DeviceType(..),
    getDisplayDeviceProperties,
    connectSystem,
  )
where

import Control.Exception (throwIO)
import DBus
import DBus.Client (Client, call, call_, clientError, connectSystem, getAllProperties)
import Data.Int (Int64)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Word (Word32)

cRootObject = "/org/freedesktop/UPower"

cDisplayDevice = "/org/freedesktop/UPower/devices/DisplayDevice"

cDeviceInterface = "org.freedesktop.UPower.Device"

data Device = Device

withUpowerDest method = method {methodCallDestination = Just "org.freedesktop.UPower"}

data DisplayDeviceProperties = DisplayDeviceProperties
  { -- | Battery charge percentage
    prop_percentage :: Double,
    -- | Time till battery is fully charged (in seconds)
    prop_timeToFull :: Int64,
    -- | Time till battery is empty (in seconds)
    prop_timeToEmpty :: Int64,
    -- | Whether battery info should be displayed.
    prop_isPresent :: Bool,
    prop_state :: State,
    prop_type :: DeviceType
  }
  deriving (Show)

instance IsVariant DisplayDeviceProperties where
  toVariant = error "not implemented"
  fromVariant v = do
    map <- fromVariant @(Map Text Variant) v
    let getField key = fromVariant =<< map Map.!? key
    prop_percentage <- getField "Percentage"
    prop_timeToFull <- getField "TimeToFull"
    prop_timeToEmpty <- getField "TimeToEmpty"
    prop_isPresent <- getField "IsPresent"
    prop_state <- getField "State"
    prop_type <- getField "Type"
    return
      DisplayDeviceProperties
        { prop_percentage,
          prop_timeToFull,
          prop_timeToEmpty,
          prop_isPresent,
          prop_state,
          prop_type
        }

data State
  = StateUnknown
  | StateCharging
  | StateDischarging
  | StateEmpty
  | StateFullyCharged
  | StatePendingCharge
  | StatePendingDischarge
  deriving (Bounded, Enum, Show)

instance IsVariant State where
  toVariant = error "not implemented"
  fromVariant = enumFromVariant

data DeviceType
  = TypeUnknown
  | TypeLinePower
  | TypeBattery
  | TypeUps
  | TypeMonitor
  | TypeMouse
  | TypeKeyboard
  | TypePda
  | TypePhone
  deriving (Bounded, Enum, Show)

instance IsVariant DeviceType where
  toVariant = error "not implemented"
  fromVariant = enumFromVariant

-- | Throws ClientError if things go wrong
getDisplayDeviceProperties :: Client -> IO DisplayDeviceProperties
getDisplayDeviceProperties client = do
  result <- getAllProperties client (withUpowerDest (methodCall cDisplayDevice cDeviceInterface ""))
  case result of
    Left err -> throwIO $ clientError ("Call failed: " ++ methodErrorMessage err)
    Right result1 ->
      case methodReturnBody result1 of
        [var] -> case fromVariant var of
          Nothing -> throwIO $ clientError ("Unexpected response format")
          Just v -> return v
        _ -> throwIO $ clientError ("Unexpected response format")

enumFromVariant :: (Bounded a, Enum a) => Variant -> Maybe a
enumFromVariant variant =
  case fromIntegral <$> fromVariant @Word32 variant of
    Just val
      | val >= minBound && val <= maxBound -> Just (toEnum val)
      | otherwise -> Nothing
    Nothing -> Nothing