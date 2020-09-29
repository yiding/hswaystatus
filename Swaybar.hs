{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Swaybar where

import Data.Aeson (fieldLabelModifier, omitNothingFields, ToJSON (..), defaultOptions, genericToEncoding)
import Data.Default (Default (..))
import Data.Text (Text)
import Data.Word (Word8)
import GHC.Generics (Generic)
import System.Posix (sigCONT, sigSTOP)

data Header = Header
  { version :: Int,
    click_events :: Bool,
    cont_signal :: Int,
    stop_signal :: Int
  }
  deriving (Generic, Show)

instance ToJSON Header where
  toEncoding = genericToEncoding defaultOptions

instance Default Header where
  def =
    Header
      { version = 1,
        click_events = False,
        cont_signal = fromIntegral sigCONT,
        stop_signal = fromIntegral sigSTOP
      }

data BodyBlock = BodyBlock
  { full_text :: Text,
    short_text :: Maybe Text,
    color :: Maybe HexColor,
    background :: Maybe HexColor,
    border :: Maybe HexColor,
    border_top :: Maybe Int,
    border_bottom :: Maybe Int,
    border_left :: Maybe Int,
    border_right :: Maybe Int,
    min_width :: Maybe (Either Int Text),
    align :: Maybe Align,
    name :: Maybe Text,
    instance_ :: Maybe Text,
    urgent :: Maybe Bool,
    separator :: Maybe Bool,
    separator_block_width :: Maybe Integer,
    markup :: Maybe Markup
  }
  deriving (Generic, Show)

bodyBlockJsonOptions = defaultOptions {
  fieldLabelModifier = \s -> case s of
    "instance_" -> "instance"
    _ -> s,
  omitNothingFields = True
}

instance ToJSON BodyBlock where
  toEncoding = genericToEncoding bodyBlockJsonOptions

instance Default BodyBlock where
  def =
    BodyBlock
      { full_text = "",
        short_text = Nothing,
        color = Nothing,
        background = Nothing,
        border = Nothing,
        border_top = Nothing,
        border_bottom = Nothing,
        border_left = Nothing,
        border_right = Nothing,
        min_width = Nothing,
        align = Nothing,
        name = Nothing,
        instance_ = Nothing,
        urgent = Nothing,
        separator = Nothing,
        separator_block_width = Nothing,
        markup = Nothing
      }

data Align = AlignLeft | AlignRight | AlignCenter
  deriving (Eq, Show)

alignToText :: Align -> Text
alignToText a =
  case a of
    AlignLeft -> "left"
    AlignCenter -> "center"
    AlignRight -> "right"


instance ToJSON Align where
  toJSON = toJSON . alignToText
  toEncoding = toEncoding . alignToText

data Markup = MarkupPango | MarkupNone
  deriving (Eq, Show)

markupToText :: Markup -> Text
markupToText a = case a of
  MarkupPango -> "pango"
  MarkupNone -> "none"

instance ToJSON Markup where
  toJSON = toJSON . markupToText
  toEncoding = toEncoding . markupToText

newtype HexColor = HexColor Text
  deriving (ToJSON, Eq, Show)