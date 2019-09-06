{-# LANGUAGE OverloadedStrings #-}
module Domains.Serialization (options) where

import           Data.Aeson
import           Data.Char      (isUpper)
import           Data.Semigroup ((<>))
import           Data.Text      (Text)
import qualified Data.Text      as T

-- | Aeson instances generation options
options :: Int -> Options
options d =
  defaultOptions
    { fieldLabelModifier = T.unpack . T.drop (d + 1) . camelToSnakeCase . T.pack
    , unwrapUnaryRecords = True
    }

camelToSnakeCase :: Text -> Text
camelToSnakeCase = T.concatMap f
  where f c
          | isUpper c = "_" <> T.toLower (T.singleton c)
          | otherwise = T.singleton c
