module Web.Mailchimp.Util

  where

import Control.Monad (mzero)

import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), object)
import Data.Text (pack, unpack)
import Data.Char (isAlpha, toLower, isUpper)


import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Data.Time.Format (parseTime)

-- | Creates Aeson objects after filtering to remove null values.
filterObject list =
  object $ filter notNothing list
 where
  notNothing (_, Null) = False
  notNothing _ = True

-- | Represents times in the format expected by Mailchimp's API
newtype MCTime = MCTime {unMCTime :: UTCTime}
  deriving (Show, Eq)

mcFormatString :: String
mcFormatString = "%F %T"

instance ToJSON MCTime where
  toJSON (MCTime t) = String $ pack $ formatTime defaultTimeLocale mcFormatString t
instance FromJSON MCTime where
  parseJSON (String s) = maybe mzero (return . MCTime) $ parseTime defaultTimeLocale mcFormatString (unpack s)
  parseJSON _ = mzero

-- | Removes the first Int characters of string, then converts from CamelCase to underscore_case.
--   For use by Aeson template haskell calls.
convertName :: Int -> String -> String
convertName prefixLength pname = 
  (toLower (head name) : (camelToUnderscore $ tail name))
 where
  name = drop prefixLength pname
  -- | Converts camelcase identifiers to underscored identifiers
  camelToUnderscore (x : y : xs) | isAlpha x, isUpper y = 
    x : '_' : camelToUnderscore (toLower y : xs)
  camelToUnderscore (x : xs) = x : camelToUnderscore xs
  camelToUnderscore x = x
