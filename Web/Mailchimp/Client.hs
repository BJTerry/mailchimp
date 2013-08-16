module Web.Mailchimp.Client where

import Text.Parsec (parse, many1, hexDigit, char, alphaNum)
import Text.Parsec.Text (GenParser)
import Control.Exception.Base (Exception)
import Control.Exception.Lifted (throwIO, catch)
import Control.Monad (mzero, MonadPlus)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Default (def)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text, pack, concat, unpack)
import Data.Typeable (Typeable)
import Data.Aeson (ToJSON(..), FromJSON(..), (.:), Value(..), decode, object)
import Data.Aeson.Encode (encode)
import Data.Conduit (runResourceT, ResourceT)
import Network.HTTP.Conduit (parseUrl, newManager, httpLbs, RequestBody(..), Response(..), HttpException(..), Request(..), Manager)
import Network.HTTP.Types (methodPost)
import Network.HTTP.Types.Header (ResponseHeaders)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTime)
import Control.Monad.Logger (logDebug, LoggingT, runStderrLoggingT)

data MailchimpConfig = MailchimpConfig 
  { mcApiKey :: MailchimpApiKey
  , mcManager :: Manager
  }

-- | Convenience method to ask ReaderT for the current API key
askApiKey :: Mailchimp MailchimpApiKey
askApiKey = fmap mcApiKey ask

-- | Creates a MailchimpConfig with a new Manager
defaultMailchimpConfig :: MonadIO m => MailchimpApiKey -> m MailchimpConfig
defaultMailchimpConfig apiKey = do
  man <- liftIO $ newManager def
  return MailchimpConfig { mcApiKey = apiKey
                         , mcManager = man
                         }

type Mailchimp a = LogginT (ReaderT MailchimpConfig (ResourceT IO)) a

runMailchimp :: (MonadIO m) => MailchimpConfig -> Mailchimp a -> m a
runMailchimp config action = 
  liftIO $ runResourceT $ flip runReaderT config $ runStderrLoggingT action


-- | Represents a mailchimp api key, which implicitly includes a datacenter.
data MailchimpApiKey = MailchimpApiKey
  { makApiKey :: Text -- Full API key including datacenter
  , makDatacenter :: Text -- 2-letter datacenter code
  }
  deriving (Show)

-- | Create a mailchimp key from Text
mailchimpKey :: Text -> Maybe MailchimpApiKey
mailchimpKey apiKey = 
  case parse parseKey "(unknown)" apiKey of
    Left _ -> Nothing
    Right (_, dcString) ->
      Just MailchimpApiKey 
        { makApiKey = apiKey 
        , makDatacenter = pack dcString
        }

parseKey :: GenParser st (String, String)
parseKey = do
  key <- many1 hexDigit
  _ <- char '-'
  dc <- many1 alphaNum
  return (key, dc)

-- | Builds the mailchimp endpoint URL
apiEndpointUrl :: Text -> Text -> Text -> Text
apiEndpointUrl datacenter section apiMethod = 
  Data.Text.concat ["https://", datacenter, ".api.mailchimp.com/2.0/", section, "/", apiMethod, ".json"]

query :: (FromJSON x) => Text -> Text -> Value -> Mailchimp x
query section apiMethod request = do
  config <- ask
  initReq <- liftIO $ parseUrl $ unpack $ apiEndpointUrl (makDatacenter $ mcApiKey config) section apiMethod
  let req = initReq { requestBody = RequestBodyLBS $ encode request 
                    , method = methodPost
                    }
  response <- catch (httpLbs req $ mcManager config) catchHttpException
  case decode $ responseBody response of
    Just result -> return result
    Nothing -> throwIO $ OtherMailchimpError (-1) "ParseError" "Could not parse result JSON from Mailchimp"
 where
  catchHttpException :: HttpException -> Mailchimp a
  catchHttpException e@(StatusCodeException _ headers _) = do
    maybe (throwIO e) id (throwIO `fmap` (decodeError headers))
  catchHttpException e = throwIO e
  decodeError :: ResponseHeaders -> Maybe MailchimpError
  decodeError headers = fromStrict `fmap` (lookup "X-Response-Body-Start" headers) >>= decode

data MailchimpError = InvalidApiKey Int Text Text
                    | UserDisabled Int Text Text
                    | UserInvalidRole Int Text Text
                    | TooManyConnections Int Text Text
                    | UserUnderMaintenance Int Text Text
                    | UserInvalidAction Int Text Text
                    | ValidationError Int Text Text
                    | ListDoesNotExist Int Text Text
                    | ListAlreadySubscribed Int Text Text -- Undocumented Error
                    | InvalidDateTime Int Text Text
                    | InvalidPagingLimit Int Text Text
                    | InvalidPagingStart Int Text Text
                    | OtherMailchimpError Int Text Text
  deriving (Typeable, Show)

instance Exception MailchimpError

instance FromJSON MailchimpError where
  parseJSON (Object v) = do
    status <- v .: "status"
    if status /= ("error" :: Text) then mzero else return ()
    name <- v .: "name"
    code <- v .: "code"
    message <- v .: "error" -- Documentation shows this is under the "message" key, but it is incorrect
    return $ (errConstructor name) code name message
   where
      errConstructor name = case (name :: Text) of
        "Invalid_ApiKey" -> InvalidApiKey 
        "User_Disabled" -> UserDisabled 
        "User_InvalidRole" -> UserInvalidRole 
        "Too_Many_Connections" -> TooManyConnections 
        "User_UnderMaintenance" -> UserUnderMaintenance 
        "User_InvalidAction" -> UserInvalidAction 
        "ValidationError" -> ValidationError 
        "List_DoesNotExist" -> ListDoesNotExist 
        "List_AlreadySubscribed" -> ListAlreadySubscribed
        "Invalid_DateTime" -> InvalidDateTime
        "Invalid_PagingLimit" -> InvalidPagingLimit
        "Invalid_PagingStart" -> InvalidPagingStart
        _ -> OtherMailchimpError 
  parseJSON _ = mzero


filterObject list =
  object $ filter notNothing list
 where
  notNothing (_, Null) = False
  notNothing _ = True

newtype MCTime = MCTime {unMCTime :: UTCTime}

mcFormatString :: String
mcFormatString = "%F %T"

instance ToJSON MCTime where
  toJSON (MCTime t) = String $ pack $ formatTime defaultTimeLocale mcFormatString t
instance FromJSON MCTime where
  parseJSON (String s) = maybe mzero (return . MCTime) $ parseTime defaultTimeLocale mcFormatString (unpack s)
  parseJSON _ = mzero
  

test = do
  cfg <- defaultMailchimpConfig (MailchimpApiKey "123" "123")
  runMailchimp cfg $ do
    $(logDebug) "This is a test"
