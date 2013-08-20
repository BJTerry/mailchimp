-- |
--
-- Module for interfacing with the Mailchimp JSON API.
--
-- Example usage to subscribe a user (in IO monad):
--
-- > let key = mailchimpKey "00000000000000000000000000000000-us1"
-- > case key of
-- >   Just k -> do
-- >     cfg <- defaultMailchimpConfig k
-- >     runMailchimp cfg $ do
-- >       subscribeUser <listId> <userEmail> [] EmailTypeHTML Nothing Nothing Nothing
-- 
-- If you are in a monad that implements MonadIO, MonadLogger and MonadBase IO (such as a Yesod Handler), you can alse use runMailchimpT in the same way.

module Web.Mailchimp
  (

  -- * The MailchimpT monad
    MailchimpT
  , runMailchimpT
  , runMailchimp
  , runMailchimpLogging
  , askApiKey
  -- * Running queries
  , query
  , query'
  -- * Configuration
  , MailchimpConfig(..)
  , defaultMailchimpConfig
  , MailchimpApiKey(..)
  , mailchimpKey
  -- * Error handling
  , MailchimpError(..)
  , 
  )
  where

import Web.Mailchimp.Util

import Text.Parsec (parse, many1, hexDigit, char, alphaNum)
import Text.Parsec.Text (GenParser)
import Control.Exception.Base (Exception)
import Control.Exception.Lifted (throwIO, catch)
import Control.Monad (mzero, MonadPlus, when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Default (def)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text, pack, concat, unpack)
import Data.Typeable (Typeable)
import Data.Aeson (FromJSON(..), (.:), Value(..), decode, (.=))
import Data.Aeson.Types (Pair)
import Data.Aeson.Encode (encode)
import Data.Conduit (runResourceT, ResourceT)
import Network.HTTP.Conduit (parseUrl, newManager, httpLbs, RequestBody(..), Response(..), HttpException(..), Request(..), Manager)
import Network.HTTP.Types (methodPost)
import Network.HTTP.Types.Header (ResponseHeaders)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask)
import Control.Monad.Logger (logDebug, LoggingT, runStderrLoggingT, MonadLogger, NoLoggingT(..))
import Control.Monad.Base (MonadBase)

-- | Represents the configuration for MailchimpT.
data MailchimpConfig = MailchimpConfig 
  { mcApiKey :: MailchimpApiKey
  , mcManager :: Manager
  }

-- | Convenience method to ask ReaderT for the current API key.
askApiKey :: MailchimpT m MailchimpApiKey
askApiKey = fmap mcApiKey ask

-- | Creates a MailchimpConfig with a new Manager.
defaultMailchimpConfig :: MonadIO m => MailchimpApiKey -> m MailchimpConfig
defaultMailchimpConfig apiKey = do
  man <- liftIO $ newManager def
  return MailchimpConfig { mcApiKey = apiKey
                         , mcManager = man
                         }

type MailchimpT m a =  (MonadIO m, MonadLogger m, MonadBase IO m) => ReaderT MailchimpConfig m a

runMailchimpT :: (MonadIO m, MonadLogger m, MonadBase IO m) => MailchimpConfig -> MailchimpT m a -> m a
runMailchimpT config action = 
  runReaderT action config

-- | Runs Mailchimp in IO, ignoring the existing monadic context and without logging.
runMailchimp :: (MonadIO m) => (MailchimpConfig -> MailchimpT (NoLoggingT IO) a -> m a)
runMailchimp config action =
  liftIO $ runNoLoggingT $ runReaderT action config

-- | Runs Mailchimp in IO, ignoring the existing monadic context and logging to stderr.
runMailchimpLogging :: (MonadIO m) => (MailchimpConfig -> MailchimpT (LoggingT IO) a -> m a)
runMailchimpLogging config action =
  liftIO $ runStderrLoggingT $ runReaderT action config

-- | Represents a mailchimp API key, which implicitly includes a datacenter.
data MailchimpApiKey = MailchimpApiKey
  { makApiKey :: Text -- Full API key including datacenter
  , makDatacenter :: Text -- 3-letter datacenter code
  }
  deriving (Show, Eq)

-- | Create a MailchimpApiKey from Text
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

-- | Builds the mailchimp endpoint URL.
apiEndpointUrl :: Text -> Text -> Text -> Text
apiEndpointUrl datacenter section apiMethod = 
  Data.Text.concat ["https://", datacenter, ".api.mailchimp.com/2.0/", section, "/", apiMethod, ".json"]

-- | Perform a query to Mailchimp. Should only be necessary to call directly for 
--   unimplemented methods. If you infer a return type of Value you will get the
--   raw JSON object or array. Automatically adds apikey to the request.
query :: (FromJSON x) => Text
      -- ^ Mailchimp API section
      -> Text
      -- ^ Mailchimp API method
      -> [Pair]
      -- ^ Request info
      -> MailchimpT m x
query section apiMethod request = do
  apikey <- fmap makApiKey askApiKey
  query' section apiMethod $ filterObject ("apikey" .= apikey : request)

-- | Perform a query to Mailchimp. Should only be necessary to call directly for 
--   unimplemented methods.
query' :: (FromJSON x) => Text -> Text -> Value -> MailchimpT m x
query' section apiMethod request = do
  config <- ask
  initReq <- liftIO $ parseUrl $ unpack $ apiEndpointUrl (makDatacenter $ mcApiKey config) section apiMethod
  let req = initReq { requestBody = RequestBodyLBS $ encode request 
                    , method = methodPost
                    }
  $(logDebug) $ pack . show $ requestBody req
  response <- liftIO $ runResourceT $ catch (httpLbs req $ mcManager config) catchHttpException
  $(logDebug) $ pack . show $ responseBody response
  case decode $ responseBody response of
    Just result -> return result
    Nothing -> throwIO $ OtherMailchimpError (-1) "ParseError" "Could not parse result JSON from Mailchimp"
 where
  catchHttpException :: HttpException -> ResourceT IO a
  catchHttpException e@(StatusCodeException _ headers _) = 
    maybe (throwIO e) throwIO (decodeError headers)
  catchHttpException e = throwIO e
  decodeError :: ResponseHeaders -> Maybe MailchimpError
  decodeError headers = fromStrict `fmap` lookup "X-Response-Body-Start" headers >>= decode

data MailchimpError = InvalidApiKey Int Text Text
                    | UserDisabled Int Text Text
                    | UserInvalidRole Int Text Text
                    | TooManyConnections Int Text Text
                    | UserUnderMaintenance Int Text Text
                    | UserInvalidAction Int Text Text
                    | ValidationError Int Text Text
                    | ListDoesNotExist Int Text Text
                    | ListAlreadySubscribed Int Text Text
                    | ListInvalidMergeField Int Text Text
                    | ListNotSubscribed Int Text Text
                    | ListInvalidInterestGroup Int Text Text
                    | InvalidDateTime Int Text Text
                    | InvalidPagingLimit Int Text Text
                    | InvalidPagingStart Int Text Text
                    | InvalidOptions Int Text Text
                    | InvalidSendType Int Text Text                    
                    | InvalidEmail Int Text Text
                    | InvalidURL Int Text Text
                    | CampaignDoesNotExist Int Text Text
                    | EmailNotExists Int Text Text

                    | OtherMailchimpError Int Text Text
  deriving (Typeable, Show, Eq)

instance Exception MailchimpError

instance FromJSON MailchimpError where
  parseJSON (Object v) = do
    status <- v .: "status"
    when (status /= ("error" :: Text)) mzero
    name <- v .: "name"
    code <- v .: "code"
    message <- v .: "error" -- Documentation shows this is under the "message" key, but it is incorrect
    return $ errConstructor name code name message
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
        "List_InvalidMergeField" -> ListInvalidMergeField        
        "List_NotSubscribed" -> ListNotSubscribed
        "List_InvalidInterestGroup" -> ListInvalidInterestGroup
        "Invalid_DateTime" -> InvalidDateTime
        "Invalid_PagingLimit" -> InvalidPagingLimit
        "Invalid_PagingStart" -> InvalidPagingStart
        "Invalid_Options" -> InvalidOptions
        "Invalid_SendType" -> InvalidSendType
        "Invalid_Email" -> InvalidEmail
        "Invalid_URL" -> InvalidURL
        "Campaign_DoesNotExist" -> CampaignDoesNotExist
        "Email_NotExists" -> EmailNotExists
        _ -> OtherMailchimpError 
  parseJSON _ = mzero



