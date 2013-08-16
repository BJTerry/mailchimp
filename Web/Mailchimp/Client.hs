module Web.Mailchimp.Client where

import Text.Parsec (parse, many1, hexDigit, char, alphaNum)
import Text.Parsec.Text (GenParser(..))
import Control.Exception.Base (Exception)
import Control.Exception.Lifted (throwIO, catch)
import Control.Monad (mzero, liftM)
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text(..), pack, concat, unpack)
import Data.Typeable (Typeable)
import Data.Aeson (FromJSON(..), (.:), Value(..), decode)
import Data.Aeson.Encode (encode)
import Data.Conduit (runResourceT)
import Network.HTTP.Conduit (parseUrl, requestBody, newManager, httpLbs, RequestBody(..), Response(..), HttpException(..))
import Network.HTTP.Types.Header (ResponseHeaders(..))
import Control.Monad.Base (MonadBase)

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
apiEndpointUrl datacenter section method = 
  Data.Text.concat ["https://", datacenter, ".api.mailchimp.com/2.0/", section, "/", method, ".json"]

query :: FromJSON x => MailchimpApiKey -> Text -> Text -> Value -> IO x
query apiKey section method request = runResourceT $ do
  initReq <- parseUrl $ unpack $ apiEndpointUrl (makDatacenter apiKey) section method
  let req = initReq { requestBody = RequestBodyLBS $ encode request }
  man <- liftIO $ newManager def
  response <- catch (httpLbs req man) catchHttpException
  case decode $ responseBody response of
    Just result -> return result
    Nothing -> throwIO $ OtherMailchimpError (-1) "ParseError" "Could not parse result JSON from Mailchimp"
 where
  catchHttpException :: MonadBase IO m => HttpException -> m a
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
        _ -> OtherMailchimpError 
  parseJSON _ = mzero
