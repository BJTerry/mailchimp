This is a tutorial for building a package to interface with RESTful JSON APIs.

1. Create your package.

Create a folder for your package. Initialize with cabal init.

    mkdir mailchimp
    cabal init
    # Select default package name, package version, choose your license, etc.

You will also want to add the OverloadedStrings extension to your cabal file.

2. Creating types.

The first step to supporting the Mailchimp API is parsing the API so that we can build URLs to the API. In Mailchimp's API, unlike many others, the API key includes the datacenter within it, which must be extracted. We use the following type to represent an API key, which we will put in the Web.Mailchimp.Client library:

    -- | Represents a mailchimp api key, which implicitly includes a datacenter.
    data MailchimpApiKey = MailchimpApiKey
      { makApiKey :: Text -- Full API key including datacenter
      , makDatacenter :: Text -- 2-letter datacenter code
      }

As a note, we will be using Data.Text everywhere in this tutorial, which is preferred over String because it is more performant. 

While we could require users of the module to pass properly constructed MailchimpApiKeys to the library, we will probably want to make it easy to simply parse the API keys as provided by Mailchimp. Luckily, Haskell Platform (which you should really be using) comes with a parser (Parsec) that makes this extremely simple. One could also use regular expressions, which are likewise provided by the Haskell Platform. A full discussion of how to use either of these tools are outside of the scope of this tutorial, but you can find examples of each in [Real World Haskell](http://book.realworldhaskell.org/).

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

We also need a function to build the API endpoint URL. URLs in Mailchimp's JSON API have the form https://**dc**.api.mailchimp.com/2.0/**section**/**method**.json:

    -- | Builds the mailchimp endpoint URL
    apiEndpointUrl :: Text -> Text -> Text -> Text
    apiEndpointUrl datacenter section method = 
      Data.Text.concat ["https://", datacenter, ".api.mailchimp.com/2.0/", section, "/", method, ".json"]

For the purposes of this tutorial, we will start by supporting the List Related Methods of the API, which are the ones most likely to be used while creating a web application. Specifically, we will implement lists/subscribe, the action used to subscribe a user to a particular list. It may be helpful to look at the Mailchimp [documentation](http://apidocs.mailchimp.com/api/2.0/lists/subscribe.php) for this action. Create the file Web/Mailchimp/Lists.hs which will hold the following types:

    newtype ListId = ListId Text

    data EmailId = Email Text
                 | EmailUniqueId Text
                 | ListEmailId Text

    type MergeVarsItem = Pair


    data EmailType = EmailTypeHTML
                   | EmailTypeText
   
E-mail addresses in the Mailchimp API can always be sent in one of three forms. As an e-mail address, as an ID associated with the e-mail address, or as the ID of a particular subscriber within a list. MergeVarsItems represent data associated with the user, such as FNAME (for first name) or LNAME (for last name) or one of a number of special purpose variables. Complex versions of this are infrequent enough that it probably isn't worth supporting a special type for each of them, so we use the Pair type from the Aeson JSON library to represent them.

Let's also create a type to represent API Errors in Web.Mailchimp.Client. For now we will just include the method-specific error for this particular method in addition to the API-wide errors.

    data MailchimpError = InvalidApiKey Int Text
                        | UserDisabled Int Text
                        | UserInvalidRole Int Text
                        | TooManyConnections Int Text
                        | UserUnderMaintenance Int Text
                        | UserInvalidAction Int Text
                        | ValidationError Int Text
                        | ListDoesNotExist Int Text
                        | OtherMailchimpError (Maybe Text)

    instance Error MailchimpError where
      noMsg = OtherMailchimpError Nothing
      strMsg s = OtherMailchimpError $ Just $ pack s

The Error instance is so that we can use MailchimpError with the ErrorT monad transformer, used later.

We are now in a position where we can write a simple function perform the lists/subscribe request. It's helpful to write a specific case before deciding how to move up to a higher level of abstraction.

subscribeUser :: MailchimpApiKey 
              -> ListId 
              -> EmailId 
              -> Maybe [MergeVarsItem] 
              -> EmailType
              -> Maybe Bool 
              -> Maybe Bool 
              -> Maybe Bool 
              -> Maybe Bool 
              -> IO (EmailId, EmailId, EmailId)
subscribeUser apiKey 
              listId 
              emailId 
              mergeVars 
              emailType 
              doubleOptin 
              updateExisting 
              replaceInterests 
              sendWelcome = runResourceT $ do
  initReq <- parseUrl $ unpack $ apiEndpointUrl (makDatacenter apiKey) "lists" "subscribe"
  let requestJson = object [ "apikey" .= makApiKey apiKey
                           , "id" .= unListId listId
                           , "email" .= emailId
                           , "merge_vars" .= fmap object mergeVars
                           , "email_type" .= emailType
                           , "double_optin" .= doubleOptin
                           , "update_existing" .= updateExisting
                           , "replace_interests" .= replaceInterests
                           , "send_welcome" .= sendWelcome
                           ]
  let req = initReq { requestBody = RequestBodyLBS $ encode requestJson }
  man <- liftIO $ newManager def 
  response <- httpLbs req man
  let meResult = decode $ responseBody response
  case meResult of
    Just (Left mcError) -> throw mcError
    Just (Right emailIdTriple) -> return emailIdTriple
    Nothing -> throw $ OtherMailchimpError $ Just "Could not parse result JSON in call to lists/subscribe"

Now, this version has a couple of problems (and doesn't compile), but it's a good first try. Note that we are using a ResourceT monad (via runResourceT) to run everything, since it's required by the Network.HTTP.Conduit library that we are using to make our request. Then, we build the request that we will use by parsing the URL and adding our request JSON to the request body. This uses the Data.Aeson library to create and decode JSON using the object, encode and decode functions. HttpLbs is the function that actually performs the request, and we are going to expect back a result of Maybe Either MailchimpError (EmailId, EmailId, EmailId). If we couldn't parse the response we will throw an OtherMailchimpError.

The first problem with the code is that we currently don't know how to convert EmailId and EmailType to JSON, and we don't know how to convert EmailId from JSON. Also, we don't know how to convert JSON to MailchimpError. So we will write instances for these using the Aeson library. The instances look like this:

    instance ToJSON EmailType where
      toJSON EmailTypeHTML = "html"
      toJSON EmailTypeText = "text"


    data EmailReturn = EmailReturn { erEmail :: EmailId
                                   , erEmailUniqueId :: EmailId
                                   , erListEmailId :: EmailId
                                   }

    instance FromJSON EmailReturn where
      parseJSON (Object v) = do
        email <- v .: "email"
        euid <- v .: "euid"
        leid <- v .: "leid"
        return $ EmailReturn (Email email) (EmailUniqueId euid) (ListEmailId leid)
      parseJSON _ = mzero

    instance ToJSON EmailId where
      toJSON (Email t) = object ["email" .= t]
      toJSON (EmailUniqueId t) = object ["euid" .= t]
      toJSON (ListEmailId t) = object ["leid" .= t]

    instance FromJSON MailchimpError where
      parseJSON (Object v) = do
        status <- v .: "status"
        if status /= ("error" :: Text) then mzero else return ()
        name <- v .: "name"
        code <- v .: "code"
        message <- v .: "message"
        return $ (errConstructor name) code message
        where
          errConstructor name = case (name :: Text) of
            "Invalid_ApiKey" -> InvalidApiKey
            "User_Disabled" -> UserDisabled
            "User_InvalidRole" -> UserInvalidRole
            "Too_Many_Connections" -> TooManyConnections
            "User_UnderMaintenance" -> UserUnderMaintenance
            "User_InvalidAction" -> UserInvalidAction
            "ValidationError" -> ValidationError
      parseJSON _ = mzero

The EmailReturn 
