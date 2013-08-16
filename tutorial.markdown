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

For the purposes of this tutorial, we will start by supporting the List Related Methods of the API, which are the ones most likely to be used while creating a web application. Specifically, we will implement lists/subscribe, the action used to subscribe a user to a particular list. It may be helpful to look at the Mailchimp [documentation](http://apidocs.mailchimp.com/api/2.0/lists/subscribe.php) for this action. (Although during the creation of this tutorial I found several errors in it. I imagine you will find the same with APIs you are accessing!) Create the file Web/Mailchimp/Lists.hs which will hold the following types:

    -- | Represents an individual mailing list
    newtype ListId = ListId Text
    
    -- | Represents one of the canonical ways of identifying subscribers
    data EmailId = Email Text
                 | EmailUniqueId Text
                 | ListEmailId Text

    -- | Represents an individual merge variable    
    type MergeVarsItem = Pair


    -- | The type of e-mail your user will receive
    data EmailType = EmailTypeHTML
                   | EmailTypeText
   
E-mail addresses in the Mailchimp API can always be sent in one of three forms. As an e-mail address, as an ID associated with the e-mail address, or as the ID of a particular subscriber within a list. MergeVarsItems represent data associated with the user, such as FNAME (for first name) or LNAME (for last name) or one of a number of special purpose variables. These are used to insert user data into e-mails that you are sending out. The special items within mergeVars of this are infrequent enough that it probably isn't worth supporting a special type for each of them, so we use the Pair type from the Aeson JSON library to represent them, which is just a pair of (Text, Value), where Value is a JSON object, string, etc.

We can now write a simple function to perform the lists/subscribe request. It's helpful to write a specific case before deciding how to move up to a higher level of abstraction.

    subscribeUser :: MailchimpApiKey 
                  -> ListId 
                  -> EmailId 
                  -> Maybe [MergeVarsItem] 
                  -> Maybe EmailType
                  -> Maybe Bool 
                  -> Maybe Bool 
                  -> Maybe Bool 
                  -> Maybe Bool 
                  -> IO EmailReturn
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
      let req = initReq { requestBody = RequestBodyLBS $ encode requestJson 
                        , method = methodPost
                        }
      man <- liftIO $ newManager def 
      response <- httpLbs req man 
      let meResult = decode $ responseBody response
      case meResult of
        Just emailIdTriple -> return emailIdTriple
        Nothing -> liftIO mzero

Now, this version has a couple of problems (and doesn't compile), but it's a good first try. We are going to use Network.HTTP.Conduit to perform all of our network requests, and that library requires that everything be run in a ResourceT monad, so the function starts with a runResourceT. Because we are running the ResourceT monad with IO, we have access to other IO requests with liftIO, should it be necessary. First, we build the request that we will use by parsing the URL and adding our request JSON to the request body. This uses the Data.Aeson library to create and decode JSON using the object, encode and decode functions. HttpLbs (from Network.HTTP.Conduit) is the function that actually performs the request, and we are going to expect back a result of Maybe (EmailId, EmailId, EmailId). If we couldn't parse the response we will throw an OtherMailchimpError.

The first problem with the code is that we currently don't know how to convert EmailId and EmailType to JSON, and we don't know how to convert EmailId from JSON. We will use the Aeson library to create JSON instances. The instances look like this:

    instance ToJSON EmailType where
      toJSON EmailTypeHTML = "html"
      toJSON EmailTypeText = "text"

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

The ToJSON instances simply take one of their respective values and create the appropriate JSON objects. FromJSON is a little more complex. The parseJSON function of FromJSON runs in the Parser monad. The .: operator accesses object values by key, allowing us to construct the final value. The parseJSON instance function can parse any type of JSON value, not only objects, but anything other than an object is clearly not a MailchimpError, which we represent with the mzero function.

The next issue with subscribeUser is its return value. Before, we had intended to return a triple of EmailIds, but this ends up being less than ideal. Suppose you are parsing the result of lists/subscribe, which looks like this:

    {"email": "example@example.com", "euid": "123abc", "leid": "123abc"}

If you want to parse the EmailIds at the level of the individual item, you wont have access to the key to tell you which of the EmailId alternatives to select. If you want to parse it as a triple (EmailId, EmailId, EmailId) as the original return of the function suggested, you find that this is an illegal instance declaration (although you can use the FlexibleInstances language pragma to override it). A better design is to create the EmailReturn record, which will hold the three result types. This also makes it easier for library users, who won't have to remember which entry in the tuple is the result they want. 

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

Because e-mails are all returned in this form, it will be used many times while building the API. 

The next issue with subscribeUser is that it doesn't currently handle errors in a very graceful manner. When the Mailchimp API signals an error, it always returns an HTTP status other than 200, and using httpLbs, this will immediately cause an HttpException to be thrown. We would rather throw Exceptions specific to the Mailchimp API so that users of our library can handle them as appropriate for their application.

Let's create a type to represent API exceptions in Web.Mailchimp.Client. For now we will just include the method-specific error for this particular method in addition to the API-wide errors.

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

Exception is defined in Control.Exception.Base and will allow us to throw and catch MailchimpErrors. Next, let's revise subscribeUser to take include error handling code:

    subscribeUser :: MailchimpApiKey 
                  -> ListId 
                  -> EmailId 
                  -> Maybe [MergeVarsItem] 
                  -> Maybe EmailType
                  -> Maybe Bool 
                  -> Maybe Bool 
                  -> Maybe Bool 
                  -> Maybe Bool 
                  -> IO EmailReturn
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
      let req = initReq { requestBody = RequestBodyLBS $ encode requestJson 
                        , method = methodPost
                        }
      man <- liftIO $ newManager def 
      response <- catch (httpLbs req man) 
        (\e -> 
          case e :: HttpException of
            StatusCodeException _ headers _ -> do
              let (mResponse :: Maybe MailchimpError) = fromStrict `fmap` (lookup "X-Response-Body-Start" headers) >>= decode
              maybe (throwIO e) id (throwIO `fmap` mResponse)
            _ -> throwIO e)
      let mResult = decode $ responseBody response
      case mResult of
        Just emailResult -> return emailResult
        Nothing -> throwIO $ OtherMailchimpError (-1) "ParseError" "Could not parse result JSON from Mailchimp"

Because we are in the ResourceT IO monad, we use throwIO and catch from the Control.Exception.Lifted module in the lifted-base package. They work the same as their analogues in base, but are generalized to a wider variety of monadic contexts. When httpLbs gets a non-200 result, it throws a StatusCodeException, which we catch. Mailchimp's API sends the response body of errors in the X-Response-Body-Start header of the result, so we attempt to decode that and if possible, we throw the appropriate MailchimpError, otherwise we throw the original HttpException. We will also throw an OtherMailchimpError if we couldn't parse the result JSON.

We now have a working function, so lets try it in ghci:

    ~ ghci -fglasgow-exts
    > :set -XDeriveDataTypeable
    > :set -XOverloadedStrings
    > :l Web.Mailchimp.Lists Web.Mailchimp.Client
    > :m Web.Mailchimp.Lists Web.Mailchimp.Client
    > let key = MailchimpApiKey "<your key here>" "<dc code>"
    > subscribeUser key (ListId "<list id>") (Email "example@example.com") Nothing (Just EmailTypeHTML) Nothing Nothing Nothing Nothing
    EmailReturn {erEmail = Email "example@example.com", erEmailUniqueId = EmailUniqueId "<id>", erListEmailId = ListEmailId "<id>"}

The function worked. And if you run it again, you see that error handling also works:

    > subscribeUser key (ListId "<list id>") (Email "example@example.com") Nothing (Just EmailTypeHTML) Nothing Nothing Nothing Nothing
    *** Exception: ListAlreadySubscribed 214 "List_AlreadySubscribed" "example@example.com is already subscribed to list Users Newsletter. Click here to update your profile."

Wunderbar! Of course, we don't want to write such a long function for each of Mailchimp's 104 API methods, so let's extract some common functionality from subscribeUser that we'll use over and over. We will also take some time to make the function a little bit clearer:

    query :: FromJSON x => MailchimpApiKey -> Text -> Text -> Value -> IO x
    query apiKey section method request = runResourceT $ do
      initReq <- parseUrl $ unpack $ apiEndpointUrl (makDatacenter apiKey) section method
      let req = initReq { requestBody = RequestBodyLBS $ encode request 
                        , method = methodPost
                        }
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


    subscribeUser :: MailchimpApiKey 
                  -> ListId 
                  -> EmailId 
                  -> Maybe [MergeVarsItem] 
                  -> Maybe EmailType
                  -> Maybe Bool 
                  -> Maybe Bool 
                  -> Maybe Bool 
                  -> Maybe Bool 
                  -> IO EmailReturn
    subscribeUser apiKey 
                  listId 
                  emailId 
                  mergeVars 
                  emailType 
                  doubleOptin 
                  updateExisting 
                  replaceInterests 
                  sendWelcome = 
      query apiKey "lists" "subscribe" request
     where
      request = object [ "apikey" .= makApiKey apiKey
                       , "id" .= unListId listId
                       , "email" .= emailId
                       , "merge_vars" .= fmap object mergeVars
                       , "email_type" .= emailType
                       , "double_optin" .= doubleOptin
                       , "update_existing" .= updateExisting
                       , "replace_interests" .= replaceInterests
                       , "send_welcome" .= sendWelcome
                       ]

Note that because of our type annotations in catchHttpException, we have to use Flexibleinstances in this case.

There are still a couple improvements that could be made to the library. First, we are creating a new Manager each time query is called, which is very inefficient as it's an expensive operation (according to the documentation, at least). Second, if you are making multiple calls within the library, the current syntax could be slightly improved. Suppose that you want to delete all your folders created before a certain date, and you are in a non-IO monadic context, such as in a Yesod Handler. The code to perform that operation would look like this:

    liftIO $ do
      now <- getCurrentTime
      folders <- listFolders apiKey "campaign"
      mapM_ (\f -> deleteFolder apiKey (folderId f) (folderType f)) $ 
        filter (\folder -> dateCreated folder < (now - 60 * 60 * 24 * 7)) folders

For every action, we are passing in apiKey even though the key is obviously not changing. One can imagine even more complex actions where it would be more of a burden. To solve this problem and the problem of creating potentially hundreds of managers, let's wrap up our actions in a monad. But first, we'll create a MailchimpConfig type to carry the apiKey and the shared Manager:

    data MailchimpConfig = MailchimpConfig 
      { mcApiKey :: MailchimpApiKey
      , mcManager :: Manager
      }

    -- | Creates a MailchimpConfig with a new Manager
    defaultMailchimpConfig :: MonadIO m => MailchimpApiKey -> m MailchimpConfig
    defaultMailchimpConfig apiKey = do
      man <- liftIO $ newManager def
      return MailchimpConfig { mcApiKey = apiKey
                             , mcManager = man
                             }

    type Mailchimp a = ReaderT MailchimpConfig (ResourceT IO) a

    runMailchimp :: (MonadIO m) => MailchimpConfig -> Mailchimp a -> m a
    runMailchimp config action = 
      liftIO $ runResourceT $ flip runReaderT config $ action

MailchimpConfig is just a simple type to carry around the configuration, and a way to create a default configuration with a new Manager. We will run all of our actions in the Mailchimp Monad stack, which is IO, ResourceT (used by Network.HTTP.Conduit) and a ReaderT, which will allow us to query the MailchimpConfig without having to pass it around in every function.

The final versions of query and subscribeUser now look like this:

    query :: (FromJSON x) => Text -> Text -> Value -> Mailchimp x
    query section method request = do
      config <- ask
      initReq <- liftIO $ parseUrl $ unpack $ apiEndpointUrl (makDatacenter $ mcApiKey config) section method
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

    subscribeUser :: ListId 
                  -> EmailId 
                  -> Maybe [MergeVarsItem] 
                  -> Maybe EmailType
                  -> Maybe Bool 
                  -> Maybe Bool 
                  -> Maybe Bool 
                  -> Maybe Bool 
                  -> Mailchimp EmailReturn
    subscribeUser listId 
                  emailId 
                  mergeVars 
                  emailType 
                  doubleOptin 
                  updateExisting 
                  replaceInterests 
                  sendWelcome = do
      apiKey <- askApiKey
      query "lists" "subscribe" $ request apiKey
     where
      request apiKey = object [ "apikey" .= makApiKey apiKey
                              , "id" .= unListId listId
                              , "email" .= emailId
                              , "merge_vars" .= fmap object mergeVars
                              , "email_type" .= emailType
                              , "double_optin" .= doubleOptin
                              , "update_existing" .= updateExisting
                              , "replace_interests" .= replaceInterests
                              , "send_welcome" .= sendWelcome
                              ]
