{-# LANGUAGE PatternSynonyms, DataKinds, TypeOperators, DeriveGeneric #-}

module MoviesClient where

import Data.Aeson
import GHC.Generics ( Generic )
import System.Console.Haskeline ( getInputLine, InputT )
import Control.Monad.Trans.State.Lazy (StateT)
import Control.Monad.Trans (liftIO, MonadTrans (lift))
import Data.Maybe (fromMaybe)
import Data.Functor ((<&>))
import Text.Read (readMaybe)
import Control.Monad.State.Lazy (MonadState(..))
import Network.HTTP.Client
import Servant.Client
import Servant
import Network.HTTP.Simple
import Data.Foldable (traverse_)


type Token = String

type Address = String

type Port = Int

type ServerL = (Token, Address, Port)



-- StateT for storage



type MoviesClient m a = StateT ServerL m a


data Movie =

    Movie
    { nameOfMovie :: String
    , rating :: Int}

    deriving (Show, Generic)

instance ToJSON Movie
instance FromJSON Movie

data MovieToken =

    MovieToken
    { token :: String
    , movies :: Movie }

    deriving Generic

instance ToJSON MovieToken
instance FromJSON MovieToken


data RegisterMessage =

    RM 
    { user :: String
    , passOrToken :: String}

    deriving Generic

instance ToJSON RegisterMessage
instance FromJSON RegisterMessage



-- Parser types for the command-line interface

data Command =

      SetAddress Address Port
    | Register String String
    | AddMovie Token String Int
    | FindMovies Token String
    | ShowStores
    | Quit

pattern FailedTest :: Maybe a
pattern FailedTest = Nothing



-- Servant interface for MoviesServer API

type API = "register" :> ReqBody '[JSON] RegisterMessage :> Post '[JSON] Token
    :<|> "movie" :> ReqBody '[JSON] MovieToken :> Post '[JSON] ()
    :<|> "movies" :> QueryParam "token" Token :> QueryParam "query" String :> Get '[JSON] [Movie]

register2 :: RegisterMessage -> ClientM Token

movie :: MovieToken -> ClientM ()

movies2 :: Maybe Token -> Maybe String -> ClientM [Movie]

register2 :<|> movie :<|> movies2 = client (Servant.Proxy :: Servant.Proxy API)



-- Generic utility functions



-- Network accessors


makePostRequest :: ToJSON a => String -> Int -> a -> Request
makePostRequest ipGiven port json =
    setRequestBodyJSON json $ setRequestPort port $ parseRequest_ ("POST " <>  ipGiven)

moviesGetRequest :: String -> Int -> String -> String -> Request
moviesGetRequest ipGiven port token query = 
    setRequestPort port $ parseRequest_ ( "GET " <> ipGiven <> "?token=" <> token <> "&query=\"" <> query <> "\"")



-- | mainLoop is the hook for the program, called by the main module. It runs
-- | a console which allows pinging the server.

mainLoop :: MoviesClient (InputT IO) ()
mainLoop = ( lift $ getInputLine "MoviesClient > " ) >>= executeInput . parseInput

    where

        executeInput :: Maybe Command -> MoviesClient (InputT IO) ()
        executeInput FailedTest = pure (putStrLn "\n Invalid or empty entry.\n") >> mainLoop
        executeInput (Just input) =
            
            case input of

                SetAddress address port -> do
                    ( token , _ , _ ) <- get
                    put ( token , address , port )
                    mainLoop
                
                Register username password -> do

                    ( _ , address , port ) <- get
                    let request = makePostRequest address port $ RM username password
                    response <- httpJSON request
                    let newToken = getResponseBody response

                    lift . lift $ putStrLn $ "The access token from the server is \"" <> newToken <> "\"."
                    put (newToken, address, port)

                AddMovie token movieName rating -> do

                    ( token , address , port ) <- get
                    let request = makePostRequest address port $ MovieToken token $ Movie movieName rating
                    
                    nothing <- httpJSON request

                    pure $ getResponseBody nothing
                    
                FindMovies token movieName -> do

                    ( token , address , port ) <- get
                    let request = moviesGetRequest address port token movieName

                    contents <- getResponseBody <$> httpJSON request

                    lift . lift $ traverse_ print (contents ::  [Movie])

                    pure ()

                ShowStores -> do
                    ( token , address , port ) <- get
                    let storesMessage = "Your token is \"" <> token <> "\". Your destination address \
                     \ is " <> address <> ":" <> show port <> "."
                    lift . lift $ putStrLn storesMessage

                Quit -> pure ()

        parseInput :: Maybe String -> Maybe Command
        parseInput providedLine =
    
            case processArgs . rmSpace . break ( == ' ' ) <$> providedLine of

                -- unfortunate clunkwork with sanitizers

                Just ( "SetAddress" , ( address , ( port , [] ) ) ) -> setAddressSanitizer address port
                Just ( "Register" , ( username , ( password , [] ) ) ) -> pure $ Register username password
                Just ( "AddMovie" , ( token , ( string , int ) ) ) -> addMoviesSanitizer token string int
                Just ( "FindMovies" , ( token , ( movieName , [] ) ) ) -> pure $ FindMovies token movieName
                Just ( "ShowStores" , ( [] , ( [] , [] ) ) ) -> pure ShowStores
                Just ( "Quit" , ( [] , ( [] , [] ) ) ) -> pure Quit
                _ -> FailedTest
    
                where

                    setAddressSanitizer address port = case ( checkAddress address, checkPort port ) of

                        (Just address, Just port) -> pure $ SetAddress address port
                        _ -> FailedTest

                    checkAddress address = case format address of
                        ( Just segment1 , ( Just segment2 , ( Just segment3 , Just segment4 ) ) ) |
                            testRange segment1 _8BitRange && testRange segment2 _8BitRange &&
                            testRange segment3 _8BitRange && testRange segment4 _8BitRange ->
                                pure $ show segment1 <> "." <> show segment2 <> "." <> show segment3 <>
                                    "." <> show segment4
                        _ -> FailedTest
                            
                    _8BitRange = ( 0 , 255 )

                    testRange element ( lowerBound , upperBound ) = lowerBound <= element && element <= upperBound

                    format address = castQuadToInt . preFormat $ address

                        where

                            castQuadToInt ( Just segment1 , ( Just segment2 , ( Just segment3 , Just segment4 ) ) ) =
                                (readMaybe segment1 , ( readMaybe segment2 , ( readMaybe segment3 , readMaybe segment4 ) ) )
                            castQuadToInt _ = ( FailedTest , ( FailedTest , ( FailedTest , FailedTest ) ) )

                            preFormat = ( fmap . fmap . fmap ) pure . ( fmap . fmap ) breakAndUnDot .
                                fmap breakAndUnDot . breakAndUnDot 

                            breakAndUnDot = rmDot . break ( == '.' )

                            rmDot ( x , '.' : ys ) = ( Just x , ys )
                            rmDot ( _ , ys ) = ( FailedTest , ys )

                    checkPort port = case readMaybe port :: Maybe Int of
                        Just int | testRange int _8BitRange -> Just int
                        _ -> FailedTest

                    addMoviesSanitizer token string int = case readMaybe int of
                        Just int | testRange int ( 0 , 10 ) -> pure $ AddMovie token string int
                        _ -> FailedTest

                    processArgs ( r , input ) = ( r , (\(a,b) -> (a, rmSpace $ break ( == ' ') b) ) $
                        rmSpace $ break ( == ' ') input )

                    rmSpace ( x , [] ) = ( x , [] )
                    rmSpace ( x , y : ys ) = ( x , ys )





-- | Get the server token by registering/logging in with the given username and password
register :: String -> String -> Token
register = undefined

-- | Add a movie to your user list in the server, given the user token, the name
-- of the movie and the rating
addMovie :: Token -> String -> Int -> IO ()
addMovie = undefined

-- | Search for movies given a user token and a query string
findMovies :: Token -> String -> IO [Movie]
findMovies = undefined