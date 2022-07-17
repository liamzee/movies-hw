{-# LANGUAGE DataKinds, TypeOperators, TypeApplications, OverloadedStrings #-}
{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms #-}


module MoviesServer where

import Network.Wai.Handler.Warp
import Servant
import Data.Aeson hiding (Success)
import GHC.Generics
import Data.Int (Int8)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Crypto.Hash
import Data.ByteString as B hiding (zipWith, all, map, null, filter, concat, notElem, elem, head)
import Data.ByteString.Lazy as LB hiding (zipWith, all, map, null, filter, concat, head, notElem, elem)
import Data.Maybe (fromMaybe)
import Data.Functor ((<&>))
import Data.Function ((&))
import Control.Arrow ((&&&))
import Control.Applicative (Applicative(liftA2))

-- | needs fixing

type Token = String



type MoviesAPI = "register" :> ReqBody '[JSON] RegisterMessage :> Post '[JSON] Token

    :<|> "movie" :> ReqBody '[JSON] MovieToken :> Post '[JSON] ()
    
    :<|> "movies" :> QueryParam "token" Token :> QueryParam "query" String :> Get '[JSON] [Movie]



data JSONStore =
    
    JSONStore
    
    { usernames :: [RegisterMessage]
    
    , movieTokens :: [MovieToken]}
    
    deriving Generic

instance ToJSON JSONStore

instance FromJSON JSONStore



data RegisterMessage =
    
    RM 
    
    { user :: String
    
    , passOrToken :: String}
    
    deriving Generic

instance ToJSON RegisterMessage

instance FromJSON RegisterMessage



data Movie =
    
    Movie
    
    { nameOfMovie :: String
    
    , rating :: Int}
    
    deriving Generic

instance ToJSON Movie

instance FromJSON Movie



data MovieToken =
    
    MovieToken
    
    { token :: String
    
    , movies :: Movie}
    
    deriving Generic

instance ToJSON MovieToken

instance FromJSON MovieToken



-- Servant framework code


moviesApp :: Application

moviesApp = serve (Proxy @MoviesAPI) impl



impl :: Server MoviesAPI

impl = (accessError . liftIO . uncurry register . processRegister
    
    :<|> accessError . liftIO . processAddMovie addMovie
    
    :<|> (accessError . ) . (liftIO . ) . findMovies)
    
    where

    accessError = (=<<) $ \case

            Nothing -> throwError err403

            Just content -> pure content
    
    processRegister registerMessage = (user registerMessage, passOrToken registerMessage)
    
    processAddMovie internalFunction movieToken = internalFunction (token movieToken)
    
        (nameOfMovie . movies $ movieToken) (rating.movies $ movieToken)



-- Utility functions for the next block, includes a pattern synonym for use of
-- Maybe to propagate success



hashCheck :: Token -> [RegisterMessage] -> Bool

hashCheck tokenToBeChecked list = elem tokenToBeChecked $ passOrToken <$> list



hashSHA256 :: String -> String

hashSHA256 = show . hashWith SHA256 . (read :: String -> B.ByteString)


    
userList :: IO (Maybe [RegisterMessage])

userList = (fmap.fmap) usernames decodeAttempt



moviesList :: IO (Maybe [MovieToken])

moviesList = (fmap.fmap) movieTokens decodeAttempt



decodeAttempt :: IO (Maybe JSONStore)

decodeAttempt = decode <$> LB.readFile "db.json"



updateDB :: [RegisterMessage] -> [MovieToken] -> IO ()

updateDB usersField tokensField = "db.json" `LB.writeFile` encode (JSONStore usersField tokensField)



pattern NoExistingDBEntry :: Maybe a

pattern NoExistingDBEntry = Nothing



-- mandated functions to make impl work



-- | Get the server token by registering/logging in with the given username and password

register :: String -> String -> IO (Maybe Token)

register uName (\u -> hashSHA256 $ uName <> u -> hashed) =

    let returnToken = pure hashed

        wrongPassword = Nothing in

    processedInputs >>= \case

        NoExistingDBEntry -> registerToken >> pure returnToken

        Just registerMessageList -> pure $ 
        
            if hashCheck hashed registerMessageList

                then returnToken

                else wrongPassword

    where

        processedInputs = userList <&> \(Just list) ->
            
            if notElem uName $ user <$> list
                
                then NoExistingDBEntry
                
                else Just list
        
        registerToken = do

            existingUserList <- fromMaybe [] <$> userList

            existingMoviesList <- fromMaybe [] <$> moviesList

            let newUserList = RM uName hashed : existingUserList

            updateDB newUserList existingMoviesList



-- | Add a movie to your user list in the server, given the user token, the name
-- of the movie and the rating

addMovie :: Token -> String -> Int -> IO (Maybe ())

addMovie entryToken movieName movieRating =

    let invalidToken = pure Nothing

        newMovie = MovieToken entryToken (Movie movieName movieRating) in

        do

    existingUserList  <- fromMaybe [] <$> userList

    existingMoviesList <- fromMaybe [] <$> moviesList

    case hashCheck entryToken existingUserList of

        False -> invalidToken
        
        True | alreadyInList existingMoviesList -> pure <$> overwriteRating (existingUserList, existingMoviesList, newMovie)

        True -> pure <$> appendToList (existingUserList, existingMoviesList, newMovie)
        
    where

        alreadyInList existingMoviesList = not . null $
                
            filter matchesTokenAndName existingMoviesList
        
        matchesTokenAndName u = movieName == (nameOfMovie . movies) u

            && entryToken == token u
        
        overwriteRating (existingUserList, existingMoviesList, newMovie) = 
                            
            let filteredMoviesList = filter (not . matchesTokenAndName) existingMoviesList in

            updateDB existingUserList $ newMovie : filteredMoviesList

        appendToList (existingUserList, existingMoviesList, newMovie) =

            updateDB existingUserList $ newMovie : existingMoviesList



-- | Search for movies given a user token and a query string

findMovies :: Maybe Token -> Maybe String -> IO (Maybe [Movie])

forbidden :: IO (Maybe a)

forbidden = pure Nothing

findMovies Nothing queryArg = forbidden

findMovies (Just tokArg) queryArg = do
    
    registry <- (fmap.fmap) passOrToken $ fromMaybe [] <$> userList

    movieList <- fromMaybe [] <$> moviesList

    case elem tokArg registry of

        False -> forbidden

        True -> 
            
            let filteredAndConvertedMovieList = movies <$> filter ((==tokArg).token) movieList in
            
            pure.pure $ findMoviesInner queryArg filteredAndConvertedMovieList

    where
        
        findMoviesInner Nothing movieList = []

        findMoviesInner (Just queryArg) movieList = filter ( parse queryArg . nameOfMovie ) movieList

        parse queryArg list = go queryArg list 
        
            where
                
                go queryArg [] = False
                
                go queryArg (x:xs) =
                    
                    if (all id $ zipWith (==) queryArg list) 
                        
                        then True

                        else go queryArg xs