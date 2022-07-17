

module MoviesClient where

import Data.Aeson
import GHC.Generics
import System.Console.Haskeline
import Control.Monad.Trans.State.Lazy (StateT)
import Control.Monad.Trans (liftIO, MonadTrans (lift))
import Data.Maybe (fromMaybe)
import Data.Functor ((<&>))


type Token = String

type Address = String

type Port = String

type Server = (Token, Address, Port)



-- StateT for storage



type MoviesClient m a = StateT Server m a


data Movie =

    Movie
    { nameOfMovie :: String
    , rating :: Int}

    deriving Generic

instance ToJSON Movie
instance FromJSON Movie

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
    | Quit

-- | mainLoop is the hook for the program, called by the main module. It runs
-- | a console which allows pinging the server.

mainLoop :: MoviesClient (InputT IO) ()
mainLoop = executeInput =<< parseInput <$> ( lift $ getInputLine "MoviesClient > " )

    where

        executeInput :: Maybe Command -> MoviesClient (InputT IO) ()
        executeInput Nothing = mainLoop
        executeInput (Just input) =
            
            case input of
                SetAddress address port -> undefined
                Register username password -> undefined
                AddMovie token movieName rating -> undefined
                FindMovies token movieName -> undefined
                Quit -> pure ()

        parseInput :: Maybe String -> Maybe Command
        parseInput providedLine =
    
            case processArgs . rmSpace . span ( == ' ' ) <$> providedLine of

                Just ( "SetAddress" , a) -> undefined
                Just ( "Register" , a) -> undefined
                Just ( "AddMovie" , a) -> undefined
                Just ( "FindMovies" , a) -> undefined
                Just ( "Quit" , "" ) -> pure Quit
                _ -> Nothing
    
                where

                    processArgs ( r , input ) = undefined

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