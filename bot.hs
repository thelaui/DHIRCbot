import Network
import System.IO
import Text.Printf
import Data.List
import System.Exit
import System.Random
import Data.IORef

data IrcState = IrcState 
  { counter :: Int
  , ircServer :: String
  } deriving (Show, Read)

defaultIrcState :: IrcState
defaultIrcState = IrcState 
  { counter = 0 
  , ircServer = "chat.freenode.net" 
  }

-- IORef 
 
server = "chat.freenode.net"
port   = 6667
chan   = "#marsshootertest"
nick   = "InfoDude"
 
main = do
    stateRef <- newIORef defaultIrcState
    
    readIORef stateRef >>= print     
    -- x <- readIORef stateRef
    -- print x
    modifyIORef stateRef (\x -> x {counter = 1 + counter x} )
    readIORef stateRef >>= print

    handle <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering handle NoBuffering
    write handle "NICK" nick
    write handle "USER" (nick++" 0 * :tutorial bot")
    write handle "JOIN" chan
    listen handle
 
write :: Handle -> String -> String -> IO ()
write h s t = do
    hPrintf h "%s %s\r\n" s t
 
listen :: Handle -> IO ()
listen h = forever $ do
    t <- hGetLine h
    let s = init t
    if ping s then pong s else eval h (clean s)
    putStrLn s
  where
    forever a = a >> forever a
 
    clean     = drop 1 . dropWhile (/= ':') . drop 1
 
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write h "PONG" (':' : drop 6 x)
    		
    		
    
eval :: Handle -> String -> IO ()
--eval h    "!gtfo"                = write h "QUIT" ":Exiting" >> exitWith ExitSuccess

----- Greetings -----
eval h x | "huhu"     `isPrefixOf` x = privmsg h "Greetings!"
eval h x | "hello"    `isPrefixOf` x = privmsg h "Greetings!"
eval h x | "hallo"    `isPrefixOf` x = privmsg h "Greetings!"
eval h x | "hi"       `isPrefixOf` x = privmsg h "Greetings!"
eval h x | "hey"      `isPrefixOf` x = privmsg h "Greetings!"
eval h x | "salute"   `isPrefixOf` x = privmsg h "Greetings!"
eval h x | "servus"   `isPrefixOf` x = privmsg h "Greetings!"
eval h x | "aloha"    `isPrefixOf` x = privmsg h "Greetings!"
eval h x | "moin"     `isPrefixOf` x = privmsg h "Greetings!"
eval h x | "Huhu"     `isPrefixOf` x = privmsg h "Greetings!"
eval h x | "Hello"    `isPrefixOf` x = privmsg h "Greetings!"
eval h x | "Hallo"    `isPrefixOf` x = privmsg h "Greetings!"
eval h x | "Hi"       `isPrefixOf` x = privmsg h "Greetings!"
eval h x | "Hey"      `isPrefixOf` x = privmsg h "Greetings!"
eval h x | "Salute"   `isPrefixOf` x = privmsg h "Greetings!"
eval h x | "Servus"   `isPrefixOf` x = privmsg h "Greetings!"
eval h x | "Aloha"    `isPrefixOf` x = privmsg h "Greetings!"
eval h x | "Moin"     `isPrefixOf` x = privmsg h "Greetings!"

----- Good Byeings -----
eval h x | "bye"      `isPrefixOf` x = privmsg h "Bye bye! Come back soon!"
eval h x | "bb"       `isPrefixOf` x = privmsg h "Bye bye! Come back soon!"
eval h x | "cu"       `isPrefixOf` x = privmsg h "Bye bye! Come back soon!"
eval h x | "see you"  `isPrefixOf` x = privmsg h "Bye bye! Come back soon!"
eval h x | "ciao"     `isPrefixOf` x = privmsg h "Bye bye! Come back soon!"
eval h x | "adios"    `isPrefixOf` x = privmsg h "Bye bye! Come back soon!"
eval h x | "tschau"   `isPrefixOf` x = privmsg h "Bye bye! Come back soon!"
eval h x | "bis bald" `isPrefixOf` x = privmsg h "Bye bye! Come back soon!"
eval h x | "goodbye"  `isPrefixOf` x = privmsg h "Bye bye! Come back soon!"
eval h x | "Bye"      `isPrefixOf` x = privmsg h "Bye bye! Come back soon!"
eval h x | "See you"  `isPrefixOf` x = privmsg h "Bye bye! Come back soon!"
eval h x | "Ciao"     `isPrefixOf` x = privmsg h "Bye bye! Come back soon!"
eval h x | "Adios"    `isPrefixOf` x = privmsg h "Bye bye! Come back soon!"
eval h x | "Tschau"   `isPrefixOf` x = privmsg h "Bye bye! Come back soon!"
eval h x | "Bis bald" `isPrefixOf` x = privmsg h "Bye bye! Come back soon!"
eval h x | "Goodbye"  `isPrefixOf` x = privmsg h "Bye bye! Come back soon!"


----- Emotes -----
eval h x | ":D"     `isPrefixOf` x = privmsg h ":D Good one!"
eval h x | "^^"     `isPrefixOf` x = privmsg h ":) nice"
eval h x | ":)"     `isPrefixOf` x = privmsg h ":)"
eval h x | ":("     `isPrefixOf` x = privmsg h "Don't worry, be happy!"
eval h x | ":-("    `isPrefixOf` x = privmsg h "Don't worry, be happy!"
eval h x | ":'-("   `isPrefixOf` x = privmsg h "Don't worry, be happy!"
eval h x | ":'("    `isPrefixOf` x = privmsg h "Don't worry, be happy!"
eval h x | "w00t"   `isPrefixOf` x = privmsg h "I'm surprised, too!"
eval h x | "woot"   `isPrefixOf` x = privmsg h "I'm surprised, too!"
eval h x | "o.O"    `isPrefixOf` x = privmsg h "I'm surprised, too!"
eval h x | "O.o"    `isPrefixOf` x = privmsg h "I'm surprised, too!"
eval h x | "Oo"     `isPrefixOf` x = privmsg h "I'm surprised, too!"
eval h x | "oO"     `isPrefixOf` x = privmsg h "I'm surprised, too!"
eval h x | "B)"     `isPrefixOf` x = privmsg h "You aren't as cool as you think!"
eval h x | "B-)"    `isPrefixOf` x = privmsg h "You aren't as cool as you think!"
eval h x | "-.-"    `isPrefixOf` x = privmsg h "Don't be mad!"

----- Commands -----
eval h x | "!help !download"     `isPrefixOf` x = privmsg h "I give you the link to the download page of M.A.R.S."
eval h x | "!help !homepage"     `isPrefixOf` x = privmsg h "I give you the link to the homepage of M.A.R.S."
eval h x | "!help !twitter"      `isPrefixOf` x = privmsg h "I give you the link to the twitter page of M.A.R.S."
eval h x | "!help !facebook"     `isPrefixOf` x = privmsg h "I give you the link to the facebook wall of M.A.R.S."
eval h x | "!help !forum"        `isPrefixOf` x = privmsg h "I give you the link to the forum of M.A.R.S."
eval h x | "!help !mail"         `isPrefixOf` x = privmsg h "I give you the email address of MarsCoreTeam"
eval h x | "!help !joke"         `isPrefixOf` x = privmsg h "I tell a more or less funny joke"
eval h x | "!help"     `isPrefixOf` x = do
					privmsg h "Available commands are: !download, !facebook, !forum, !homepage, !joke, !mail, !twitter"
					privmsg h "Type !help <command> for detailed information."
eval h x | "!download" `isPrefixOf` x = privmsg h "http://mars-game.sourceforge.net/?page_id=8"
eval h x | "!forum"    `isPrefixOf` x = privmsg h "http://mars-game.sourceforge.net/?page_id=160"
eval h x | "!homepage" `isPrefixOf` x = privmsg h "http://www.marsshooter.org"
eval h x | "!twitter"  `isPrefixOf` x = privmsg h "http://twitter.com/MarsCoreTeam"
eval h x | "!facebook" `isPrefixOf` x = privmsg h "http://de-de.facebook.com/pages/MarsCoreTeam/191763920848958?v=info#!/pages/MarsCoreTeam/191763920848958?sk=wall"
eval h x | "!mail"     `isPrefixOf` x = privmsg h "marscoreteam@googlemail.com"
eval h x | "!joke"     `isPrefixOf` x = do 
                                        r <- randomIO
                                        tellJoke h (abs(((r + 1):: Int)`mod` 8))



eval h x | "InfoDude"  `isPrefixOf` x = postInfo h
eval h x | "infodude"  `isPrefixOf` x = postInfo h
eval h x | "infoDude"  `isPrefixOf` x = postInfo h
eval h x | "Infodude"  `isPrefixOf` x = postInfo h
eval h x | "@InfoDude" `isPrefixOf` x = postInfo h
eval h x | "@infodude" `isPrefixOf` x = postInfo h
eval h x | "@infoDude" `isPrefixOf` x = postInfo h
eval h x | "@Infodude" `isPrefixOf` x = postInfo h

eval h x | "shit" `isInfixOf` x = privmsg h "Bad word!"

eval _   _                       = return () -- ignore everything else

privmsg :: Handle -> String -> IO ()
privmsg h s = write h "PRIVMSG" (chan ++ " :" ++ s)

postInfo :: Handle -> IO ()
postInfo handle = do
	privmsg handle "I am the InfoDude (built by thelaui) and my job is to give you some information about M.A.R.S.!"
	privmsg handle "Type !help for a list of commands you can use to... well... command me!"

tellJoke :: Handle -> Int -> IO ()
tellJoke handle number | number == 0 = do 
                                        privmsg handle "Mommy, Mommy! When are we going to have Aunt Edna for dinner?"
                                        privmsg handle "Shut up, we haven't even finished your Grandmother yet."
tellJoke handle number | number == 1 =  privmsg handle "I may be Schizophrenic, but at least I have each other."
tellJoke handle number | number == 2 = do 
                                        privmsg handle "My aunt died, God bless her, at a ripe old age of 104."
                                        privmsg handle "We called her Aunt Tique."
tellJoke handle number | number == 3 = do 
                                        privmsg handle "Q: Why didn't the sailors play cards?"
                                        privmsg handle "A: Because the captain was sitting on the deck."
tellJoke handle number | number == 4 = privmsg handle "Don't say this to a cop: Aren't you the guy from the villiage people?"
tellJoke handle number | number == 5 = privmsg handle "Welcome to Entropy Burgers -- may I take your order?"
tellJoke handle number | number == 6 = privmsg handle "A little boy went up to his father and asked, >>Dad, where did my intelligence come from?<< The father replied, >>Well, son, you must have got it from your mother, cause I still have mine.<<"
tellJoke handle number | number == 7 = privmsg handle "A bird in the hand makes it hard to blow your nose."
tellJoke handle _ =                     privmsg handle "Best joke ever: M.A.R.S.' network mode is coming soon!"

