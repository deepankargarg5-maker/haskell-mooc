module Set14b where

import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Text.Encoding (encodeUtf8)
import Text.Read (readMaybe)

import Network.Wai (pathInfo, responseLBS, Application, Request)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)

import Database.SQLite.Simple
    ( open, execute, execute_, query, query_, Connection, Query(..) )

-- DATABASE SETUP

initQuery :: Query
initQuery = Query (T.pack "CREATE TABLE IF NOT EXISTS events (account TEXT NOT NULL, amount NUMBER NOT NULL);")

depositQuery :: Query
depositQuery = Query (T.pack "INSERT INTO events (account, amount) VALUES (?, ?);")

getAllQuery :: Query
getAllQuery = Query (T.pack "SELECT account, amount FROM events;")

openDatabase :: String -> IO Connection
openDatabase name = do
  conn <- open name
  execute_ conn initQuery
  return conn

deposit :: Connection -> T.Text -> Int -> IO ()
deposit db name amount = execute db depositQuery (name, amount)

-- BALANCE CHECKING

balanceQuery :: Query
balanceQuery = Query (T.pack "SELECT amount FROM events WHERE account = ?;")

balance :: Connection -> T.Text -> IO Int
balance db name = do
  rows <- query db balanceQuery (Only name) :: IO [Only Int]
  return $ sum [x | Only x <- rows]

-- COMMAND PARSING

data Command = Deposit T.Text Int | Withdraw T.Text Int | Balance T.Text
  deriving (Show, Eq)

parseInt :: T.Text -> Maybe Int
parseInt = readMaybe . T.unpack

parseCommand :: [T.Text] -> Maybe Command
parseCommand [cmd, acc, amtText]
  | cmd == "deposit" = parseInt amtText >>= \amt -> Just (Deposit acc amt)
  | cmd == "withdraw" = parseInt amtText >>= \amt -> Just (Withdraw acc amt)
parseCommand [cmd, acc]
  | cmd == "balance" = Just (Balance acc)
parseCommand _ = Nothing

-- RUNNING COMMANDS

perform :: Connection -> Maybe Command -> IO T.Text
perform _ Nothing = return "ERROR"
perform db (Just (Deposit name amt)) = deposit db name amt >> return "OK"
perform db (Just (Withdraw name amt)) = deposit db name (-amt) >> return "OK"
perform db (Just (Balance name)) = do
  bal <- balance db name
  return $ T.pack (show bal)

-- RESPONSE ENCODING

encodeResponse :: T.Text -> LB.ByteString
encodeResponse = LB.fromStrict . encodeUtf8

-- SIMPLE SERVER

simpleServer :: Application
simpleServer _ respond = respond (responseLBS status200 [] (encodeResponse "BANK"))

-- FULL BANK SERVER

server :: Connection -> Application
server db request respond = do
  let command = parseCommand (pathInfo request)
  response <- perform db command
  respond (responseLBS status200 [] (encodeResponse response))

-- SERVER ENTRY POINT

port :: Int
port = 3421

main :: IO ()
main = do
  db <- openDatabase "bank.db"
  putStrLn $ "Running on port: " ++ show port
  run port (server db)
