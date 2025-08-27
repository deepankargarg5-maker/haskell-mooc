{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module Set14b where

import Mooc.Todo
import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Text.Encoding (encodeUtf8)
import Text.Read (readMaybe)
import Network.Wai (pathInfo, responseLBS, Application)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Database.SQLite.Simple (open, execute_, execute, query, query_, Connection, Query(..), Only(..))

------------------------------------------------------------------------------
-- Ex 1
initQuery :: Query
initQuery = Query (T.pack "CREATE TABLE IF NOT EXISTS events (account TEXT NOT NULL, amount NUMBER NOT NULL);")

depositQuery :: Query
depositQuery = Query (T.pack "INSERT INTO events (account, amount) VALUES (?, ?);")

getAllQuery :: Query
getAllQuery = Query (T.pack "SELECT account, amount FROM events;")

openDatabase :: String -> IO Connection
openDatabase filename = do
  conn <- open filename
  execute_ conn initQuery
  return conn

deposit :: Connection -> T.Text -> Int -> IO ()
deposit conn account amount =
  execute conn depositQuery (account, amount)

------------------------------------------------------------------------------
-- Ex 2
balanceQuery :: Query
balanceQuery = Query (T.pack "SELECT amount FROM events WHERE account = ?;")

balance :: Connection -> T.Text -> IO Int
balance conn account = do
  amounts <- query conn balanceQuery (Only account) :: IO [Only Int]
  return (sum (map fromOnly amounts))

------------------------------------------------------------------------------
-- Ex 3
data Command = Deposit T.Text Int | Balance T.Text | Withdraw T.Text Int
  deriving (Show, Eq)

parseInt :: T.Text -> Maybe Int
parseInt = readMaybe . T.unpack

parseCommand :: [T.Text] -> Maybe Command
parseCommand ["deposit", acc, amtTxt] = do
  amt <- parseInt amtTxt
  return (Deposit acc amt)
parseCommand ["balance", acc] =
  return (Balance acc)
parseCommand ["withdraw", acc, amtTxt] = do
  amt <- parseInt amtTxt
  return (Withdraw acc amt)
parseCommand _ = Nothing

------------------------------------------------------------------------------
-- Ex 4
perform :: Connection -> Maybe Command -> IO T.Text
perform _ Nothing = return (T.pack "ERROR")
perform conn (Just cmd) = case cmd of
  Deposit acc amt -> deposit conn acc amt >> return (T.pack "OK")
  Withdraw acc amt -> deposit conn acc (-amt) >> return (T.pack "OK")
  Balance acc -> do
    bal <- balance conn acc
    return (T.pack (show bal))

------------------------------------------------------------------------------
-- Ex 5
encodeResponse :: T.Text -> LB.ByteString
encodeResponse t = LB.fromStrict (encodeUtf8 t)

simpleServer :: Application
simpleServer _req respond =
  respond (responseLBS status200 [] (encodeResponse (T.pack "BANK")))

------------------------------------------------------------------------------
-- Ex 6
server :: Connection -> Application
server conn request respond = do
  let cmd = parseCommand (pathInfo request)
  print cmd
  res <- perform conn cmd
  respond (responseLBS status200 [] (encodeResponse res))

port :: Int
port = 3421

main :: IO ()
main = do
  db <- openDatabase "bank.db"
  putStrLn $ "Running on port: " ++ show port
  run port (server db)
