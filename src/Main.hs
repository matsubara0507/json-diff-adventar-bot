{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad      (when)
import           Data.Either        (isRight)
import           Data.List          (intersperse)
import           Data.Maybe         (fromJust, isJust)
import           Data.Text          (Text, pack, unpack)
import           Entry
import           Json
import           Slack
import           System.Environment (getArgs, getEnv)

import           GHC.IO.Encoding

main :: IO ()
main = do
  setLocaleEncoding utf8
  [oldPath, newPath, channel] <- fmap pack <$> getArgs
  [token] <- fmap pack <$> mapM getEnv ["SLACK_TOKEN"]
  runBot oldPath newPath (token, channel)

runBot :: Text -> Text -> (Token, ChannelName) -> IO ()
runBot oldPath newPath (token, channel) = do
  oldCal <- readEntryJson oldPath
  newCal <- readEntryJson newPath

  let
    message = mkMessage oldCal newCal

  when (isJust message) $ do
    result <- postMessage token channel (pack $ fromJust message)
    case result of
      Right _ -> putStrLn "Success!"
      Left  e -> putStrLn $ "Error: " `mappend` unpack e

    when (isRight result) $ do
      putStrLn "Update Json"
      updateEntryJson oldPath newCal
      putStrLn "Updated Json"

mkMessage :: Calendar -> Calendar -> Maybe String
mkMessage oldCal newCal =
  if null message then Nothing else Just message
  where
    message = unlines . filter ("" /=) $ mkMessage' <$> dates
    mkMessage' date = diffShow date oldCal newCal

diffShow :: Date -> Calendar -> Calendar -> String
diffShow date = (.) (diffShow' date) . diff date

diffShow' :: Date -> DiffEntry -> String
diffShow' date diffEntry =
  case diffEntry of
    NewEntry newEntry ->
      mconcat [unpack date, " [New] ", ppEntry newEntry]
    UpdateBody newEntry ->
      mconcat [unpack date, " [Update] ", ppEntry newEntry]
    RemoveEntry oldEntry ->
      mconcat [unpack date, " [Remove] ", ppEntry oldEntry]
    ChangeUser oldEntry newEntry ->
      mconcat . intersperse "\n" $
        diffShow' date <$> [RemoveEntry oldEntry, NewEntry newEntry]
    NoChanged -> ""

ppEntry :: Entry -> String
ppEntry (Entry user' comment' title' url') =
  mconcat [mkBody comment' title' url', " by ", unpack user']
  where
    mkBody "" _ ""          = unpack "No Comment..."
    mkBody comment_ _ ""    = unpack comment_
    mkBody comment_ "" url_ = unpack $ mconcat ["<", url_, "|", comment_, ">"]
    mkBody _ title_ url_    = unpack $ mconcat ["<", url_, "|", title_, ">"]
