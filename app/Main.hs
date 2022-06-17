{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState (get, put), execStateT)
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Attoparsec.ByteString.Lazy as PL
import qualified Data.ByteString.Lazy as BL
import Debug.Trace (traceShowM)
import System.IO (hFlush, stderr, stdout)

data GameInit = GameInit

data GameInfo = GameInfo

data Move = Move

gameInitParser :: P.Parser GameInit
gameInitParser = undefined

gameInfoParser :: P.Parser GameInfo
gameInfoParser = undefined

parseOrDie :: MonadIO m => MonadState BL.ByteString m => P.Parser a -> m a
parseOrDie p = do
  bs <- get
  let res =
        PL.parse p bs
  case res of
    PL.Fail rest _ err -> do
      traceShowM err
      traceShowM $ BL.take 25 bs
      traceShowM $ BL.take 10 rest
      liftIO $ hFlush stderr
      error $ "Parse error: " ++ err
    PL.Done rest r -> do
      put rest
      pure r

main :: IO ()
main = do
  contents <- BL.getContents
  void $ execStateT mainM contents

mainM :: MonadIO m => MonadState BL.ByteString m => m ()
mainM = do
  gi <- parseOrDie gameInitParser
  mainLoop gi

mainLoop :: MonadIO m => MonadState BL.ByteString m => GameInit -> m ()
mainLoop gi = do
  gs <- parseOrDie (P.endOfLine *> gameInfoParser)
  let move = doMove gi gs
  liftIO $ putStrLn $ moveStr move
  liftIO $ hFlush stdout
  mainLoop gi

moveStr :: Move -> String
moveStr = undefined

doMove :: GameInit -> GameInfo -> Move
doMove = undefined
