{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import           Cardano.Api
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Options.Applicative (Parser, (<**>))
import qualified Options.Applicative as Opt
import           Control.Monad.Trans.Except (runExceptT)
import           Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Base16 as B16
import           Cardano.Crypto.Hash.Class (hashToBytesAsHex)

msg :: Channel -> Block era -> IO (Maybe Int)
msg chan block =
  publishMsg
    chan
    ""
    "cardano-mq-sync"
    (newMsg {msgBody = (BL.pack (show $ convertHash blockHeaderHash)), msgDeliveryMode = Just Persistent})
  where
    (Block (BlockHeader _slotNo blockHeaderHash (BlockNo _blockNoI)) _transactions) = block
    convertHash = T.decodeLatin1 . B16.encode . serialiseToRawBytes

data Args = Args
  { conf         :: String
  , socket       :: String }

parser :: Parser Args
parser = Args
  <$> Opt.strOption
      ( Opt.long "conf"
     <> Opt.short 'c'
     <> Opt.metavar "FILEPATH"
     <> Opt.help "configuration file" )
  <*> Opt.strOption
      ( Opt.long "socket"
     <> Opt.short 's'
     <> Opt.metavar "FILEPATH"
     <> Opt.help "socket" )

watchQueue :: (Message,Envelope) -> IO ()
watchQueue (message, env) = do
    putStrLn $ "received message: " ++ (BL.unpack $ msgBody message)
    -- acknowledge receiving the message
    ackEnv env

main :: IO ()
main = do
  conn <- openConnection "127.0.0.1" "/" "guest" "guest"
  chan <- openChannel conn

  -- declare a queue, exchange and binding
  _ <- declareQueue chan newQueue {queueName = "cardano-mq-sync"}
  declareExchange chan newExchange {exchangeName = "cardano-mq-sync", exchangeType = "direct"}
  bindQueue chan "cardano-mq-sync" "cardano-mq-sync" "cardano-mq-sync"

  -- subscribe to the queue
  _ <- consumeMsgs chan "cardano-mq-sync" Ack watchQueue

  args <- Opt.execParser $
    Opt.info (parser <**> Opt.helper) (Opt.fullDesc <> Opt.progDesc "Cardano MQ Sync")
  let f = either (error . T.unpack . renderFoldBlocksError) id
  !_ <- fmap f $ runExceptT $ foldBlocks
         (conf args)
         (socket args)
         QuickValidation
         ()
         (\_env
           !_ledgerState
           _
           (BlockInMode
             block
             _era)
           _ -> do
             msgNum <- msg chan block
             print $ "message number " <> show msgNum
             return ()
    )


  _ <- getLine -- wait for keypress
  closeConnection conn
  putStrLn "connection closed"
  return ()
