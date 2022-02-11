{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import           Cardano.Api
import           Cardano.Api.Shelley
import           Cardano.Ledger.Address (getRewardAcnt)
import           Cardano.Ledger.Alonzo.PParams (PParams' (..))
import qualified Cardano.Ledger.BaseTypes as L
import           Cardano.Ledger.Compactible (Compactible (..))
import qualified Cardano.Ledger.Core as LC
import qualified Cardano.Ledger.Shelley.API as L
import qualified Cardano.Ledger.Shelley.Rewards as L
import qualified Cardano.Ledger.Shelley.RewardUpdate as L
import qualified Codec.Binary.Bech32 as Bech32
import           Control.Monad.Trans.Except (runExceptT)
import qualified Data.Binary.Get as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Base16 as Base16
import           Data.Char (ord)
import           Data.Foldable (toList)
import           Data.List (intercalate)
import qualified Data.Compact.VMap as VMap
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import qualified Data.Text as T
import           GHC.Records (HasField (..))
import           Options.Applicative (Parser, (<|>), (<**>))
import qualified Options.Applicative as Opt
import           Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley

import Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as BL

data State = State
  { lastCheckpoint    :: SlotNo
  , lastBalanceCheck  :: EpochNo
  , lastRewStartEpoch :: EpochNo
  , lastRewEndEpoch   :: EpochNo
  , lastEra           :: String
  , lastProtocolVer   :: L.ProtVer
  }

startingState :: State
startingState = State
  { lastCheckpoint    = SlotNo 0
  , lastBalanceCheck  = EpochNo 0
  , lastRewStartEpoch = EpochNo 0
  , lastRewEndEpoch   = EpochNo 0
  , lastEra           = "byron"
  , lastProtocolVer   = L.ProtVer 0 0
  }

data IsOwner = IsOwnerYes | IsOwnerNo
  deriving (Show)

data IsPoolRewardAccount = IsPoolRewardAccountYes | IsPoolRewardAccountNo
  deriving (Show)

data Event c
  = CheckPointEvent SlotNo
  | NewEraEvent EpochNo SlotNo String
  | NewPVEvent EpochNo SlotNo L.ProtVer
  | StakeRegistrationEvent EpochNo SlotNo
  | StakeDeRegistrationEvent EpochNo SlotNo
  | DelegationEvent SlotNo (Hash StakePoolKey)
  | PoolRegistrationEvent SlotNo (Hash StakePoolKey) IsOwner IsPoolRewardAccount
  | MIREvent EpochNo SlotNo Lovelace L.MIRPot
  | WithdrawalEvent EpochNo SlotNo Lovelace
  | RewardBalanceEvent EpochNo SlotNo Lovelace
  | RewardStartEvent EpochNo SlotNo Lovelace
  | RewardEndEvent EpochNo SlotNo (Set (L.Reward c))
  | Error String

msg :: Channel -> Block era -> IO (Maybe Int)
msg chan block = let (Block (BlockHeader slotNo _blockHeaderHash (BlockNo _blockNoI)) transactions) = block
  in publishMsg chan "" "cardano-mq-sync" $ newMsg {msgBody = (BL.pack (show _blockHeaderHash)), msgDeliveryMode = Just Persistent}

maybeToRight :: a -> Maybe b -> Either a b
maybeToRight a Nothing   = Left a
maybeToRight _ (Just b) = Right b

first :: (a -> c) -> Either a b -> Either c b
first f (Left a) = Left (f a)
first _ (Right b) = Right b

isPermittedPrefix :: T.Text -> Bool
isPermittedPrefix prefix = prefix `elem` ["stake", "stake_test"]

data CheckPoint = CheckPointOff | CheckPointSize SlotNo

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
watchQueue (msg, env) = do
    putStrLn $ "received message: " ++ (BL.unpack $ msgBody msg)
    -- acknowledge receiving the message
    ackEnv env


main :: IO ()
main = do
  conn <- openConnection "127.0.0.1" "/" "guest" "guest"
  chan <- openChannel conn

  -- declare a queue, exchange and binding
  declareQueue chan newQueue {queueName = "cardano-mq-sync"}
  declareExchange chan newExchange {exchangeName = "cardano-mq-sync", exchangeType = "direct"}
  bindQueue chan "cardano-mq-sync" "cardano-mq-sync" "cardano-mq-sync"

  -- subscribe to the queue
  consumeMsgs chan "cardano-mq-sync" Ack watchQueue

  args <- Opt.execParser $
    Opt.info (parser <**> Opt.helper) (Opt.fullDesc <> Opt.progDesc "Cardano MQ Sync")
  let f = either (error . T.unpack . renderFoldBlocksError) id
  !_ <- fmap f $ runExceptT $ foldBlocks
         (conf args)
         (socket args)
         QuickValidation
         startingState
         (\_env
           !ledgerState
           _
           (BlockInMode
             block
             _era)
           state -> do
             msg chan block
    )


  getLine -- wait for keypress
  closeConnection conn
  putStrLn "connection closed"
  return ()
