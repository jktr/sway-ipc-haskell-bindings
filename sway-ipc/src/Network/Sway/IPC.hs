{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}

module Network.Sway.IPC (
  Client,
  EventCallback,
  EventType(..),
  IpcException(..),
  QueryCommand(..),
  RunCommand(..),
  SendTickCommand(..),
  SubscribeCommand(..),
  runCommand,
  subscribe,
  startClient',
  startClient,
  stopClient,
) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM (STM, TVar, TMVar)
import Control.Concurrent.STM qualified as STM
import Control.Exception (bracketOnError, Exception)
import Control.Exception qualified as STM
import Control.Monad (when, forM_, forever)
import Control.Monad.Fix (mfix)
import Data.Aeson ((.!=), (.:?))
import Data.Aeson qualified as A
import Data.Binary (Word32)
import Data.Binary.Get (Get)
import Data.Binary.Get qualified as BG
import Data.Binary.Put (Put)
import Data.Binary.Put qualified as BP
import Data.ByteString qualified as BS
import Data.ByteString.Builder.Extra (defaultChunkSize)
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.Socket (Socket)
import Network.Socket qualified as S
import Network.Socket.ByteString qualified as S
import System.Environment (getEnv)

data Client = Client
  { socket :: Socket
  , commands :: TVar (Map CommandType (TMVar BSL.ByteString))
  , callbacks :: TVar (Map EventType [EventCallback])
  , recvThread :: ThreadId
  }

type MessageId = Word32
type MessageType = Either CommandType EventType
type EventCallback = EventType -> A.Value -> IO ()

data CommandType
  = RunCommandType
  | QueryCommandType QueryCommand
  | SubscribeCommandType
  | SendTickCommandType
  | SyncCommandType
  deriving (Show, Eq, Ord)

data EventType
  = WorkspaceEventType
  | ModeEventType
  | WindowEventType
  | BarConfigUpdateEventType
  | BindingEventType
  | ShutdownEventType
  | TickEventType
  | BarStateUpdateEventType
  | InputEventType
  deriving (Eq, Ord)

instance Show EventType where
  show WorkspaceEventType       = "workspace"
  show ModeEventType            = "mode"
  show WindowEventType          = "window"
  show BarConfigUpdateEventType = "barconfig_update"
  show BindingEventType         = "binding"
  show ShutdownEventType        = "shutdown"
  show TickEventType            = "tick"
  show BarStateUpdateEventType  = "bar_state_update"
  show InputEventType           = "input"

instance A.ToJSON EventType where
  toJSON x = A.toJSON (show x)

idToMessageType :: MonadFail m => MessageId -> m MessageType
idToMessageType   0 = pure (Left RunCommandType)
idToMessageType   1 = pure (Left $ QueryCommandType GetWorkspaces)
idToMessageType   2 = pure (Left SubscribeCommandType)
idToMessageType   3 = pure (Left $ QueryCommandType GetOutputs)
idToMessageType   4 = pure (Left $ QueryCommandType GetTree)
idToMessageType   5 = pure (Left $ QueryCommandType GetMarks)
idToMessageType   6 = pure (Left $ QueryCommandType GetBarConfig)
idToMessageType   7 = pure (Left $ QueryCommandType GetVersion)
idToMessageType   8 = pure (Left $ QueryCommandType GetBindingModes)
idToMessageType   9 = pure (Left $ QueryCommandType GetConfig)
idToMessageType  10 = pure (Left SendTickCommandType)
idToMessageType  11 = pure (Left SyncCommandType)
idToMessageType  12 = pure (Left $ QueryCommandType GetBindingState)
idToMessageType 100 = pure (Left $ QueryCommandType GetInputs)
idToMessageType 101 = pure (Left $ QueryCommandType GetSeats)
idToMessageType 0x80000000 = pure (Right WorkspaceEventType)
idToMessageType 0x80000002 = pure (Right ModeEventType)
idToMessageType 0x80000003 = pure (Right WindowEventType)
idToMessageType 0x80000004 = pure (Right BarConfigUpdateEventType)
idToMessageType 0x80000005 = pure (Right BindingEventType)
idToMessageType 0x80000006 = pure (Right ShutdownEventType)
idToMessageType 0x80000007 = pure (Right TickEventType)
idToMessageType 0x80000014 = pure (Right BarStateUpdateEventType)
idToMessageType 0x80000015 = pure (Right InputEventType)
idToMessageType msgId = fail $ "unhandled message id: " <> show msgId

commandTypeToId :: CommandType -> MessageId
commandTypeToId RunCommandType                     =   0
commandTypeToId (QueryCommandType GetWorkspaces)   =   1
commandTypeToId SubscribeCommandType               =   2
commandTypeToId (QueryCommandType GetOutputs)      =   3
commandTypeToId (QueryCommandType GetTree)         =   4
commandTypeToId (QueryCommandType GetMarks)        =   5
commandTypeToId (QueryCommandType GetBarConfig)    =   6
commandTypeToId (QueryCommandType GetVersion)      =   7
commandTypeToId (QueryCommandType GetBindingModes) =   8
commandTypeToId (QueryCommandType GetConfig)       =   9
commandTypeToId SendTickCommandType                =  10
commandTypeToId SyncCommandType                    =  11
commandTypeToId (QueryCommandType GetBindingState) =  12
commandTypeToId (QueryCommandType GetInputs)       = 100
commandTypeToId (QueryCommandType GetSeats)        = 101


class (Show a, Show b) => Command a b | a -> b where
  getCommandType :: a -> CommandType
  encodePayload :: a -> BSL.ByteString
  decodePayload :: BSL.ByteString -> Either String b

data Result a where
  Result :: A.FromJSON a => (Either String a) -> Result a

instance forall a. A.FromJSON a => A.FromJSON (Result a) where
  parseJSON x@(A.Object o) = do
    success <- o .:? "success" .!= True
    if success
      then Result . Right <$> A.parseJSON x
      else do
        err <- o .:? "error" .!= "server reported nonspecific command failure"
        pure $ Result (Left err)

  parseJSON (x :: A.Value) = Result . Right <$> A.parseJSON x

data RunCommand
  = RunCommand Text
  | RunCommands [Text]
  deriving Show

newtype SubscribeCommand = SubscribeCommand [EventType]
  deriving Show

newtype SendTickCommand = SendTickCommand (Maybe A.Value)
  deriving Show

data QueryCommand
  = GetWorkspaces
  | GetOutputs
  | GetTree
  | GetMarks
  | GetBarConfig
  | GetVersion
  | GetBindingModes
  | GetConfig
  | GetBindingState
  | GetInputs
  | GetSeats
  deriving (Show, Eq, Ord)

instance Command RunCommand [Either String A.Object] where
  getCommandType = const RunCommandType
  encodePayload (RunCommand x) = BSL.fromStrict $ TE.encodeUtf8 x
  encodePayload (RunCommands x) = BSL.fromStrict $ TE.encodeUtf8 $ T.intercalate ";" x
  decodePayload payload = do
    res :: [Result a] <- A.eitherDecode payload
    Right $ (\(Result x) -> x) <$> res

instance Command SubscribeCommand (Either String A.Object) where
  getCommandType = const SubscribeCommandType
  encodePayload (SubscribeCommand xs) = A.encode xs
  decodePayload payload = do
    res :: Result a <- A.eitherDecode payload
    Right $ (\(Result x) -> x) res

instance Command QueryCommand A.Value where
  getCommandType = QueryCommandType
  encodePayload _ = BSL.empty
  decodePayload = A.eitherDecode

instance Command SendTickCommand (Either String A.Object) where
  getCommandType = const SendTickCommandType
  encodePayload (SendTickCommand Nothing) = BSL.empty
  encodePayload (SendTickCommand (Just x)) = A.encode x
  decodePayload = A.eitherDecode

stopClient :: Client -> IO ()
stopClient ipc = do
  killThread ipc.recvThread
  S.close ipc.socket
  STM.atomically do
    STM.modifyTVar ipc.callbacks $ const Map.empty

    -- abort pending commands with decode failure
    commands <- STM.readTVar ipc.commands
    STM.modifyTVar ipc.commands $ const Map.empty
    forM_ commands $ \x -> STM.putTMVar x BSL.empty

startClient :: IO Client
startClient = getEnv "SWAYSOCK" >>= startClient'

startClient' :: FilePath -> IO Client
startClient' path = bracketOnError socket' S.close $ \socket -> do
  S.connect socket (S.SockAddrUnix path)
  callbacks <- STM.newTVarIO Map.empty
  commands <- STM.newTVarIO Map.empty

  mfix \ipc -> do
    recvThread <- forkIO $ forever $ recv socket ipc
    pure Client{socket, commands, callbacks, recvThread}

  where
    socket' = S.socket S.AF_UNIX S.Stream 0

    recv :: Socket -> Client -> IO ()
    recv socket ipc = do
      (msgId, payload) <- decodeSock socket decodeMessage
      recvMessage ipc msgId payload

encodeMessage :: MessageId -> BSL.ByteString -> Put
encodeMessage msgId payload = do
  BP.putStringUtf8 "i3-ipc"
  BP.putWord32host (fromIntegral (BSL.length payload))
  BP.putWord32host msgId
  BP.putLazyByteString payload

decodeMessage :: Get (MessageId, BSL.ByteString)
decodeMessage  = do
  magic <- BG.getByteString 6
  when (magic /= TE.encodeUtf8 "i3-ipc") $ fail "incorrect magic in header"

  len <- BG.getWord32host
  when (len > 5*1024*1024) $ fail "reply exceeds 5MiB limit"

  msgId <- BG.getWord32host

  payload <- BG.getLazyByteString $ fromIntegral len
  pure (msgId, payload)

data IpcException
  = ConcurrentCommand
  | OrphanCommand
  deriving Show

instance Exception IpcException

registerCommand :: Client -> CommandType -> STM (TMVar BSL.ByteString)
registerCommand ipc cmdType = do
  promise :: TMVar BSL.ByteString <- STM.newEmptyTMVar
  commands <- Map.lookup cmdType <$> STM.readTVar ipc.commands
  case commands of
    Nothing -> STM.modifyTVar ipc.commands $ Map.insert cmdType promise
    _ -> STM.throwSTM ConcurrentCommand
  pure promise

unregisterCommand :: Client -> CommandType -> STM ()
unregisterCommand ipc cmdType = do
  STM.modifyTVar ipc.commands $ Map.delete cmdType

registerCallback :: Client -> EventType -> EventCallback -> STM ()
registerCallback ipc eventType callback = do
  callbacks <- Map.findWithDefault [] eventType <$> STM.readTVar ipc.callbacks
  STM.modifyTVar ipc.callbacks $ Map.insert eventType (callback : callbacks)

recvCommandMessage :: Client -> CommandType -> BSL.ByteString -> STM ()
recvCommandMessage ipc cmdType payload = do
  command <- Map.lookup cmdType <$> STM.readTVar ipc.commands
  case command of
    Nothing -> STM.throw OrphanCommand
    Just x -> do
      STM.putTMVar x payload
      STM.modifyTVar ipc.commands $ Map.delete cmdType

recvEventMessage :: Client -> EventType -> BSL.ByteString -> IO ()
recvEventMessage ipc eventType payload = do
  callbacks :: [EventCallback] <- STM.atomically do
    Map.findWithDefault [] eventType <$> STM.readTVar ipc.callbacks

  val :: A.Value <- either fail pure $ A.eitherDecode payload
  forM_ callbacks $ \f -> f eventType val

recvMessage :: Client -> MessageId -> BSL.ByteString -> IO ()
recvMessage ipc msgId payload = do
  msgType <- idToMessageType msgId
  case msgType of
    Left x -> STM.atomically $ recvCommandMessage ipc x payload
    Right x -> recvEventMessage ipc x payload

runCommand :: Command a b => Client -> a -> IO b
runCommand ipc cmd = runCommand' ipc cmd $ const (pure ())

runCommand' :: forall a b. Command a b => Client -> a -> (b -> STM ()) -> IO b
runCommand' ipc cmd onResult = do
  let
    cmdType = getCommandType cmd
    message = BP.runPut $ encodeMessage (commandTypeToId cmdType) (encodePayload cmd)

  promise :: TMVar BSL.ByteString <- STM.atomically $ registerCommand ipc cmdType
  S.sendAll ipc.socket $ BSL.toStrict message

  reply :: Either String b <- STM.atomically $ handleCommandResult cmdType promise
  either fail pure reply

  where
    handleCommandResult :: CommandType -> TMVar BSL.ByteString -> STM (Either String b)
    handleCommandResult cmdType promise = do
      reply <- STM.readTMVar promise
      unregisterCommand ipc cmdType
      let decoded :: Either String b = decodePayload @a reply
      mapM_ onResult decoded
      pure decoded

subscribe :: Client -> [EventType] -> EventCallback -> IO ()
subscribe ipc eventTypes callback = do
  let
    onResult :: Either String A.Object -> STM ()
    onResult (Left _) = pure ()
    onResult (Right _) = do
      forM_ eventTypes $ \eventType -> registerCallback ipc eventType callback

  result <- runCommand' ipc (SubscribeCommand eventTypes) onResult
  either fail (pure . const ()) result

decodeSock :: Socket -> Get a -> IO a
decodeSock sock g = do
  feed $ BG.runGetIncremental g
  where
    feed :: BG.Decoder a -> IO a
    feed (BG.Done _leftovers _ x) = do
      pure x -- FIXME leftovers
    feed (BG.Fail _ _ str) = print @String "fail" *> fail str
    feed (BG.Partial k) = do
      chunk <- S.recv sock defaultChunkSize
      case BS.length chunk of
        0 -> feed $ k Nothing
        _ -> feed $ k (Just chunk)

