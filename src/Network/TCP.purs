module Network.TCP
  ( tcpClient
  , tlsClient
  , Connection(..)
  , Address(..)
  ) where

import Prelude

import Control.Coroutine (Consumer, Producer)
import Control.Coroutine (await, loop) as Co
import Control.Coroutine.Aff (close, emit, produce) as Co
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Data.ByteString as BS
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.AVar (AVar)
import Effect.Aff (Aff, launchAff_, makeAff, nonCanceler)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (throw)
import Node.Buffer as Buf
import Node.Net.Socket (Socket)
import Node.Net.Socket as Net
import Node.Net.Socket as Socket
import Node.TLS (TLSClientOptions)
import Node.TLS as TLS

newtype Address = Address
  { address :: String
  , port :: Int
  }

newtype Connection m a = Connection
  { source :: Producer a m Unit
  , sink :: Consumer (Maybe a) m Unit
  }

tlsClient :: Address -> TLSClientOptions -> Aff (Connection Aff ByteString)
tlsClient (Address { port, address }) clientOptions =
  fromSocket =<< TLS.connect port address clientOptions

tcpClient :: Address -> Aff (Connection Aff ByteString)
tcpClient (Address { port, address }) = do
  fromSocket =<< liftEffect (Net.createConnectionTCP port address (pure unit))

fromSocket :: Socket -> Aff (Connection Aff ByteString)
fromSocket sock = ado
  closed <- AVar.empty
  in Connection { source: socketProducer sock closed, sink: socketConsumer sock closed }

socketConsumer :: Socket -> AVar Boolean -> Consumer (Maybe ByteString) Aff Unit
socketConsumer sock closed = Co.loop do
  m <- Co.await
  case m of
    Nothing ->
      lift $ AVar.put true closed $> Just unit
    Just bs -> do
      buf <- liftEffect $ BS.flush bs
      lift $ makeAff \cb -> Socket.write sock buf (cb $ Right unit) $> nonCanceler
      pure Nothing

socketProducer :: Socket -> AVar Boolean -> Producer ByteString Aff Unit
socketProducer sock closed = Co.produce $ \emitter -> do
  Socket.onData sock (onData emitter)
  Socket.onTimeout sock onTimeout
  Socket.onError sock onError
  Socket.onEnd sock (onEnd emitter)
  launchAff_ $ do
    isClosed <- AVar.take closed
    when isClosed do
      empty <- liftEffect $ Buf.create 0
      liftEffect $ Socket.end sock empty (pure unit)
  where
    onData emitter (Left buf) =
      Co.emit emitter $ BS.unsafeFreeze buf
    onData _ (Right str) =
      throw "shouldn't get here"
    onTimeout = do
      Console.error "socket timeout"
      empty <- Buf.create 0
      Socket.end sock empty (pure unit)
    onError error =
      Console.error $ "socket error " <> show error
    onEnd emitter =
      Co.close emitter unit
