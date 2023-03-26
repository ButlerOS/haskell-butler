{-# OPTIONS_GHC -Wno-orphans #-}

module Butler.Prelude (
    -- * Custom helpers
    die,
    showT,
    putTextLn,
    whenM,
    unlessM,
    ignoringExceptions,
    sktRecv,
    sktSendAll,

    -- * ki
    module Ki.Unlifted,

    -- * unliftio
    module UnliftIO,
    module UnliftIO.STM,
    Control.Concurrent.STM.stateTVar,
    Control.Concurrent.STM.check,

    -- * PyF
    module PyF,

    -- * lucid
    module Lucid,
    module Lucid.Htmx,
    Lucid.Base.makeAttribute,

    -- * optparse-applicative
    Options.Applicative.Types.ParserInfo,

    -- * aeson
    (.=),
    (.:?),
    (.:),
    Data.Aeson.Value (Null, Object, String),
    Data.Aeson.Types.Pair,
    Data.Aeson.decode',
    Data.Aeson.FromJSON (parseJSON),
    Data.Aeson.FromJSONKey,
    Data.Aeson.fromJSON,
    Data.Aeson.ToJSON,
    Data.Aeson.ToJSONKey,
    Data.Aeson.toJSON,
    Data.Aeson.withText,
    Data.Aeson.withObject,
    Data.Aeson.Result (..),
    encodeJSON,
    decodeJSON,
    Data.Aeson.object,

    -- * xstatic
    module XStatic,

    -- * witch
    Witch.TryFrom,
    Witch.TryFromException (..),
    Witch.Utility.maybeTryFrom,
    Witch.From,
    Witch.from,
    Witch.tryFrom,
    Witch.into,
    Witch.via,
    Witch.unsafeFrom,

    -- * lens
    (%~),
    (.~),
    (^?),
    Control.Lens.ix,
    Control.Lens.set,
    Control.Lens.over,

    -- * lens-aeson
    Data.Aeson.Lens.key,
    Data.Aeson.Lens._String,
    Data.Aeson.Lens._Integer,
    Data.Aeson.Lens._Integral,
    Data.Aeson.Lens._Bool,
    Data.Aeson.Lens._JSON,

    -- * uuid
    Data.UUID.UUID,

    -- * servant
    Servant.API.FromHttpApiData (..),
    Servant.API.ToHttpApiData (..),

    -- * serialise
    Codec.Serialise.Serialise (..),
    Codec.Serialise.serialise,
    Codec.Serialise.deserialise,
    Codec.Serialise.deserialiseOrFail,

    -- * time
    Data.Time.Clock.UTCTime,
    Data.Time.Clock.getCurrentTime,

    -- * network
    Network.Socket.Socket,

    -- * text bytestring containers
    Data.Sequence.Seq,
    Data.Text.Text,
    LByteString,
    Data.ByteString.ByteString,
    Data.ByteString.Short.ShortByteString,
    Data.IntMap.Strict.IntMap,
    Data.IntSet.IntSet,
    Data.Set.Set,
    Data.Map.Strict.Map,
    Data.HashMap.Strict.HashMap,
    Warp.Port,
    Data.Hashable.Hashable,
    module Data.Text.Encoding,
    module Data.Text.Encoding.Error,

    -- * base extra
    System.Posix.ByteString.RawFilePath,
    GHC.Exts.fromList,
    bool,
    Data.Foldable.traverse_,
    module Control.Applicative,
    module Control.Monad,
    module Control.Monad.Reader,
    module Data.Bifunctor,
    module Data.Char,
    module Data.Coerce,
    module Data.Dynamic,
    module Data.Either,
    module Data.Functor.Identity,
    module Data.Int,
    module Data.Maybe,
    module Data.Monoid,
    module Data.Proxy,
    module Data.Void,
    module Data.Word,
    module Debug.Trace,
    module Foreign.C.Types,
    module GHC.Stack,
    GHC.Generics.Generic,
    GHC.Records.HasField (..),
    Numeric.Natural.Natural,
    Control.Concurrent.ThreadId,
    Control.Concurrent.myThreadId,
    Control.Concurrent.threadDelay,
    Text.Read.readMaybe,
    Data.String.IsString (..),
    (&),
    Data.Foldable.toList,
) where

import Codec.Serialise qualified
import Control.Applicative
import Control.Concurrent qualified
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception hiding (Handler)
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Lens qualified
import Data.Aeson.Types qualified
import Data.Bifunctor
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified
import Data.ByteString.Short qualified
import Data.Char
import Data.Coerce
import Data.Dynamic
import Data.Either
import Data.Foldable qualified
import Data.Functor.Identity qualified
import Data.Generics.Labels ()
import Data.HashMap.Strict qualified
import Data.Hashable qualified
import Data.Int
import Data.IntMap.Strict qualified
import Data.IntSet qualified
import Data.Map.Strict qualified
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.Sequence
import Data.Set qualified
import Data.String qualified
import Data.Text qualified
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Text.IO qualified
import Data.Text.Rope
import Data.Time.Clock qualified
import Data.UUID qualified
import Data.Void
import Data.Word
import Debug.Trace
import Foreign.C.Types
import GHC.Exts (fromList)
import GHC.Generics qualified
import GHC.Records qualified
import GHC.Stack
import Ki.Unlifted
import Lucid
import Lucid.Base (makeAttribute)
import Lucid.Htmx
import Network.Socket
import Network.Socket.ByteString qualified
import Network.Wai.Handler.Warp qualified as Warp
import Network.WebSockets qualified as WS
import Numeric.Natural qualified
import Options.Applicative.Types qualified
import PyF
import Servant.API
import System.Posix
import System.Posix.ByteString (RawFilePath)
import System.Process.Typed qualified as ProcessTyped
import Text.Read (readMaybe)
import UnliftIO
import UnliftIO.STM
import Witch qualified
import Witch.Utility qualified
import XStatic

type LByteString = Data.ByteString.Lazy.ByteString

whenM :: Monad m => m Bool -> m () -> m ()
whenM getCondition action = do
    condition <- getCondition
    Control.Monad.when condition action

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM getCondition action = do
    condition <- getCondition
    Control.Monad.unless condition action

instance Witch.From CTime Int64 where
    from (CTime c) = c

putTextLn :: MonadIO m => Data.Text.Text -> m ()
putTextLn = liftIO . Data.Text.IO.putStrLn

encodeJSON :: Data.Aeson.ToJSON a => a -> LByteString
encodeJSON = Data.Aeson.encode

decodeJSON :: Data.Aeson.FromJSON a => LByteString -> Maybe a
decodeJSON = Data.Aeson.decode

die :: HasCallStack => MonadIO m => Data.Text.Text -> m ()
die = error . Data.Text.unpack

showT :: Show a => a -> Data.Text.Text
showT = Witch.from . show

instance Data.Aeson.ToJSON ProcessTyped.ExitCode

instance Witch.From WS.DataMessage LByteString where
    from = \case
        WS.Binary lbs -> lbs
        WS.Text lbs _ -> lbs

instance Witch.From Numeric.Natural.Natural System.Posix.COff where
    from = COff . Witch.unsafeFrom

instance Witch.From System.Posix.COff Data.Text.Text where
    from (COff v) = Witch.from (show v)

-- | Run an IO action, ignoring synchronous exceptions
ignoringExceptions :: MonadUnliftIO m => m () -> m ()
ignoringExceptions action =
    action `catchAny` \_ -> pure ()

sktRecv :: MonadIO m => Network.Socket.Socket -> Int -> m Data.ByteString.ByteString
sktRecv skt = liftIO . Network.Socket.ByteString.recv skt

sktSendAll :: MonadIO m => Network.Socket.Socket -> Data.ByteString.ByteString -> m ()
sktSendAll skt = liftIO . Network.Socket.ByteString.sendAll skt

instance ToJSON Rope where
    toJSON = toJSON . Data.Text.Rope.toText
