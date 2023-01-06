{-# OPTIONS_GHC -Wno-orphans #-}
-- TODO: remove -Wno
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Butler.Prelude (
    die,
    showT,

    -- * unliftio
    module UnliftIO,
    module UnliftIO.STM,
    Control.Concurrent.STM.stateTVar,
    Control.Concurrent.STM.check,
    module Control.Monad.Morph,

    -- * mtl
    module Control.Monad.Reader,

    -- * base extra
    whenM,
    module GHC.Stack,
    module Data.Char,
    System.Posix.ByteString.RawFilePath,

    -- * relude
    Relude.putText,
    Relude.putTextLn,
    Relude.putBS,
    Relude.putBSLn,

    -- * PyF
    module PyF,

    -- * lucid
    Lucid.ToHtml (..),
    Lucid.Html,
    Lucid.HtmlT,

    -- * lens
    (%~),
    (.~),
    (^?),
    Control.Lens.set,
    Control.Lens.over,

    -- * lens-aeson
    Data.Aeson.Lens.key,
    Data.Aeson.Lens._String,
    Data.Aeson.Lens._Integer,
    Data.Aeson.Lens._Bool,
    Data.Aeson.Lens._JSON,

    -- * uuid
    Data.UUID.UUID,

    -- * re-exports from base
    bool,
    module Debug.Trace,
    module Data.Coerce,
    module Data.Int,
    module Data.Word,
    module Foreign.C.Types,
    Data.Foldable.traverse_,
    Data.Void.Void,
    module Control.Applicative,
    module Data.Bifunctor,
    module Data.Functor.Identity,
    module Data.Dynamic,
    Data.Proxy.Proxy (..),
    module Data.Maybe,
    module Data.Either,
    module Data.Monoid,
    GHC.Generics.Generic,
    GHC.Records.HasField (..),
    module Control.Monad,
    Numeric.Natural.Natural,
    Control.Concurrent.ThreadId,
    Control.Concurrent.myThreadId,
    Control.Concurrent.threadDelay,
    Text.Read.readMaybe,
    Data.String.IsString,
    (&),
    Data.Foldable.toList,

    -- * text/bytestring/containers
    Data.Text.Text,
    LByteString,
    Data.ByteString.ByteString,
    Data.IntMap.Strict.IntMap,
    Data.IntSet.IntSet,
    Data.Set.Set,
    Data.Map.Strict.Map,
    Warp.Port,
    module Data.Text.Encoding,
    module Data.Text.Encoding.Error,

    -- * servant
    Servant.API.FromHttpApiData (..),
    Servant.API.ToHttpApiData (..),

    -- * ki
    module Ki,

    -- * serialise
    Codec.Serialise.Serialise (..),
    Codec.Serialise.serialise,
    Codec.Serialise.deserialise,
    Codec.Serialise.deserialiseOrFail,

    -- * time
    Data.Time.Clock.UTCTime,
    Data.Time.Clock.getCurrentTime,

    -- * cli parser
    Options.Applicative.Types.ParserInfo,

    -- * stm

    -- module Control.Concurrent.STM,

    -- * aeson
    (.=),
    Data.Aeson.Value (Object, String),
    Data.Aeson.decode',
    Data.Aeson.FromJSON,
    Data.Aeson.fromJSON,
    Data.Aeson.ToJSON,
    Data.Aeson.toJSON,
    Data.Aeson.Result (..),
    encodeJSON,
    Data.Aeson.object,

    -- * xstatic
    module XStatic,

    -- * Convertion
    Witch.From,
    Witch.from,
    Witch.tryFrom,
    Witch.into,
    Witch.via,
    Witch.unsafeFrom,
) where

import Codec.Serialise qualified
import Control.Applicative
import Control.Concurrent qualified
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception hiding (Handler)
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Fix qualified
import Control.Monad.IO.Class qualified
import Control.Monad.Morph (generalize, hoist)
import Control.Monad.Reader
import Control.Monad.State qualified
import Data.Aeson ((.=))
import Data.Aeson qualified
import Data.Aeson.Lens qualified
import Data.Bifunctor
import Data.Bool (bool)
import Data.ByteString qualified
import Data.ByteString.Lazy qualified
import Data.Char
import Data.Coerce
import Data.Dynamic
import Data.Either
import Data.Foldable qualified
import Data.Functor
import Data.Functor.Identity qualified
import Data.Generics.Labels ()
import Data.Int
import Data.IntMap.Strict qualified
import Data.IntSet qualified
import Data.Kind qualified
import Data.Map qualified
import Data.Map.Strict qualified
import Data.Maybe
import Data.Monoid
import Data.Proxy qualified
import Data.Set qualified
import Data.String qualified
import Data.Text qualified
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Time.Clock qualified
import Data.UUID qualified
import Data.Void qualified
import Data.Word
import Debug.Trace
import Foreign.C.Types
import GHC.Generics qualified
import GHC.Records qualified
import GHC.Stack
import GHC.TypeLits qualified
import Ki
import Lucid
import Network.Wai.Handler.Warp qualified as Warp
import Numeric.Natural qualified
import Options.Applicative.Types qualified
import PyF
import Relude qualified
import Servant.API
import System.Posix.ByteString (RawFilePath)
import Text.Read (readMaybe)
import UnliftIO hiding (Handler)
import UnliftIO.STM
import Witch qualified
import XStatic

type LByteString = Data.ByteString.Lazy.ByteString

whenM :: Monad m => m Bool -> m () -> m ()
whenM getCondition action = do
    condition <- getCondition
    Control.Monad.when condition action

instance Witch.From CTime Int64 where
    from (CTime c) = c

encodeJSON :: Data.Aeson.ToJSON a => a -> LByteString
encodeJSON = Data.Aeson.encode

die :: Relude.HasCallStack => MonadIO m => Relude.Text -> m ()
die = error . Data.Text.unpack

showT :: Show a => a -> Data.Text.Text
showT = Witch.from . show
