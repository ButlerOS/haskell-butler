module Butler.User where

import Butler.Prelude
import Data.Hashable (Hashable (hash))
import Lucid

newtype UserName = UserName Text
    deriving newtype (Ord, Eq, IsString, Show, Serialise)
    deriving (FromJSON, ToJSON) via Text
    deriving (Generic)

instance ToHtml UserName where
    toHtml (UserName n) = toHtml n

instance FromHttpApiData UserName where
    parseUrlPiece txt = pure $ UserName txt

instance From UserName Text where
    from (UserName n) = n

-- | A TabID represent a single client window.
newtype TabID = TabID Natural
    deriving (Ord, Eq, Generic, Show)
    deriving newtype (Num, ToJSON, FromJSON, FromHttpApiData, Serialise, Hashable)

instance From TabID Text where
    from (TabID tabID) = from (show tabID)

userColor :: UserName -> Text
userColor (UserName n) =
    let k = hash n
     in "hsl(" <> from (show $ k `mod` 300) <> ", 60%, 50%)"

userColorStyle :: UserName -> Attribute
userColorStyle user = style_ $ "color : " <> userColor user

userIconTitle :: Monad m => UserName -> Text -> HtmlT m ()
userIconTitle user title =
    with
        i_
        [ class_ "ri-user-fill p-0.5"
        , userColorStyle user
        , title_ title
        ]
        mempty

userIcon :: Monad m => UserName -> HtmlT m ()
userIcon user = userIconTitle user (from user)

userTabIcon :: Monad m => UserName -> TabID -> HtmlT m ()
userTabIcon user tab = userIconTitle user (from user <> "<" <> from tab <> ">")
