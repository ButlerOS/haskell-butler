-- | This module contains logic around sqlite-easy
module Butler.Database (
    Database,
    withDatabase,
    DatabaseMigration (..),
    dbSimpleCreate,
    dbExecute,
    dbQuery,
    dbInsert,
    dbUpdate,

    -- * newtypes
    UUIDB,
    ValueB,

    -- * re-export
    SQLData (..),
    FromField (..),
    ToField (..),
    NamedParam ((:=)),
    Only (..),
)
where

import Data.UUID qualified as UUID
import Database.Migrant hiding (withTransaction)
import Database.Migrant.Driver.Sqlite ()
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField (FromField (..), fieldData)
import Database.SQLite.Simple.ToField (ToField (..))

import Butler.Core
import Butler.Core.Storage
import Butler.Prelude

newtype Database = Database (MVar Connection)

withDatabase :: StorageAddress -> DatabaseMigration -> (Database -> ProcessIO a) -> ProcessIO a
withDatabase addr migrations cb = do
    fp <-
        if addr == ":memory:"
            then pure ":memory:"
            else flip mappend ".sqlite" . into @FilePath . decodeUtf8 <$> getPath addr
    withRunInIO \runInIO ->
        withConnection fp \conn -> runInIO do
            db <- Database <$> newMVar conn
            dbSetup db migrations
            cb db

data DatabaseMigration = DatabaseMigration
    { migrations :: [MigrationName]
    , migrateUp :: MigrationName -> Database -> ProcessIO ()
    , migrateDown :: MigrationName -> Database -> ProcessIO ()
    }

dbSimpleCreate :: Text -> Text -> DatabaseMigration
dbSimpleCreate tableName fields = DatabaseMigration [migrationName] doUp doDown
  where
    migrationName = fromString (into @String tableName <> "-create")
    doUp name db = do
        when (name /= migrationName) (error $ "Invalid name!?: " <> show name)
        dbExecute db (Query $ "CREATE TABLE " <> tableName <> "(" <> fields <> ")") []
    doDown name db = do
        when (name /= migrationName) (error $ "Invalid name!?: " <> show name)
        dbExecute db (Query $ "DROP TABLE " <> tableName) []

dbSetup :: Database -> DatabaseMigration -> ProcessIO ()
dbSetup (Database mvConn) databaseSetup = withMVar mvConn \conn ->
    withRunInIO \runInIO ->
        migrate
            databaseSetup.migrations
            (\m d -> runInIO (databaseSetup.migrateUp m . Database =<< newMVar d))
            (\m d -> runInIO (databaseSetup.migrateDown m . Database =<< newMVar d))
            conn

dbExecute :: MonadUnliftIO m => Database -> Query -> [NamedParam] -> m ()
dbExecute (Database mvConn) q args = withMVar mvConn \conn -> liftIO (executeNamed conn q args)

-- | 'dbUpdate' returns the number of updated row: http://www.sqlite.org/c3ref/changes.html
dbUpdate :: MonadUnliftIO m => Database -> Query -> [NamedParam] -> m Int
dbUpdate (Database mvConn) q args = withMVar mvConn \conn -> liftIO do
    executeNamed conn q args
    changes conn

-- | 'dbInsert' returns the ROWID of the INSERT: http://www.sqlite.org/c3ref/last_insert_rowid.html
dbInsert :: MonadUnliftIO m => Database -> Query -> [NamedParam] -> m Int64
dbInsert (Database mvConn) q args = withMVar mvConn \conn -> liftIO do
    executeNamed conn q args
    lastInsertRowId conn

dbQuery :: (MonadUnliftIO m, FromRow r) => Database -> Query -> [NamedParam] -> m [r]
dbQuery (Database mvConn) q args = withMVar mvConn \conn -> liftIO (queryNamed conn q args)

-- | A storable 'UUID'
newtype UUIDB = UUIDB UUID

instance From UUIDB UUID where from = coerce
instance From UUID UUIDB where from = coerce
instance ToField UUIDB where toField = SQLText . UUID.toText . from

instance FromField UUIDB where
    fromField f = case fieldData f of
        SQLText txt -> case UUID.fromText txt of
            Just uuid -> pure (UUIDB uuid)
            Nothing -> fail ("Invalid uuid: " <> show txt)
        _ -> fail "Invalid uuid type"

-- | A storable 'Value'
newtype ValueB = ValueB Value

instance From ValueB Value where from = coerce
instance From Value ValueB where from = coerce
instance ToField ValueB where toField = SQLBlob . from . encodeJSON . into @Value

instance FromField ValueB where
    fromField f = case fieldData f of
        SQLBlob buf -> case decodeJSON (from buf) of
            Just v -> pure (ValueB v)
            Nothing -> fail ("Invalid json: " <> show buf)
        _ -> fail "Invalid json type"
