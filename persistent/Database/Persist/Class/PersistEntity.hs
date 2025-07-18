{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# language PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

module Database.Persist.Class.PersistEntity
    ( PersistEntity (..)
    , tabulateEntity
    , Update (..)
    , BackendSpecificUpdate
    , SelectOpt (..)
    , Filter (..)
    , FilterValue (..)
    , BackendSpecificFilter
    , Entity (.., Entity, entityKey, entityVal)
    , ViaPersistEntity (..)

    , recordName
    , entityValues
    , keyValueEntityToJSON, keyValueEntityFromJSON
    , entityIdToJSON, entityIdFromJSON
      -- * PersistField based on other typeclasses
    , toPersistValueJSON, fromPersistValueJSON
    , toPersistValueEnum, fromPersistValueEnum
      -- * Support for @OverloadedLabels@ with 'EntityField'
    , SymbolToField (..)
    , -- * Safety check for inserts
      SafeToInsert
    , SafeToInsertErrorMessage
    ) where

import Data.Functor.Constant
import Data.Functor.Apply (Apply)

import Data.Aeson
       ( FromJSON(..)
       , ToJSON(..)
       , Value(Object)
       , fromJSON
       , object
       , withObject
       , (.:)
       , (.=)
       )
import qualified Data.Aeson.Parser as AP
import Data.Aeson.Text (encodeToTextBuilder)
import Data.Aeson.Types (Parser, Result(Error, Success))
import Data.Attoparsec.ByteString (parseOnly)
import Data.Functor.Identity
import Web.PathPieces (PathMultiPiece(..), PathPiece(..))

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as AM
#else
import qualified Data.HashMap.Strict as AM
#endif

import GHC.Records
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB
import GHC.Generics
import GHC.OverloadedLabels
import GHC.TypeLits
import Data.Kind (Type)

import Database.Persist.Class.PersistField
import Database.Persist.Names
import Database.Persist.Types.Base

-- | Persistent serialized Haskell records to the database.
-- A Database 'Entity' (A row in SQL, a document in MongoDB, etc)
-- corresponds to a 'Key' plus a Haskell record.
--
-- For every Haskell record type stored in the database there is a
-- corresponding 'PersistEntity' instance. An instance of PersistEntity
-- contains meta-data for the record.  PersistEntity also helps abstract
-- over different record types. That way the same query interface can return
-- a 'PersistEntity', with each query returning different types of Haskell
-- records.
--
-- Some advanced type system capabilities are used to make this process
-- type-safe. Persistent users usually don't need to understand the class
-- associated data and functions.
class ( PersistField (Key record), ToJSON (Key record), FromJSON (Key record)
      , Show (Key record), Read (Key record), Eq (Key record), Ord (Key record))
  => PersistEntity record where
    -- | Persistent allows multiple different backends (databases).
    type PersistEntityBackend record

    -- | By default, a backend will automatically generate the key
    -- Instead you can specify a Primary key made up of unique values.
    data Key record
    -- | A lower-level key operation.
    keyToValues :: Key record -> [PersistValue]
    -- | A lower-level key operation.
    keyFromValues :: [PersistValue] -> Either Text (Key record)
    -- | A meta-operation to retrieve the 'Key' 'EntityField'.
    persistIdField :: EntityField record (Key record)

    -- | Retrieve the 'EntityDef' meta-data for the record.
    entityDef :: proxy record -> EntityDef

    -- | An 'EntityField' is parameterised by the Haskell record it belongs to
    -- and the additional type of that field.
    --
    -- As of @persistent-2.11.0.0@, it's possible to use the @OverloadedLabels@
    -- language extension to refer to 'EntityField' values polymorphically. See
    -- the documentation on 'SymbolToField' for more information.
    data EntityField record :: Type -> Type
    -- | Return meta-data for a given 'EntityField'.
    persistFieldDef :: EntityField record typ -> FieldDef
    -- | A meta-operation to get the database fields of a record.
    toPersistFields :: record -> [PersistValue]
    -- | A lower-level operation to convert from database values to a Haskell record.
    fromPersistValues :: [PersistValue] -> Either Text record

    -- | This function allows you to build an @'Entity' a@ by specifying an
    -- action that returns a value for the field in the callback function.
    -- Let's look at an example.
    --
    -- @
    -- parseFromEnvironmentVariables :: IO (Entity User)
    -- parseFromEnvironmentVariables =
    --     tabulateEntityA $ \\userField ->
    --         case userField of
    --             UserName ->
    --                 getEnv "USER_NAME"
    --             UserAge -> do
    --                 ageVar <- getEnv "USER_AGE"
    --                 case readMaybe ageVar of
    --                     Just age ->
    --                         pure age
    --                     Nothing ->
    --                         error $ "Failed to parse Age from: " <> ageVar
    --             UserAddressId -> do
    --                 addressVar <- getEnv "USER_ADDRESS_ID"
    --                 pure $ AddressKey addressVar
    -- @
    --
    -- @since 2.14.0.0
    tabulateEntityA
        :: Applicative f
        => (forall a. EntityField record a -> f a)
        -- ^ A function that builds a fragment of a record in an
        -- 'Applicative' context.
        -> f (Entity record)

    -- | Like 'tabulateEntityA', but works with any 'Apply' f. This works
    -- because all entities have at least one field, and so we can tabulate
    -- things into semigroup-like shapes instead.
    --
    -- @since 2.17.0.0
    tabulateEntityApply
        :: (Apply f)
        => (forall a. EntityField record a -> f a)
        -> f (Entity record)

    -- | Unique keys besides the 'Key'.
    data Unique record
    -- | A meta operation to retrieve all the 'Unique' keys.
    persistUniqueKeys :: record -> [Unique record]
    -- | A lower level operation.
    persistUniqueToFieldNames :: Unique record -> NonEmpty (FieldNameHS, FieldNameDB)
    -- | A lower level operation.
    persistUniqueToValues :: Unique record -> [PersistValue]

    -- | Use a 'PersistField' as a lens.
    fieldLens :: EntityField record field
              -> (forall f. Functor f => (field -> f field) -> Entity record -> f (Entity record))

    -- | Extract a @'Key' record@ from a @record@ value. Currently, this is
    -- only defined for entities using the @Primary@ syntax for
    -- natural/composite keys. In a future version of @persistent@ which
    -- incorporates the ID directly into the entity, this will always be Just.
    --
    -- @since 2.11.0.0
    keyFromRecordM :: Maybe (record -> Key record)
    keyFromRecordM = Nothing

-- | Newtype wrapper for optionally deriving typeclass instances on
-- 'PersistEntity' keys.
--
-- @since 2.14.6.0
newtype ViaPersistEntity record = ViaPersistEntity (Key record)

instance PersistEntity record => PathMultiPiece (ViaPersistEntity record) where
    fromPathMultiPiece pieces = do
        Right key <- keyFromValues <$> mapM fromPathPiece pieces
        pure $ ViaPersistEntity key
    toPathMultiPiece (ViaPersistEntity key) = map toPathPiece $ keyToValues key

-- | Construct an @'Entity' record@ by providing a value for each of the
-- record's fields.
--
-- These constructions are equivalent:
--
-- @
-- entityMattConstructor, entityMattTabulate :: Entity User
-- entityMattConstructor =
--     Entity
--         { entityKey = toSqlKey 123
--         , entityVal =
--             User
--                 { userName = "Matt"
--                 , userAge = 33
--                 }
--         }
--
-- entityMattTabulate =
--     tabulateEntity $ \\case
--         UserId ->
--             toSqlKey 123
--         UserName ->
--             "Matt"
--         UserAge ->
--             33
-- @
--
-- This is a specialization of 'tabulateEntityA', which allows you to
-- construct an 'Entity' by providing an 'Applicative' action for each
-- field instead of a regular function.
--
-- @since 2.14.0.0
tabulateEntity
    :: PersistEntity record
    => (forall a. EntityField record a -> a)
    -> Entity record
tabulateEntity fromField =
    runIdentity (tabulateEntityA (Identity . fromField))

type family BackendSpecificUpdate backend record

-- Moved over from Database.Persist.Class.PersistUnique
-- | Textual representation of the record
recordName
    :: (PersistEntity record)
    => record -> Text
recordName = unEntityNameHS . entityHaskell . entityDef . Just

-- | Updating a database entity.
--
-- Persistent users use combinators to create these.
data Update record = forall typ. PersistField typ => Update
    { updateField :: EntityField record typ
    , updateValue :: typ
    -- FIXME Replace with expr down the road
    , updateUpdate :: PersistUpdate
    }
    | BackendUpdate
          (BackendSpecificUpdate (PersistEntityBackend record) record)

-- | Query options.
--
-- Persistent users use these directly.
data SelectOpt record = forall typ. Asc  (EntityField record typ)
                      | forall typ. Desc (EntityField record typ)
                      | OffsetBy Int
                      | LimitTo Int

type family BackendSpecificFilter backend record

-- | Filters which are available for 'select', 'updateWhere' and
-- 'deleteWhere'. Each filter constructor specifies the field being
-- filtered on, the type of comparison applied (equals, not equals, etc)
-- and the argument for the comparison.
--
-- Persistent users use combinators to create these.
--
-- Note that it's important to be careful about the 'PersistFilter' that
-- you are using, if you use this directly. For example, using the 'In'
-- 'PersistFilter' requires that you have an array- or list-shaped
-- 'EntityField'. It is possible to construct values using this that will
-- create malformed runtime values.
data Filter record = forall typ. PersistField typ => Filter
    { filterField  :: EntityField record typ
    , filterValue  :: FilterValue typ
    , filterFilter :: PersistFilter -- FIXME
    }
    | FilterAnd [Filter record] -- ^ convenient for internal use, not needed for the API
    | FilterOr  [Filter record]
    | BackendFilter
          (BackendSpecificFilter (PersistEntityBackend record) record)

-- | Value to filter with. Highly dependant on the type of filter used.
--
-- @since 2.10.0
data FilterValue typ where
  FilterValue  :: typ -> FilterValue typ
  FilterValues :: [typ] -> FilterValue typ
  UnsafeValue  :: forall a typ. PersistField a => a -> FilterValue typ

-- | Datatype that represents an entity, with both its 'Key' and
-- its Haskell record representation.
--
-- When using a SQL-based backend (such as SQLite or
-- PostgreSQL), an 'Entity' may take any number of columns
-- depending on how many fields it has. In order to reconstruct
-- your entity on the Haskell side, @persistent@ needs all of
-- your entity columns and in the right order.  Note that you
-- don't need to worry about this when using @persistent@\'s API
-- since everything is handled correctly behind the scenes.
--
-- However, if you want to issue a raw SQL command that returns
-- an 'Entity', then you have to be careful with the column
-- order.  While you could use @SELECT Entity.* WHERE ...@ and
-- that would work most of the time, there are times when the
-- order of the columns on your database is different from the
-- order that @persistent@ expects (for example, if you add a new
-- field in the middle of you entity definition and then use the
-- migration code -- @persistent@ will expect the column to be in
-- the middle, but your DBMS will put it as the last column).
-- So, instead of using a query like the one above, you may use
-- 'Database.Persist.Sql.rawSql' (from the
-- "Database.Persist.Sql" module) with its /entity
-- selection placeholder/ (a double question mark @??@).  Using
-- @rawSql@ the query above must be written as @SELECT ??  WHERE
-- ..@.  Then @rawSql@ will replace @??@ with the list of all
-- columns that we need from your entity in the right order.  If
-- your query returns two entities (i.e. @(Entity backend a,
-- Entity backend b)@), then you must you use @SELECT ??, ??
-- WHERE ...@, and so on.
data Entity record =
    Entity
        { entityKey :: Key record
        , entityVal :: record
        }

deriving instance (Generic (Key record), Generic record) => Generic (Entity record)
deriving instance (Eq (Key record), Eq record) => Eq (Entity record)
deriving instance (Ord (Key record), Ord record) => Ord (Entity record)
deriving instance (Show (Key record), Show record) => Show (Entity record)
deriving instance (Read (Key record), Read record) => Read (Entity record)

-- | Get list of values corresponding to given entity.
entityValues :: PersistEntity record => Entity record -> [PersistValue]
entityValues (Entity k record) =
  if isJust (entityPrimary ent)
    then
      -- TODO: check against the key
      map toPersistValue (toPersistFields record)
    else
      keyToValues k ++ map toPersistValue (toPersistFields record)
  where
    ent = entityDef $ Just record

-- | Predefined @toJSON@. The resulting JSON looks like
-- @{"key": 1, "value": {"name": ...}}@.
--
-- The typical usage is:
--
-- @
-- instance ToJSON (Entity User) where
--     toJSON = keyValueEntityToJSON
-- @
keyValueEntityToJSON :: (PersistEntity record, ToJSON record)
                     => Entity record -> Value
keyValueEntityToJSON (Entity key value) = object
    [ "key" .= key
    , "value" .= value
    ]

-- | Predefined @parseJSON@. The input JSON looks like
-- @{"key": 1, "value": {"name": ...}}@.
--
-- The typical usage is:
--
-- @
-- instance FromJSON (Entity User) where
--     parseJSON = keyValueEntityFromJSON
-- @
keyValueEntityFromJSON :: (PersistEntity record, FromJSON record)
                       => Value -> Parser (Entity record)
keyValueEntityFromJSON (Object o) = Entity
    <$> o .: "key"
    <*> o .: "value"
keyValueEntityFromJSON _ = fail "keyValueEntityFromJSON: not an object"

-- | Predefined @toJSON@. The resulting JSON looks like
-- @{"id": 1, "name": ...}@.
--
-- The typical usage is:
--
-- @
-- instance ToJSON (Entity User) where
--     toJSON = entityIdToJSON
-- @
entityIdToJSON :: (PersistEntity record, ToJSON record) => Entity record -> Value
entityIdToJSON (Entity key value) = case toJSON value of
        Object o -> Object $ AM.insert "id" (toJSON key) o
        x -> x

-- | Predefined @parseJSON@. The input JSON looks like
-- @{"id": 1, "name": ...}@.
--
-- The typical usage is:
--
-- @
-- instance FromJSON (Entity User) where
--     parseJSON = entityIdFromJSON
-- @
entityIdFromJSON :: (PersistEntity record, FromJSON record) => Value -> Parser (Entity record)
entityIdFromJSON = withObject "entityIdFromJSON" $ \o -> do
    val <- parseJSON (Object o)
    k <- case keyFromRecordM of
        Nothing ->
            o .: "id"
        Just func ->
            pure $ func val
    pure $ Entity k val

instance (PersistEntity record, PersistField record, PersistField (Key record))
  => PersistField (Entity record) where
    toPersistValue (Entity key value) = case toPersistValue value of
        (PersistMap alist) -> PersistMap ((idField, toPersistValue key) : alist)
        _ -> error $ T.unpack $ errMsg "expected PersistMap"

    fromPersistValue (PersistMap alist) = case after of
        [] -> Left $ errMsg $ "did not find " `mappend` idField `mappend` " field"
        ("_id", kv):afterRest ->
            fromPersistValue (PersistMap (before ++ afterRest)) >>= \record ->
                keyFromValues [kv] >>= \k ->
                    Right (Entity k record)
        _ -> Left $ errMsg $ "impossible id field: " `mappend` T.pack (show alist)
      where
        (before, after) = break ((== idField) . fst) alist

    fromPersistValue x = Left $
          errMsg "Expected PersistMap, received: " `mappend` T.pack (show x)

errMsg :: Text -> Text
errMsg = mappend "PersistField entity fromPersistValue: "

-- | Realistically this is only going to be used for MongoDB,
-- so lets use MongoDB conventions
idField :: Text
idField = "_id"

-- | Convenience function for getting a free 'PersistField' instance
-- from a type with JSON instances.
--
--
-- Example usage in combination with 'fromPersistValueJSON':
--
-- @
-- instance PersistField MyData where
--   fromPersistValue = fromPersistValueJSON
--   toPersistValue = toPersistValueJSON
-- @
toPersistValueJSON :: ToJSON a => a -> PersistValue
toPersistValueJSON = PersistText . LT.toStrict . TB.toLazyText . encodeToTextBuilder . toJSON

-- | Convenience function for getting a free 'PersistField' instance
-- from a type with JSON instances. The JSON parser used will accept JSON
-- values other that object and arrays. So, if your instance serializes the
-- data to a JSON string, this will still work.
--
--
-- Example usage in combination with 'toPersistValueJSON':
--
-- @
-- instance PersistField MyData where
--   fromPersistValue = fromPersistValueJSON
--   toPersistValue = toPersistValueJSON
-- @
fromPersistValueJSON :: FromJSON a => PersistValue -> Either Text a
fromPersistValueJSON z = case z of
  PersistByteString bs -> mapLeft (T.append "Could not parse the JSON (was a PersistByteString): ")
                        $ parseGo bs
  PersistText t -> mapLeft (T.append "Could not parse the JSON (was PersistText): ")
                 $ parseGo (TE.encodeUtf8 t)
  a -> Left $ T.append "Expected PersistByteString, received: " (T.pack (show a))
  where parseGo bs = mapLeft T.pack $ case parseOnly AP.value bs of
          Left err -> Left err
          Right v -> case fromJSON v of
            Error err -> Left err
            Success a -> Right a
        mapLeft _ (Right a) = Right a
        mapLeft f (Left b)  = Left (f b)

-- | Convenience function for getting a free 'PersistField' instance
-- from a type with an 'Enum' instance. The function 'derivePersistField'
-- from the persistent-template package should generally be preferred.
-- However, if you want to ensure that an @ORDER BY@ clause that uses
-- your field will order rows by the data constructor order, this is
-- a better choice.
--
-- Example usage in combination with 'fromPersistValueEnum':
--
-- @
-- data SeverityLevel = Low | Medium | Critical | High
--   deriving (Enum, Bounded)
-- instance PersistField SeverityLevel where
--   fromPersistValue = fromPersistValueEnum
--   toPersistValue = toPersistValueEnum
-- @
toPersistValueEnum :: Enum a => a -> PersistValue
toPersistValueEnum = toPersistValue . fromEnum

-- | Convenience function for getting a free 'PersistField' instance
-- from a type with an 'Enum' instance. This function also requires
-- a `Bounded` instance to improve the reporting of errors.
--
-- Example usage in combination with 'toPersistValueEnum':
--
-- @
-- data SeverityLevel = Low | Medium | Critical | High
--   deriving (Enum, Bounded)
-- instance PersistField SeverityLevel where
--   fromPersistValue = fromPersistValueEnum
--   toPersistValue = toPersistValueEnum
-- @
fromPersistValueEnum :: (Enum a, Bounded a) => PersistValue -> Either Text a
fromPersistValueEnum v = fromPersistValue v >>= go
  where go i = let res = toEnum i in
               if i >= fromEnum (asTypeOf minBound res) && i <= fromEnum (asTypeOf maxBound res)
                 then Right res
                 else Left ("The number " `mappend` T.pack (show i) `mappend` " was out of the "
                  `mappend` "allowed bounds for an enum type")

-- | This type class is used with the @OverloadedLabels@ extension to
-- provide a more convenient means of using the 'EntityField' type.
-- 'EntityField' definitions are prefixed with the type name to avoid
-- ambiguity, but this ambiguity can result in verbose code.
--
-- If you have a table @User@ with a @name Text@ field, then the
-- corresponding 'EntityField' is @UserName@. With this, we can write
-- @#name :: 'EntityField' User Text@.
--
-- What's more fun is that the type is more general: it's actually
-- @
-- #name
--     :: ('SymbolToField' "name" rec typ)
--     => EntityField rec typ
-- @
--
-- Which means it is *polymorphic* over the actual record. This allows you
-- to write code that can be generic over the tables, provided they have
-- the right fields.
--
-- @since 2.11.0.0
class SymbolToField (sym :: Symbol) rec typ | sym rec -> typ where
    symbolToField :: EntityField rec typ

-- | This instance delegates to 'SymbolToField' to provide
-- @OverloadedLabels@ support to the 'EntityField' type.
--
-- @since 2.11.0.0
instance SymbolToField sym rec typ => IsLabel sym (EntityField rec typ) where
    fromLabel = symbolToField @sym

-- | A type class which is used to witness that a type is safe to insert into
-- the database without providing a primary key.
--
-- The @TemplateHaskell@ function 'mkPersist' will generate instances of this
-- class for any entity that it works on. If the entity has a default primary
-- key, then it provides a regular instance. If the entity has a @Primary@
-- natural key, then this works fine. But if the entity has an @Id@ column with
-- no @default=@, then this does a 'TypeError' and forces the user to use
-- 'insertKey'.
--
-- @since 2.14.0.0
class SafeToInsert a where

type SafeToInsertErrorMessage a
    = 'Text "The PersistEntity " ':<>: ShowType a ':<>: 'Text " does not have a default primary key."
    ':$$: 'Text "This means that 'insert' will fail with a database error."
    ':$$: 'Text "Please  provide a default= clause inthe entity definition,"
    ':$$: 'Text "or use 'insertKey' instead to provide one."

instance (TypeError (FunctionErrorMessage a b)) => SafeToInsert (a -> b)

type FunctionErrorMessage a b =
    'Text "Uh oh! It looks like you are trying to insert a function into the database."
    ':$$: 'Text "Argument: " ':<>: 'ShowType a
    ':$$: 'Text "Result:   " ':<>: 'ShowType b
    ':$$: 'Text "You probably need to add more arguments to an Entity construction."

type EntityErrorMessage a =
    'Text "It looks like you're trying to `insert` an `Entity " ':<>: 'ShowType a ':<>: 'Text "` directly."
    ':$$: 'Text "You want `insertKey` instead. As an example:"
    ':$$: 'Text "    insertKey (entityKey ent) (entityVal ent)"

instance TypeError (EntityErrorMessage a) => SafeToInsert (Entity a)
