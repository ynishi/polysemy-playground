{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Application where

import Colog.Core (Severity (..))
import Colog.Polysemy (Log, log)
import Control.Exception (bracket)
import Data.Text (Text)
import Database
import Database.Beam.Sqlite (SqliteM, runBeamSqlite)
import Database.SQLite.Simple (close, open)
import GHC.Int (Int64)
import Message (Message (..))
import Polysemy (
  Embed,
  Member,
  Members,
  Sem,
  embed,
  interpret,
  makeSem,
 )
import Polysemy.Error (Error, fromEither, throw)
import Polysemy.Reader (Reader, ask)
import Prelude hiding (log)

import Domain

-- This function is defined in Polysemy.Error for 1.3.0.0
-- however w/ 1.3.0.0 this code won't compile on my machine
note :: Member (Error e) r => e -> Maybe a -> Sem r a
note e Nothing = throw e
note _ (Just a) = pure a

data ApplicationEff m a where
  MakeTablesIfNotExists :: ApplicationEff m ()
  ListPersons :: Maybe Text -> Maybe Int64 -> Maybe Text -> ApplicationEff m [Person]
  CreatePerson :: CreatePersonReq -> ApplicationEff m Person
  ReadPerson :: Int64 -> ApplicationEff m Person
  UpdatePerson :: Person -> ApplicationEff m Person
  DestroyPerson :: Person -> ApplicationEff m ()

makeSem ''ApplicationEff

runQuery :: String -> SqliteM c -> IO c
runQuery s q = bracket (open s) close (`runBeamSqlite` q)

applicationEffToIO ::
  Members '[Embed IO, Reader String, Log Message, Error DbErr] r =>
  Sem (ApplicationEff ': r) a ->
  Sem r a
applicationEffToIO sem = do
  conn_s <- ask @String
  interpret
    ( \case
        MakeTablesIfNotExists -> do
          log $ Message Info "makeTableIfNotExist called"
          embed $ bracket (open conn_s) close Database.makeTablesIfNotExists
        ListPersons mName mAge mAddr -> do
          log $ Message Info "listPersons called"
          embed . runQuery conn_s $ Database.listPersons mName mAge mAddr
        CreatePerson pNoId -> do
          log $ Message Info "insertPerson called"
          (embed . runQuery conn_s $ Database.createPerson pNoId) >>= fromEither
        ReadPerson pKey -> do
          log $ Message Info "readPerson called"
          (embed . runQuery conn_s $ Database.readPerson pKey)
            >>= note (PersonIdDoesNotExist pKey)
        UpdatePerson person -> do
          let Person {..} = person
          log $ Message Info "updatePerson called"
          (embed . runQuery conn_s $ Database.updatePerson person)
            >>= note (PersonDoesNotExist name)
        DestroyPerson person -> do
          log $ Message Info "destroyPerson called"
          embed . runQuery conn_s $ Database.destroyPersonByName person
    )
    sem
