{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Domain where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Int (Int64)
import GHC.Generics

data Person = Person
  { pKey :: Int64
  , name :: Text
  , age :: Int64
  , address :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data CreatePersonReq = CreatePersonReq
  { name :: Text
  , age :: Int64
  , address :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)