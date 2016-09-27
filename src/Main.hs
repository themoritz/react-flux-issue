{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import           Control.Concurrent (forkIO)
import           Control.DeepSeq    (NFData)

import           Data.Typeable      (Typeable)
import           GHC.Generics       (Generic)
import           React.Flux

store :: ReactStore String
store = mkStore "initial"

data Action
  = SetString String
  | SetString2 String
  deriving (Typeable, Generic, NFData)

instance StoreData String where
  type StoreAction String = Action
  transform (SetString str) st = do
    forkIO $ alterStore store $ SetString2 "2"
    pure $ st ++ str
  transform (SetString2 str) _ = pure str

app :: ReactView ()
app = defineControllerView "app" store $ \st () -> do
  button_ [ onClick $ \_ _ -> [SomeStoreAction store (SetString "1")] ] "Go"
  elemString st

main :: IO ()
main = reactRender "app" app ()
