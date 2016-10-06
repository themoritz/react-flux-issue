{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import           Control.DeepSeq (NFData)

import           Data.Typeable   (Typeable)
import           GHC.Generics    (Generic)
import           React.Flux

store :: ReactStore String
store = mkStore "initial"

data Action
  = SetString String
  | SetString2 String
  deriving (Typeable, Generic, NFData)

type State = String

setString :: State -> String -> IO State
setString st str =
  pure $ st ++ str

setString2 :: State -> String -> IO State
setString2 st str =
  pure $ st ++ str

instance StoreData String where
  type StoreAction String = Action
  transform (SetString str) st = do
    st' <- setString st str
    setString2 st' "2"
  transform (SetString2 str) st = setString2 st str

app :: ReactView ()
app = defineControllerView "app" store $ \st () -> do
  button_ [ onClick $ \_ _ -> [SomeStoreAction store (SetString "1")] ] "Go"
  elemString st

main :: IO ()
main = reactRender "app" app ()
