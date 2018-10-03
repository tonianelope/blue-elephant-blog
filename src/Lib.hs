{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( main
    ) where

import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html
import qualified Data.Text as T

data Post = Post {id::Int, title::String, body::String} deriving (Read, Show)

main :: IO ()
main = do
  putStrLn "Starting Server..."
  S.scotty 3000 routes

newPostHtml :: Html
newPostHtml =
  H.html $ do
    H.head $ do
      H.title greet
    H.body $ do
      H.p greet
  where
    greet = H.toHtml ("Hello, " :: T.Text)

routes :: S.ScottyM()
routes = do
  S.get "/" $ do
    S.html . renderHtml $ do
      H.h1 "Blue Elephant!"

  S.get "/test" $ do
    S.html . renderHtml $ do
      H.h1 "Test"

  S.get "/new_post" $ do
    S.html . renderHtml $ do
      newPostHtml
