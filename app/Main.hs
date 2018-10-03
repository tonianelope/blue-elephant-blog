{-# LANGUAGE OverloadedStrings #-}
module Main( main) where

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
      H.title "new post"
    H.body $ do
      H.h1 "New post"
      H.form $ do
        H.textarea ! A.name "title" $ "Blog post Title"
        H.br
        H.textarea ! A.name "body" $ "Blog post Body"
        H.br
        H.input ! A.type_ "submit" ! A.value "Submit post"

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
