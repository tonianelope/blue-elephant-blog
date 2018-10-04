{-# LANGUAGE OverloadedStrings #-}
module Main( main) where

import Data.Monoid
import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.IO.Class

data Post = Post {id::Int, title::String, body::String} deriving (Read, Show)

idFileName = "idfile"
blogFileName = "post"

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
      H.form ! A.method "post" $ do
        H.textarea ! A.name "title" $ "Blog post Title"
        H.br
        H.textarea ! A.name "body" $ "Blog post Body"
        H.br
        H.input ! A.type_ "submit" ! A.value "Submit post"

readPosts :: [Int] -> IO ([Post])
readPosts = mapM (\id -> fmap read $ readFile $ blogFileName <> show id)

blogHtml :: Post -> Html
blogHtml (Post id title body) =
  H.div $ do
    H.h1 $ H.toHtml title
    H.p $ H.toHtml body

header :: Html
header =
  H.div $ do
    H.a ! A.href "/" $ "home"
    H.a ! A.href "/" $ "home"
    H.a ! A.href "/" $ "home"

routes :: S.ScottyM()
routes = do
  S.get "/" $ do
    S.html . renderHtml $ do
      H.h1 "Blue Elephant!"

  S.get "/test" $ do
    S.html . renderHtml $ do
      H.h1 "Test"

  S.get "/posts" $ do
    id <- liftIO $ fmap read $ readFile idFileName
    posts <- liftIO $ readPosts [0..id]
    S.html . renderHtml $ do
      H.h1 "Posts"
      mapM_ blogHtml posts

  S.get "/new_post" $ do
    S.html . renderHtml $ do
      newPostHtml

  S.post "/new_post" $ do
    title <- S.param "title"
    body <- S.param "body"
    identifier <- liftIO $ fmap read $ readFile idFileName
    -- liftIO $ writeFile idFileName (show (identifier + 1))
    liftIO $ writeFile (blogFileName <> show identifier) $ show $ Post identifier title body
    S.html .renderHtml $ do
      H.h1 $ "Success!!! "
