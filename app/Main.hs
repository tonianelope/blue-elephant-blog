{-# LANGUAGE OverloadedStrings #-}
module Main( main) where

import Control.Monad.IO.Class
import Control.Applicative
import Data.Hashable
import Data.List
import Data.Monoid
import Data.Ord
import Data.String
import Data.Text (unpack, pack, Text)
import Data.Time
import Data.Time.Format
import System.Directory
import System.FilePath
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Web.Scotty as S

data Page = Home | AllPosts | NewPost | OnePost | LogIn deriving Eq
data PageConfig = PageConfig Page Text Text
data Post = Post
  { date  :: UTCTime
  , title :: String
  , body  :: String
  } deriving (Read, Show)

homePage        = PageConfig Home "/" "Home"
allPostsPage    = PageConfig AllPosts "/posts" "All Posts"
newPostPage     = PageConfig NewPost "/new-post" "New Post"
postPage title  = PageConfig OnePost "/posts/:postID" title
loginPage       = PageConfig LogIn "/login" "Log In"

pages = [ homePage
        , allPostsPage
        , newPostPage
        , loginPage
        ]

postDir = "posts"

main :: IO ()
main = do
  -- TODO add css
  createDirectory postDir <|> pure ()
  putStrLn "Starting Server..."
  S.scotty 3000 routes

pagePath :: PageConfig -> S.RoutePattern
pagePath (PageConfig _ a _) = S.capture $ unpack a

mkPage :: PageConfig -> Html -> S.ActionM ()
mkPage (PageConfig page _ title) body =
  S.html . renderHtml $ do
    H.docType
    H.html $ do
      H.head $ do
        H.title $ toHtml title
        -- TODO link style sheet
        H.link
          ! A.rel "stylesheet"
          ! A.type_ "text/css"
          ! A.href "/style.css"
      H.body $
        H.div ! A.class_ "main" $ do
          header page
          body

header :: Page -> Html
header page = H.header $ H.nav $ mconcat $ fmap (linkPage page) pages

postToHtml :: Post -> Html
postToHtml p@(Post time title body) =
  H.div $ do
    H.h2 $ linkPost p
    H.p
      ! A.class_ "date"
      $ toHtml $ formatTime defaultTimeLocale "%Y-%m-%d" time
    H.p $ toHtml body

routes :: S.ScottyM()
routes = do
  S.get (pagePath homePage) $ do
    posts <- liftIO $ readPosts
    mkPage homePage $ do
      H.h1 "Posts"
      mapM_ postToHtml posts

  --TODO only available on login
  S.get (pagePath newPostPage) $
    mkPage newPostPage $ do
      H.h1 "New post"
      H.form ! A.method "post" $ do
        H.textarea ! A.name "title" $ "Blog post Title"
        H.br
        H.textarea ! A.name "body" $ "Blog post Body"
        H.br
        H.input ! A.type_ "submit" ! A.value "Submit post"

  S.post (pagePath newPostPage) $ do
    title <- S.param "title"
    body <- S.param "body"
    time <- liftIO $ getCurrentTime
    let p = Post time title body
    liftIO $ savePost p
    --TODO error catching!
    S.redirect "/"

  -- TODO sort by date/display dates?
  S.get (pagePath allPostsPage) $ do
    posts <- liftIO $ readPosts
    mkPage allPostsPage $ do
      H.h1 "All posts"
      H.ul $ do
        mapM_ (H.li . linkPost) posts

  S.get (pagePath $ postPage "") $ do
    postID <- S.param "postID"
    post <- liftIO $ readPost postID
    mkPage (postPage "Test") $ do
      postToHtml post

  S.get (pagePath loginPage) $ do
    mkPage loginPage $ H.p "nothing here"

  S.get "/style.css" $ do
    S.setHeader "Content-Type" "text/css"
    cd <- liftIO getCurrentDirectory
    S.file $ cd </> "static" </> "style.css"

savePost :: Post -> IO ()
savePost p = withPostDir $ writeFile (postId p) (show p)

postId :: Post -> String
postId (Post _ title _) = show $ hash title

readPosts :: IO [Post]
readPosts = listDirectory postDir >>= mapM readPost

-- TODO error handling check file exists
readPost :: FilePath -> IO Post
readPost = withPostDir . fmap read . readFile

linkPost :: Post -> Html
linkPost p@(Post _ title _) =
  H.a
    ! A.href (textValue $ pack $ "/posts/" <> postId p)
    $ toHtml title

linkPage :: Page -> PageConfig -> Html
linkPage currentPage (PageConfig page path text) =
  H.a
    ! A.href (textValue path)
    ! A.class_ (if page == currentPage then "current" else "")
    $ toHtml text

withPostDir :: IO a -> IO a
withPostDir a = do
  cd <- getCurrentDirectory
  --abs <- makeAbsolute(postDir
  withCurrentDirectory (cd </> postDir) $ a
