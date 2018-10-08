{-# LANGUAGE OverloadedStrings #-}
module Main( main) where

import Control.Monad.IO.Class
import Data.Hashable
import Data.List
import Data.Monoid
import Data.Ord
import Data.String
import Data.Text (unpack, Text)
import Data.Time
import Data.Time.Format
import System.Directory
import System.FilePath
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text
import qualified Data.Text.IO as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Web.Scotty as S

data Page = Home | NewPost | LogIn deriving Eq
data Post = Post
  { id    :: Int
  , date  :: UTCTime
  , title :: String
  , body  :: String
  } deriving (Read, Show)

idFileName = "./idfile"
postDir = "posts"

main :: IO ()
main = do
  -- TODO add css
  putStrLn "Starting Server..."
  S.scotty 3000 routes

data PageConfig = PageConfig Page Text Text
homePage    = PageConfig Home "/" "Home"
newPostPage = PageConfig NewPost "/new-post" "New Post"
loginPage   = PageConfig LogIn "/login" "Log In"

pagePath :: PageConfig -> S.RoutePattern
pagePath (PageConfig _ a _) = S.capture $ unpack a

pages = [ homePage
        , newPostPage
        , loginPage
        ]

mkPage :: PageConfig -> Html -> S.ActionM ()
mkPage (PageConfig page _ title) body =
  S.html . renderHtml $ do
    H.head $ do
      H.title $ toHtml title
      -- TODO link style sheet
    H.body $
      H.div ! A.class_ "main" $ do
        header page
        body

--readPosts :: [Int] -> IO ([Post])
--readPosts = mapM (\id -> fmap read $ readFile $ blogFileName <> show id)
--readPosts = getDirectoryContents postDir
readPosts :: [FilePath] -> IO [Post]
readPosts = mapM (fmap read . readFile)

pToHtml :: Post -> Html
pToHtml (Post i time title body) =
  H.div $ do
    H.h1 $ H.toHtml title
    H.p $ H.toHtml body

header :: Page -> Html
header page =
  H.header $ H.nav $ mconcat $ fmap (toLink page) pages
  where
    toLink currentLoc (PageConfig loc path text) =
      H.a
        ! A.href (textValue path)
        ! A.class_ (if loc==currentLoc then "current" else "")
        $ toHtml text

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

now :: IO String
now = do
  time <- getCurrentTime
  return $ formatTime defaultTimeLocale "%Y-%m-%d" time


tString :: FormatTime t => t -> String
tString = formatTime defaultTimeLocale "%Y-%m-%d"

tUnix :: FormatTime t => t -> String
tUnix = formatTime defaultTimeLocale "%s"


routes :: S.ScottyM()
routes = do
  S.get (pagePath homePage) $ do
    posts <- liftIO $ do
      files <- listDirectory postDir
      wd <- getCurrentDirectory
      posts <- withCurrentDirectory (wd </> postDir) . readPosts $ files
      return $ take 5 $ sortBy (comparing date) posts
    -- postAmt <- liftIO $ fmap read $ readFileOr idFileName "0"
    -- posts <- liftIO $ readPosts [0..(postAmt - 1)]
    mkPage homePage $ do
      H.h1 "Posts"
      mapM_ pToHtml posts

  S.get "/test" $ do
    S.html . renderHtml $ do
      H.h1 "Test"

  -- S.get "/posts" $ do
  --   id <- liftIO $ fmap read $ readFile idFileName
  --   posts <- liftIO $ readPosts [0..id]
  --   S.html . renderHtml $ do
  --     H.h1 "Posts"
  --     mapM_ pToHtml posts

  S.get (pagePath newPostPage) $ do
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
    -- identifier <- liftIO $ fmap read $ readFile idFileName
    time <- liftIO $ getCurrentTime
    let id = hash $ title
        p = Post id time title body
    -- liftIO $ writeFile idFileName (show (identifier + 1))
    liftIO $ writeFile (pToPath p) (show p)
    S.html .renderHtml $ do
      H.h1 $ "Success!!! "

pToPath :: Post -> FilePath
pToPath (Post i tm t b) = postDir <> "/" <> (tUnix tm) <> (show i)
