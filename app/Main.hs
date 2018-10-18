{-# LANGUAGE OverloadedStrings #-}
module Main( main) where

import Cases
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.Hashable
import Data.List
import Data.Monoid
import Data.Ord
import Data.String
import Data.Text (splitOn, unpack, pack, Text)
import Data.Time
import Data.Time.Format
import Network.HTTP.Types.Status
import System.Directory
import System.FilePath
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Web.Scotty as S
import qualified Web.Scotty.Cookie as SC

data Page = Home | AllPosts | NewPost | OnePost | LogIn | LogOut deriving Eq
data PageConfig = PageConfig Page Text Text
data Post = Post
  { date  :: UTCTime
  , title :: String
  , body  :: String
  } deriving (Read, Show)

homePage     = PageConfig Home "/" "Home"
allPostsPage = PageConfig AllPosts "/posts" "All Posts"
newPostPage  = PageConfig NewPost "/new-post" "New Post"
postPage     = PageConfig OnePost "/posts/:postID"
loginPage    = PageConfig LogIn "/login" "Log In"
logoutPage    = PageConfig LogOut "/logout" "Logout"

defPages = [ homePage
        , allPostsPage
        , loginPage
        ]

userPages = [ homePage
            , allPostsPage
            , newPostPage
            , logoutPage
            ]

postDir = "posts"
auth = "auth"

main :: IO ()
main = do
  -- TODO add css
  createDirectory postDir <|> pure ()
  putStrLn "Starting Server..."
  S.scotty 3000 routes

pagePath :: PageConfig -> S.RoutePattern
pagePath (PageConfig _ a _) = S.capture $ unpack a

mkPage :: PageConfig -> Html -> S.ActionM ()
mkPage (PageConfig page _ title) body = do
  auth <- validCookie
  let pages = if auth then userPages else defPages
  S.html . renderHtml $ do
    H.docType
    H.html $ do
      H.head $ do
        H.title $ toHtml title
        H.link
          ! A.rel "stylesheet"
          ! A.type_ "text/css"
          ! A.href "/style.css"
      H.body $
        H.div ! A.class_ (textValue $ spinalize title)
          $ do
            header page pages
            body

-- point free?
header :: Page -> [PageConfig] -> Html
header page pages = H.header $ H.nav $ mconcat $ fmap (linkPage page) pages

validCookie :: S.ActionM Bool
validCookie = do
  c <- SC.getCookie auth
  case c of
    Just _ -> return True
    otherwise -> return False

postToHtml :: Post -> Html
postToHtml p@(Post time title body) =
  H.div ! A.class_ "post" $ do
      H.h2 $ linkPost p
      H.div ! A.class_ "date"
        $ toHtml $ formatTime defaultTimeLocale "%Y-%m-%d" time
      H.div ! A.class_ "body" $
        mconcat $ intercalate [H.br] $
          fmap (pure . toHtml) $ splitOn "\n" $ pack body

newPostForm :: Html
newPostForm =
  H.html $ do
    H.h1 "New post"
    H.form ! A.method "post" $ do
      H.textarea ! A.name "title" $ "Blog post Title"
      H.textarea ! A.name "body" $ "Blog post Body"
      H.input ! A.type_ "submit" ! A.value "Submit post"


loginHtml :: Html
loginHtml =
  H.html $ do
    H.form ! A.method "post" $ do
      H.input ! A.name "username" ! A.placeholder "username"
      H.input ! A.type_ "password" ! A.name "password" ! A.placeholder "password"
      H.button ! A.type_ "submit" $ "login"

routes :: S.ScottyM()
routes = do
  S.get (pagePath homePage) $ do
    posts <- liftIO readPosts
    mkPage homePage $ do
      H.h1 "Posts"
      H.div ! A.class_ "posts" $
        mapM_ postToHtml $ take 5 posts

  S.get (pagePath newPostPage) $ do
    c <- SC.getCookie auth
    case c of
      Just _ -> mkPage newPostPage newPostForm
      otherwise -> do
        S.status status401
        mkPage newPostPage $ H.h1 "Unathorised"

  S.post (pagePath newPostPage) $ do
    title <- S.param "title"
    body <- S.param "body"
    time <- liftIO getCurrentTime
    let p = Post time title body
    S.liftAndCatchIO $ savePost p
    --TODO error catching!
    S.redirect (postHref p)

  -- TODO sort by date/display dates?
  S.get (pagePath allPostsPage) $ do
    posts <- liftIO readPosts
    mkPage allPostsPage $ do
      H.h1 "All posts"
      H.ul $ mapM_ (H.li . linkPost) posts

  S.get (pagePath $ postPage "") $ do
    postID <- S.param "postID"
    post <- liftIO $ readPost postID
    mkPage (postPage "Test") $ postToHtml post

  S.get (pagePath loginPage) $ do
    mkPage loginPage $ loginHtml

  S.post (pagePath loginPage) $ do
    user <- S.param "username"
    pass <- S.param "password"
    if user == ("me"::String) && pass == ("me"::String)
      then do
        SC.setSimpleCookie auth $ pack $ show (hash ("me"::String))
        S.redirect "/"
      else mkPage loginPage $
        ( H.p ! A.class_ "error" $ "Invalid username/password" >> loginHtml)

  S.get (pagePath logoutPage) $ do
    SC.deleteCookie auth
    S.redirect "/"

  S.get "/style.css" $ do
    S.setHeader "Content-Type" "text/css"
    cd <- liftIO getCurrentDirectory
    S.file $ cd </> "static" </> "style.css"

savePost :: Post -> IO ()
savePost p = withPostDir $ do
  dublicate <- doesFileExist (postId p)
  if (not dublicate)
    then writeFile (postId p) (show p)
    else fail "Duplicate post title"

-- easy way of changin post id (used for file & url)
postId :: Post -> String
postId (Post date title _) = (formatTime defaultTimeLocale "%Y-%m-%d-" date) ++ title

readPosts :: IO [Post]
readPosts = listDirectory postDir >>= mapM readPost

-- TODO error handling check file exists
readPost :: FilePath -> IO Post
readPost = withPostDir . fmap read . readFile

postHref :: Post -> L.Text
postHref p = L.pack $ "/posts/" <> postId p

linkPost :: Post -> Html
linkPost p@(Post _ title _) = H.a ! A.href (textValue $ L.toStrict (postHref p)) $ toHtml title

linkPage :: Page -> PageConfig -> Html
linkPage currentPage (PageConfig page path text) =
  H.a
    ! A.href (textValue path)
    ! A.class_ (if page == currentPage then "current" else "")
    $ toHtml text

withPostDir :: IO a -> IO a
withPostDir a = do
  cd <- getCurrentDirectory
  withCurrentDirectory (cd </> postDir) a
