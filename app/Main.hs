{-# LANGUAGE OverloadedStrings #-}
module Main( main) where

import Cases
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.Hashable
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.String
import Data.Text (splitOn, unpack, pack, Text)
import Data.Time
import Data.Time.Format
import Network.HTTP.Types.Status
import Network.Wai.Middleware.Static hiding ((<|>))
import System.Directory
import System.FilePath
import Text.Blaze.Html hiding (text)
import Text.Blaze.Html.Renderer.Text
import Text.Read (readMaybe)
import Web.Scotty hiding (header)
import Web.Scotty.Cookie
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data Page = Home | AllPosts | NewPost | OnePost | LogIn | LogOut deriving Eq
data PageConfig = PageConfig Page Text Text

data Post = Post
  { date  :: UTCTime
  , title :: String
  , body  :: String
  } deriving (Read, Show)

-- Define Pages and Config for the Header
homePage     = PageConfig Home "/" "Home"
allPostsPage = PageConfig AllPosts "/posts" "All Posts"
newPostPage  = PageConfig NewPost "/new-post" "New Post"
postPage     = PageConfig OnePost "/posts/:postID"
loginPage    = PageConfig LogIn "/login" "Log In"
logoutPage   = PageConfig LogOut "/logout" "Logout"

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
  createDirectory postDir <|> pure ()
  putStrLn "Starting Server..."
  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "static")
    routes

users :: M.Map String Int
users = M.fromList [("me", 7205319831941986560)]

pagePath :: PageConfig ->  RoutePattern
pagePath (PageConfig _ a _) =  capture $ unpack a

mkPage :: PageConfig -> Html ->  ActionM ()
mkPage (PageConfig page _ title) body = do
  auth <- validCookie
  let pages = if auth then userPages else defPages
  html . renderHtml $ do
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

header :: Page -> [PageConfig] -> Html
header page pages = H.header $ H.nav $ mconcat $ fmap (linkPage page) pages

validCookie ::  ActionM Bool
validCookie = do
  cookie <- getCookie auth
  case cookie of
    Just c -> if elem c $ passwords then return True else return False
    _      -> return False
    where passwords = map (pack . show) $ M.elems users

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

errorHtml :: Text -> Html
errorHtml = (H.h3 ! A.class_ "error") . toHtml

routes ::  ScottyM()
routes = do
  get (pagePath homePage) $ do
    posts <- liftIO readPosts
    mkPage homePage $ do
      H.h1 "Posts"
      H.div ! A.class_ "posts" $
        mapM_ postToHtml $ take 5 $ sortOn (Down . date) posts

  get (pagePath newPostPage) $ do
    c <- getCookie auth
    case c of
      Just _ -> mkPage newPostPage newPostForm
      otherwise -> do
        status status401
        mkPage newPostPage $ errorHtml "Unathorised. Login to access"

  post (pagePath newPostPage) $ do
    title <-  param "title"
    body <-  param "body"
    time <- liftIO getCurrentTime
    let p = Post time title body
    dublicate <- liftIO $ withPostDir $ doesFileExist (postId p)
    if (not dublicate)
      then do liftIO $ savePost p
              redirect $ postHref p
      else mkPage newPostPage $ errorHtml "Duplicate post title" <> newPostForm

  get (pagePath allPostsPage) $ do
    posts <- liftIO readPosts
    mkPage allPostsPage $ do
      H.h1 "All posts"
      H.ul $ mapM_ (H.li . linkPost) $ sortOn (Down . date) posts

  get (pagePath $ postPage "") $ do
    postID <-  param "postID" `rescue` (\_ -> return "nopost")
    post <- liftIO $ readPost postID <|> pure Nothing
    mkPage (postPage "Test") $
      case post of
        Nothing -> errorHtml "This is not the post you are looking for"
        Just p  -> postToHtml p

  get (pagePath logoutPage) $ do
    deleteCookie auth
    redirect "/"

  get (pagePath loginPage) $ do
    mkPage loginPage $ loginHtml

  post (pagePath loginPage) $ do
    user <- param "username" `rescue` (\_ -> return "")
    pass <- unpack <$> param "password" `rescue` (\_ -> return "")
    case users M.!? user of
      Just p -> if p == hash pass then authorise p else showError
      _      -> showError
      where
        showError = mkPage loginPage $ errorHtml "Invalid username/password" <> loginHtml
        authorise p = do
          setSimpleCookie auth $ pack $ show p
          redirect "/"

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
readPosts = do
  files <- listDirectory postDir
  posts <- mapM readPost files <|> pure [Nothing]
  return $ catMaybes posts

readPost :: FilePath -> IO (Maybe Post)
readPost = withPostDir . fmap readMaybe . readFile

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
