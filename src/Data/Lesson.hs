--
-- Data: Article
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Data.Lesson 
  ( LessonIndex (..), maybeArticle
  , Lesson (..)
  , Article (..)
  , load
  ) where


import Control.Monad (mapM)
import Control.Monad.Except
  ( ExceptT
  , runExceptT
  , liftIO, throwError
  )
import Data.Aeson
  ( ToJSON, FromJSON
  , toEncoding, genericToEncoding, defaultOptions
  )
import Data.Map (Map)
import qualified Data.Map as M (empty, fromList, lookup)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import qualified Data.Text.IO as T (readFile)
import qualified Data.Yaml as Y
  ( decodeFileEither
  )
import GHC.Generics

import System.FilePath.Posix ( (</>) )



----------------------------------------------------------------------------------------------------
-- TYPES
----------------------------------------------------------------------------------------------------

-- Lesson Index
----------------------------------------------------------------------------------------------------

data LessonIndex = LessonIndex {
    lessons :: [Lesson]
  , byId    :: Map Text Lesson
} deriving (Eq, Show)


maybeArticle :: Text -> Text -> LessonIndex -> Maybe Article
maybeArticle lessonId articleId (LessonIndex _ byId) = do
  lesson <- M.lookup lessonId byId
  M.lookup articleId lesson.byId 
  

-- Lesson
----------------------------------------------------------------------------------------------------

data Lesson = Lesson {
    lessonId :: Text
  , title    :: Text
  , summary  :: Text
  , articles :: [Article]
  , byId     :: Map Text Article
} deriving (Eq, Generic, Show)


-- Article
----------------------------------------------------------------------------------------------------

data Article = Article {
    articleId :: Text
  , title     :: Text
  , markdown  :: Text 
} deriving (Eq, Generic, Show)


-- Index File
----------------------------------------------------------------------------------------------------

newtype IndexFile = IndexFile {
    lessonEntries :: [IndexFileLessonEntry]
} deriving (Eq, Generic, Show)

instance ToJSON IndexFile where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON IndexFile


-- Index File Lesson Entry
----------------------------------------------------------------------------------------------------

data IndexFileLessonEntry = IndexFileLessonEntry {
    lessonId       :: Text
  , title          :: Text
  , summary        :: Text
  , articleEntries :: [IndexFileArticleEntry]
} deriving (Eq, Generic, Show)

instance ToJSON IndexFileLessonEntry where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON IndexFileLessonEntry


-- Index File Article Entry
----------------------------------------------------------------------------------------------------

data IndexFileArticleEntry = IndexFileArticleEntry {
    articleId :: Text
 ,  title     :: Text
} deriving (Eq, Generic, Show)

instance ToJSON IndexFileArticleEntry where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON IndexFileArticleEntry


----------------------------------------------------------------------------------------------------
-- FUNCTIONS
----------------------------------------------------------------------------------------------------

-- | Load index file
-- Run on server startup 
load :: FilePath -> FilePath -> IO (Maybe LessonIndex)
load indexFilePath articleDir = do
  eIndexFile <- liftIO $ Y.decodeFileEither indexFilePath
  case eIndexFile of
    Left  ex        -> do
      putStrLn $ "Could not parse lesson index file: " <> show ex
      return Nothing
    Right indexFile -> do
      -- Load the content for each article
      eLessons <- runExceptT $ mapM (loadLesson articleDir) $ lessonEntries indexFile 
      case eLessons of
        Left  err     -> do
          print err
          return Nothing
        Right lessons -> return $ Just $ LessonIndex {
            lessons = lessons
          , byId = M.fromList $ (\l -> (l.lessonId, l)) <$> lessons
        }


-- | Load Lesson
loadLesson :: FilePath -> IndexFileLessonEntry -> ExceptT LoadError IO Lesson
loadLesson articleDir (IndexFileLessonEntry id_ title summary articleEntries) = do
  articles <- mapM (loadArticle articleDir) articleEntries
  return $ Lesson {
      lessonId = id_
    , title    = title
    , summary  = summary
    , articles = articles 
    , byId     = M.fromList $ (\a -> (a.articleId, a)) <$> articles
  }


-- Load Article
loadArticle :: FilePath -> IndexFileArticleEntry -> ExceptT LoadError IO Article
loadArticle articleDir (IndexFileArticleEntry id_ title) = do
  content <- liftIO $ T.readFile $ articleDir </> T.unpack (id_ <> ".md")
  return $ Article {
      articleId = id_ 
    , title     = title 
    , markdown  = content
  }
        

newtype LoadError = LoadError Text
  deriving (Eq, Show)
