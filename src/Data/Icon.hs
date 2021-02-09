--
-- Data: Icon
--

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Data.Icon
  ( IconIndex
  , iconSVGWithName, load
  ) where



import Control.Monad(forM_)
import Data.Map (Map)
import qualified Data.Map as M
  ( fromList
  , lookup
  )
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import qualified Data.Text.IO as T (readFile)
import System.Directory
  ( getCurrentDirectory
  , listDirectory
  )
import System.FilePath.Posix
  ( dropExtensions
  , takeFileName
  , (</>)
  )


data Icon = Icon
  { name :: Text
  , svg :: Text
  } deriving (Eq, Show)


newtype IconIndex = IconIndex {
  byName :: Map Text Icon
} deriving (Eq, Show)


-- | Get the icon by it's file name if it exists:
-- Example file name: this_is_the_article
iconSVGWithName :: Text -> IconIndex -> Maybe Text
iconSVGWithName iconName index = svg <$> M.lookup iconName index.byName


-- | Reads all of the file icons in the directory into the index as Text
-- TODO check if is absolute or relative path, currently assumes relative
load :: FilePath -> IO (Maybe IconIndex)
load iconsDir = do
  iconFiles <- listDirectory iconsDir
  icons <- mapM (iconFromFile iconsDir) iconFiles 
  return $ Just $ 
    IconIndex $ M.fromList $ (\i -> (i.name, i)) <$> icons
  where
    iconFromFile :: FilePath -> FilePath -> IO Icon
    iconFromFile iconsDir fp = do
      content <- T.readFile $ iconsDir </> fp 
      let iconName = dropExtensions $ takeFileName fp
      return $ Icon {
        name = T.pack iconName
      , svg  = content
      }
      
