module Main
  where

import Graphics.UI.Gtk
import System.Process
import Text.Parsec
import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.Char (toLower)
import Data.Ord

data Flag
  = Current
    deriving (Show, Eq)

data Session
  = S { sDisplay :: String
      , sVT      :: String
      , sUser    :: String
      , sType    :: String
      , sFlags   :: [Flag]
      }
  deriving (Show)


getSessionList = do
  o <- readProcess "kdmctl" ["list", "alllocal"] ""
  return $ either (const $ Left "parse error") id $ parse parseOutput "<command output>" o

switchTo s = void $ rawSystem "kdmctl" ["activate", sDisplay s]

emptySession =
  S { sDisplay = ""
    , sVT      = ""
    , sUser    = ""
    , sType    = ""
    , sFlags   = []
    }

parseOutput = do
  o <- ok <|> err
  newline
  return o

err = return $ Left "error"

ok = do
  string "ok"
  tab
  Right <$> (tabSep session)

tabSep p = p `sepBy` tab

session = do
  d <- field
  v <- field
  u <- field
  t <- field
  _ <- field
  return S { sDisplay = d
           , sVT      = v
           , sUser    = u
           , sType    = t
           , sFlags   = []
           }
  where field = do
          f <- many $ noneOf "\t\n,"
          optional $ char ','
          return f

test args = do
  o <- readProcess "kdmctl" args ""
  return $ parseTest parseOutput o

main = do
  initGUI

  window <- windowNew
  window `onDestroy` mainQuit

  ss <- either (const []) id <$> getSessionList
  print ss

  store <- listStoreNewDND ss Nothing Nothing
  sorted <- treeModelSortNewWithModel store

  let getByIter iter = do
        [n] <- treeModelGetPath store iter
        listStoreGetValue store n
      compBy :: Ord a => (Session -> a) -> TreeIter -> TreeIter -> IO Ordering
      compBy f a b =
        comparing f <$> getByIter a <*> getByIter b

  treeSortableSetDefaultSortFunc sorted $ Just $ compBy (map toLower . sUser)
  treeSortableSetSortFunc sorted 0 $ compBy (map toLower . sDisplay)
  treeSortableSetSortFunc sorted 1 $ compBy (map toLower . sVT)
  treeSortableSetSortFunc sorted 2 $ compBy (map toLower . sUser)
  treeSortableSetSortFunc sorted 3 $ compBy (map toLower . sType)

  view <- treeViewNewWithModel sorted
  treeViewSetRulesHint view True

  let addColumn title id acc = do
        column <- treeViewColumnNew
        treeViewColumnSetSortColumnId column id
        treeViewAppendColumn view column
        treeViewColumnSetTitle column title
        cell <- cellRendererTextNew
        treeViewColumnPackStart column cell True
        cellLayoutSetAttributes column cell store
          (\p -> [ cellText := acc p ])

  addColumn "Display"      0 sDisplay
  addColumn "VT"           1 sVT
  addColumn "User"         2 sUser
  addColumn "Session Type" 3 sType

  view `onRowActivated` \p _ -> do
    [n] <- treeModelSortConvertChildPathToPath sorted p
    s   <- listStoreGetValue store n
    widgetDestroy window
    switchTo s

  scroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic
  scrolledWindowSetShadowType scroll ShadowIn
  containerAdd scroll view
  containerAdd window scroll

  widgetShowAll window
  mainGUI
