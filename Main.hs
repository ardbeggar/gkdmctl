module Main
  where

import Graphics.UI.Gtk
import System.Process
import Text.Parsec
import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Control.Monad.Trans
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

newSession = void $ rawSystem "kdmctl" ["reserve"]

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
  window `on` keyPressEvent $ tryEvent $ do
    "Escape" <- eventKeyName
    []       <- eventModifier
    liftIO $ widgetDestroy window

  box <- vBoxNew False 7
  containerSetBorderWidth box 7
  containerAdd window box

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
        treeSortableSetSortFunc sorted id $ compBy (map toLower . acc)

  addColumn "Display"      0 sDisplay
  addColumn "VT"           1 sVT
  addColumn "User"         2 sUser
  addColumn "Session Type" 3 sType

  view `on` rowActivated $ \p _ -> do
    [n] <- treeModelSortConvertChildPathToPath sorted p
    s   <- listStoreGetValue store n
    widgetDestroy window
    switchTo s

  sel <- treeViewGetSelection view
  treeSelectionSelectPath sel [0]

  scroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic
  scrolledWindowSetShadowType scroll ShadowIn
  containerAdd scroll view

  boxPackStartDefaults box scroll

  bts <- hButtonBoxNew
  buttonBoxSetLayout bts ButtonboxEnd
  boxSetSpacing bts 7
  boxPackStart box bts PackNatural 0

  b <- buttonNewWithMnemonic "_New Session"
  b `on` buttonActivated $ do
    widgetDestroy window
    newSession
  containerAdd bts b

  b <- buttonNewWithMnemonic "_Close"
  b `on` buttonActivated $  widgetDestroy window
  containerAdd bts b

  widgetShowAll window
  mainGUI
