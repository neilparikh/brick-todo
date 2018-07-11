{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Main where

import GHC.Generics
import System.Directory (doesFileExist)

-- lens
import Lens.Micro
import Data.Generics.Product

-- cereal
import qualified Data.ByteString as BS (readFile, writeFile)
import Data.Serialize (Serialize, decode, encode)

-- Brick
import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Data.Vector as Vec
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( str
  , strWrap
  , vLimit
  , hLimit
  , hBox
  , vBox
  , fill
  , padLeftRight
  )
import Brick.Util (on)

-- Zipper
import qualified NonEmptyZipper as NEZ
import qualified Zipper as Z
import qualified Data.Text.Zipper as TZ

newtype ID = ID Int
    deriving (Eq, Enum, Generic)

data Task = Task {
    taskID :: ID,
    taskName :: String,
    taskOnDay :: Bool
} deriving (Generic)

data TodoList = TL String [Task]
              | MyDay
              deriving (Generic)

data Focus = Lists
           | Tasks
           deriving (Show, Eq)

data CurrentState = CS {
    csLists :: NEZ.Zipper TodoList,
    csTasks :: Maybe (Z.Zipper Task),
    csEditor :: Maybe (E.Editor String String),
    nextID :: ID
} deriving (Generic)

data FileState = FileState [TodoList] ID
    deriving (Generic)

instance Serialize ID
instance Serialize Task
instance Serialize TodoList
instance Serialize FileState

currentStateToFileState :: CurrentState -> FileState
currentStateToFileState cs = FileState (NEZ.toList . csLists $ cs) (nextID cs)

fileStateToCurrentState :: FileState -> CurrentState
fileStateToCurrentState (FileState lists newID) = CS (NEZ.fromList lists) Nothing Nothing newID

csFocus :: CurrentState -> Focus
csFocus (CS _ Nothing _ _) = Lists
csFocus (CS _ (Just _) _ _) = Tasks

setEditorTo :: Maybe (E.Editor String String) -> CurrentState -> CurrentState
setEditorTo e = field @"csEditor" .~ e

generateTasksZipper :: CurrentState -> CurrentState
generateTasksZipper cs = cs & field @"csTasks" .~ (Just . Z.fromList . getCurrentTasks) (csLists cs)

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes n f = foldr (.) id (replicate n f)

modifyTasks :: ([Task] -> [Task]) -> TodoList -> TodoList
modifyTasks _ MyDay = MyDay
modifyTasks f (TL name tasks) = TL name (f tasks)

deleteTask :: ID -> NEZ.Zipper TodoList -> NEZ.Zipper TodoList
deleteTask i = fmap (modifyTasks (filter (\t -> taskID t /= i)))

renameTask :: ID -> String -> NEZ.Zipper TodoList -> NEZ.Zipper TodoList
renameTask i newName = fmap (modifyTasks (fmap (\t -> if taskID t == i then t & field @"taskName" .~ newName else t)))

markTask :: ID -> NEZ.Zipper TodoList -> NEZ.Zipper TodoList
markTask i = fmap (modifyTasks (fmap (\t -> if taskID t == i then t & field @"taskOnDay" %~ not else t)))

listToName :: TodoList -> String
listToName (TL name _) = name
listToName MyDay = "My Day"

getCurrentTasks :: NEZ.Zipper TodoList -> [Task]
getCurrentTasks z@(NEZ.Zipper _ curr _) = case curr of
    MyDay -> filter taskOnDay . concatMap getTasks . NEZ.toList $ z
    list -> getTasks list
    where
    getTasks MyDay = []
    getTasks (TL _ tasks) = tasks

listNames :: NEZ.Zipper TodoList -> [String]
listNames = map listToName . NEZ.toList

taskNames :: Z.Zipper Task -> [String]
taskNames = map taskName . Z.toList

addMark :: Z.Zipper Task -> Z.Zipper Task
addMark = fmap (\t -> if taskOnDay t then t & field @"taskName" %~ ("* " ++) else t)

currentListName :: NEZ.Zipper TodoList -> String
currentListName = listToName . NEZ.curr

nameForCurrentItem :: CurrentState -> String
nameForCurrentItem (CS _ (Just tasks) _ _) = if name == " " then "" else name -- special case for a new task
    where
    name = maybe "" taskName (Z.getCurrent tasks)
nameForCurrentItem (CS (NEZ.Zipper _ (TL " " []) _) Nothing _ _) = "" -- special case for a new list
nameForCurrentItem (CS lists Nothing _ _) = currentListName lists

insertAtPosition :: Int -> a -> [a] -> [a]
insertAtPosition i el xs = take i xs ++ [el] ++ drop i xs

drawUI :: CurrentState -> [Widget String]
drawUI cs@(CS z tasks editor _) = [ui]
    where
    focus = csFocus cs
    listsWidget = L.listMoveBy (NEZ.offset z) $ L.list "Lists" (Vec.fromList . listNames $ z) 1
    tasksWidget = case tasks of
        Just tasksZipper -> L.listMoveBy (Z.offset tasksZipper) $ L.list (currentListName z) (Vec.fromList . taskNames . addMark $ tasksZipper) 1
        Nothing -> L.list "Tasks" (Vec.fromList []) 1
    editWidget = case editor of
        Just editor' -> E.renderEditor (str . unlines) True editor'
        Nothing -> str ""
    label1 = str "Lists"
    label2 = str . currentListName $ z
    box1 = B.borderWithLabel label1 $
          hLimit 40 $
          vLimit 30 $
          vBox $ L.renderList (\_ -> C.hCenter . str) (focus == Lists) listsWidget : [editWidget | focus == Lists]
    box2 = B.borderWithLabel label2 $
          hLimit 40 $
          vLimit 30 $
          vBox $ L.renderList (\_ -> padLeftRight 1 . C.hCenter . strWrap) (focus == Tasks) tasksWidget : [editWidget | focus == Tasks]
    emptyBox = B.border $
          hLimit 40 $
          vLimit 30 $
          fill ' '
    ui = C.hCenter . C.vCenter $ hBox [box1 , if focus == Tasks then box2 else emptyBox]

appEvent :: CurrentState -> T.BrickEvent String e -> T.EventM String (T.Next CurrentState)
appEvent cs@(CS _ _ Nothing _) (T.VtyEvent e) =
    case e of
        V.EvKey V.KUp []         -> M.continue . moveUp $ cs
        V.EvKey (V.KChar 'k') [] -> M.continue . moveUp $ cs

        V.EvKey V.KDown []       -> M.continue . moveDown $ cs
        V.EvKey (V.KChar 'j') [] -> M.continue . moveDown $ cs

        V.EvKey V.KRight []      -> M.continue . moveRight $ cs
        V.EvKey (V.KChar 'l') [] -> M.continue . moveRight $ cs

        V.EvKey V.KLeft []       -> M.continue . moveLeft $ cs
        V.EvKey (V.KChar 'h') [] -> M.continue . moveLeft $ cs

        V.EvKey (V.KChar 'd') [] -> M.continue . deleteCurrentItem $ cs

        V.EvKey (V.KChar 'm') [] -> M.continue . markCurrentItem $ cs

        V.EvKey (V.KChar 'a') [] -> M.continue . addNewItem $ cs

        V.EvKey (V.KChar 'e') [] -> case csTasks cs of
            Just (Z.Zipper [] []) -> M.continue cs
            _ -> M.continue . editCurrentItem $ cs

        V.EvKey V.KEsc [] -> M.halt cs
        _ -> M.continue cs
appEvent cs@(CS _ _ (Just editor) _) (T.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> M.continue $ setEditorTo Nothing cs
        V.EvKey V.KEnter [] -> M.continue $ setEditorTo Nothing $ updateCurrentItem cs (mconcat $ E.getEditContents editor)
        e' -> do
            editor' <- E.handleEditorEvent e' editor
            M.continue $ setEditorTo (Just editor') cs
appEvent _ _ = error "FIXME: unhandled"

editCurrentItem :: CurrentState -> CurrentState
editCurrentItem cs = setEditorTo (Just editor) cs
    where
    focus = csFocus cs
    editor = E.applyEdit TZ.gotoEOL $ E.editor ("edit " ++ show focus) (Just 1) (nameForCurrentItem cs)

addNewItem :: CurrentState -> CurrentState
addNewItem cs@(CS (NEZ.Zipper l curr r) Nothing _ _) = editCurrentItem $ cs & field @"csLists" .~ NEZ.Zipper (curr:l) (TL " " []) r
addNewItem cs@(CS (NEZ.Zipper _ (TL name tasks) _) (Just tasksZipper) _ newID) = editCurrentItem . applyNTimes offset moveDown . generateTasksZipper $ cs & field @"csLists" %~ (NEZ.updateCurrent . const $ newList) & field @"nextID" %~ succ
    where
    newTask = Task newID " " False
    newList = TL name (insertAtPosition offset newTask tasks)
    offset = Z.offset tasksZipper + 1
-- FIXME: figure out how to add items to the my day list
addNewItem cs@(CS (NEZ.Zipper _ MyDay _) (Just _) _ _) = cs

deleteCurrentItem :: CurrentState -> CurrentState
deleteCurrentItem (CS _ (Just (Z.Zipper (_:_) [])) _ _) = error "invariant violation: zipper with non empty left but empty right"
deleteCurrentItem (CS (NEZ.Zipper [] (TL _ _) []) Nothing _ _) = error "invariant violation: MyDay list does not exist"
deleteCurrentItem cs@(CS (NEZ.Zipper _ MyDay _) Nothing _ _) = cs
deleteCurrentItem cs@(CS (NEZ.Zipper l _ (x:xs)) Nothing _ _) = cs & field @"csLists" .~ NEZ.Zipper l x xs
deleteCurrentItem cs@(CS (NEZ.Zipper (x:xs) _ []) Nothing _ _) = cs & field @"csLists" .~ NEZ.Zipper xs x []
deleteCurrentItem cs@(CS _ (Just (Z.Zipper [] [])) _ _) = cs
deleteCurrentItem (CS z (Just (Z.Zipper l (t:_))) edit newID) = CS newListsZipper (Just newTasksZipper) edit newID
    where
    newListsZipper = deleteTask (taskID t) z
    newTasksZipper = applyNTimes (length l) Z.goRight . Z.fromList . getCurrentTasks $ newListsZipper

markCurrentItem :: CurrentState -> CurrentState
markCurrentItem (CS _ (Just (Z.Zipper (_:_) [])) _ _) = error "invariant violation: zipper with non empty left but empty right"
markCurrentItem cs@(CS _ Nothing _ _) = cs
markCurrentItem cs@(CS _ (Just (Z.Zipper [] [])) _ _) = cs
markCurrentItem (CS z (Just (Z.Zipper l (t:_))) edit newID) = CS newListsZipper (Just newTasksZipper) edit newID
    where
    newListsZipper = markTask (taskID t) z
    newTasksZipper = applyNTimes (length l) Z.goRight . Z.fromList . getCurrentTasks $ newListsZipper

updateCurrentItem :: CurrentState -> String -> CurrentState
updateCurrentItem (CS _ (Just (Z.Zipper (_:_) [])) _ _) _ = error "invariant violation: zipper with non empty left but empty right"
updateCurrentItem (CS _ (Just (Z.Zipper [] [])) _ _) _ = error "invariant violation: trying to update a non-existent task"
updateCurrentItem cs@(CS (NEZ.Zipper _ MyDay _) Nothing _ _) _ = cs
updateCurrentItem cs@(CS (NEZ.Zipper _ (TL _ tasks) _) Nothing _ _) newName = cs & field @"csLists" %~ (NEZ.updateCurrent . const $ TL newName tasks)
updateCurrentItem (CS z (Just (Z.Zipper l (t:_))) edit newID) newName = CS newListsZipper (Just newTasksZipper) edit newID
    where
    newListsZipper = renameTask (taskID t) newName z
    newTasksZipper = applyNTimes (length l) Z.goRight . Z.fromList . getCurrentTasks $ newListsZipper

moveUp :: CurrentState -> CurrentState
moveUp cs@(CS _ (Just _) _ _) = cs & field @"csTasks" %~ fmap Z.goLeft
moveUp cs@(CS _ Nothing _ _) = cs & field @"csLists" %~ NEZ.goLeft

moveDown :: CurrentState -> CurrentState
moveDown cs@(CS _ (Just _) _ _) = cs & field @"csTasks" %~ fmap Z.goRight
moveDown cs@(CS _ Nothing _ _) = cs & field @"csLists" %~ NEZ.goRight

moveLeft :: CurrentState -> CurrentState
moveLeft cs = cs & field @"csTasks" .~ Nothing

moveRight :: CurrentState -> CurrentState
moveRight cs = cs & field @"csTasks" .~ Just tasksZipper
    where
    tasksZipper = Z.fromList . getCurrentTasks $ csLists cs

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,                V.white `on` V.blue)
    , (L.listSelectedAttr,        V.white `on` V.black)
    , (L.listSelectedFocusedAttr, V.blue `on` V.white)
    ]

theApp :: M.App CurrentState e String
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

emptyState :: FileState
emptyState = FileState [MyDay, TL "Todo" []] (ID 1)

main :: IO ()
main = do
    let fileName = "/Users/nparikh/brick_todo.data"
    fileExists <- doesFileExist fileName
    newState <- if fileExists
    then do
        contents <- BS.readFile fileName
        case decode contents of
            Left err -> error err
            Right state -> M.defaultMain theApp (fileStateToCurrentState state)
    else M.defaultMain theApp (fileStateToCurrentState emptyState)
    BS.writeFile fileName $ encode . currentStateToFileState $ newState
