{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where
-- FIXME: Since zmy current zipper implentation only supports zippers with at
-- least one item, if any list has zero items wiil cause an error when trying
-- to focus on it

import Control.Monad (void)
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core (fill)
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import qualified Data.Vector as Vec
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( str
  , vLimit
  , hLimit
  , hBox
  )
import Brick.Util (on)

import qualified Zipper as Z

newtype ID = ID Int
    deriving (Eq, Enum)

data Task = Task {
    taskID :: ID,
    taskName :: String,
    taskOnDay :: Bool
}

data TodoList = TL String [Task]
              | MyDay

modifyTasks :: ([Task] -> [Task]) -> TodoList -> TodoList
modifyTasks _ MyDay = MyDay
modifyTasks f (TL name tasks) = TL name (f tasks)

deleteTask :: ID -> Z.Zipper TodoList -> Z.Zipper TodoList
deleteTask i = fmap (modifyTasks (filter (\t -> taskID t /= i)))

data Focus = Lists
           | Tasks
           deriving Eq

data CurrentState = CS (Z.Zipper TodoList) Focus (Maybe (Z.Zipper Task))

listToName :: TodoList -> String
listToName (TL name _) = name
listToName MyDay = "My Day"

getCurrentTasks :: Z.Zipper TodoList -> [Task]
getCurrentTasks z@(Z.Zipper _ curr _) = case curr of
    MyDay -> filter taskOnDay . concatMap getTasks . Z.toList $ z
    list -> getTasks list
    where
    getTasks MyDay = []
    getTasks (TL _ tasks) = tasks

zipperOffset :: Z.Zipper a -> Int
zipperOffset = length . Z.left

listNames :: Z.Zipper TodoList -> [String]
listNames = map listToName . Z.toList

taskNames :: Z.Zipper Task -> [String]
taskNames = map taskName . Z.toList

currentListName :: Z.Zipper TodoList -> String
currentListName = listToName . Z.curr

drawUI :: CurrentState -> [Widget Int]
drawUI (CS z focus tasks) = [ui]
    where
    listsWidget = L.listMoveBy (zipperOffset z) $ L.list 1 (Vec.fromList . listNames $ z) 1
    tasksWidget = case tasks of
        Just tasksZipper -> L.listMoveBy (zipperOffset tasksZipper) $ L.list 2 (Vec.fromList . taskNames $ tasksZipper) 1
        Nothing ->  L.list 3 (Vec.fromList []) 1
    label1 = str "Lists"
    label2 = str . currentListName $ z
    box1 = B.borderWithLabel label1 $
          hLimit 25 $
          vLimit 15 $
          L.renderList (\_ -> C.hCenter . str) (focus == Lists) listsWidget
    box2 = B.borderWithLabel label2 $
          hLimit 25 $
          vLimit 15 $
          L.renderList (\_ -> C.hCenter . str) (focus == Tasks) tasksWidget
    emptyBox = B.border $
          hLimit 25 $
          vLimit 15 $
          fill ' '
    ui = C.hCenter . C.vCenter $ hBox [box1 , if (focus == Tasks) then box2 else emptyBox]

appEvent :: CurrentState -> T.BrickEvent Int e -> T.EventM Int (T.Next CurrentState)
appEvent cs@(CS (Z.Zipper _ _ _) _ _) (T.VtyEvent e) =
    case e of
        V.EvKey V.KUp []         -> M.continue . moveUp $ cs
        V.EvKey (V.KChar 'k') [] -> M.continue . moveUp $ cs

        V.EvKey V.KDown []       -> M.continue . moveDown $ cs
        V.EvKey (V.KChar 'j') [] -> M.continue . moveDown $ cs

        V.EvKey V.KRight []      -> M.continue . moveRight $ cs
        V.EvKey (V.KChar 'l') [] -> M.continue . moveRight $ cs

        V.EvKey V.KLeft []       -> M.continue . moveLeft $ cs
        V.EvKey (V.KChar 'h') [] -> M.continue . moveLeft $ cs

        V.EvKey (V.KChar 'd') [] -> M.continue . deleteCurrentTask $ cs

        V.EvKey V.KEsc [] -> M.halt cs
        _ -> M.continue cs
appEvent _ _ = error "FIXME: unhandled"

deleteCurrentTask :: CurrentState -> CurrentState
deleteCurrentTask (CS _ Lists (Just _)) = error "invariant violation: focused on lists with tasks zipper"
deleteCurrentTask (CS _ Tasks Nothing) = error "invariant violation: focused on tasks with no tasks zipper"
deleteCurrentTask cs@(CS _ Lists Nothing) = cs
deleteCurrentTask (CS z Tasks (Just (Z.Zipper l t _))) = CS newListsZipper Tasks (Just newTasksZipper)
    where
    newListsZipper = deleteTask (taskID t) z
    newTasksZipper = applyNTimes (length l) (Z.goRight) . Z.fromList . getCurrentTasks $ newListsZipper
    applyNTimes n f = foldr (.) id (replicate n f)

moveUp :: CurrentState -> CurrentState
moveUp (CS _ Lists (Just _)) = error "invariant violation: focused on lists with tasks zipper"
moveUp (CS _ Tasks Nothing) = error "invariant violation: focused on tasks with no tasks zipper"
moveUp (CS z Tasks (Just tasksZipper)) = CS z Tasks (Just $ Z.goLeft tasksZipper)
moveUp (CS z Lists Nothing) = CS (Z.goLeft z) Lists Nothing

moveDown :: CurrentState -> CurrentState
moveDown (CS _ Lists (Just _)) = error "invariant violation: focused on lists with tasks zipper"
moveDown (CS _ Tasks Nothing) = error "invariant violation: focused on tasks with no tasks zipper"
moveDown (CS z Tasks (Just tasksZipper)) = CS z Tasks (Just $ Z.goRight tasksZipper)
moveDown (CS z Lists Nothing) = CS (Z.goRight z) Lists Nothing

moveLeft :: CurrentState -> CurrentState
moveLeft (CS z _ _) = CS z Lists Nothing

moveRight :: CurrentState -> CurrentState
moveRight (CS z _ _) = CS z Tasks (Just . Z.fromList . getCurrentTasks $ z)

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,                V.white `on` V.blue)
    , (L.listSelectedAttr,        V.white `on` V.black)
    , (L.listSelectedFocusedAttr, V.blue `on` V.white)
    ]

theApp :: M.App CurrentState e Int
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

foo :: Task
foo = Task (ID 1) "Foo" True

bar :: Task
bar = Task (ID 2) "Bar" False

baz :: Task
baz = Task (ID 3) "Baz" True

blah :: Task
blah = Task (ID 4) "Blah" False

listTodo :: TodoList
listTodo = TL "Todo" [foo, bar, blah]

listProjects :: TodoList
listProjects = TL "Projects" [baz]

initialState :: CurrentState
initialState = CS (Z.Zipper [MyDay] listTodo [listProjects]) Lists Nothing

main :: IO ()
main = void $ M.defaultMain theApp initialState
