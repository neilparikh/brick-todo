module Main where
-- FIXME: focusing and scrolling on the my day list is disabled for now
-- since it's derived from the other lists, so I'm not really sure how to keep
-- track of the current positon in the list
-- One potential way to fix this would be to store a zippered list for myday
-- as well, and then update it on every modification to the other lists,
-- instead of generate myday at render time

import Control.Monad (void)
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
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

data Task = Task String Bool -- name and if it is on the day list
    deriving Show

data TodoList = TL String (Z.Zipper Task)
              | MyDay
              | AddList
    deriving Show

data Focus = Lists
           | Tasks
           deriving Eq

data CurrentState = CS (Z.Zipper TodoList) Focus
    -- deriving Show

listToName :: TodoList -> String
listToName (TL name _) = name
listToName MyDay = "My Day"
listToName AddList = "Add List [+]"

taskToName :: Task -> String
taskToName (Task name _) = name

getCurrentTasks :: Z.Zipper TodoList -> [Task]
getCurrentTasks z@(Z.Zipper _ curr _) = case curr of
    MyDay -> filter isOnDay . concatMap getTasks . Z.toList $ z
    list -> getTasks list
    where
    isOnDay (Task _ onDay) = onDay
    getTasks MyDay = []
    getTasks AddList = []
    getTasks (TL _ tasks) = Z.toList tasks

getCurrentTasksOffset :: Z.Zipper TodoList -> Int
getCurrentTasksOffset (Z.Zipper _ curr _) = case curr of
    MyDay -> 0
    AddList -> 0
    TL _ (Z.Zipper left _ _) -> length left


foo :: Task
foo = Task "Foo" True

bar :: Task
bar = Task "Bar" True

baz :: Task
baz = Task "Baz" False

listTodo :: TodoList
listTodo = TL "Todo" (Z.fromList [foo, bar])

listProjects :: TodoList
listProjects = TL "Projects" (Z.fromList [baz])

initialState :: CurrentState
initialState = CS (Z.Zipper [MyDay] listTodo [listProjects, AddList]) Lists

drawUI :: CurrentState -> [Widget Int]
drawUI (CS z@(Z.Zipper left curr _) focus) = [ui]
    where
    -- listsWidget :: L.List Int String
    listsWidget = L.listMoveBy (length left) $ L.list 1 (Vec.fromList . map listToName . Z.toList $ z) 1
    tasksWidget = L.listMoveBy (getCurrentTasksOffset z) $ L.list 2 (Vec.fromList . map taskToName . getCurrentTasks $ z) 1
    label1 = str "Lists"
    label2 = str . listToName $ curr
    box1 = B.borderWithLabel label1 $
          hLimit 25 $
          vLimit 15 $
          L.renderList (\_ -> C.hCenter . str) (focus == Lists) listsWidget
    box2 = B.borderWithLabel label2 $
          hLimit 25 $
          vLimit 15 $
          L.renderList (\_ -> C.hCenter . str) (focus == Tasks) tasksWidget
    ui = C.hCenter . C.vCenter $ hBox [  box1 ,  box2 ]

appEvent :: CurrentState -> T.BrickEvent Int e -> T.EventM Int (T.Next CurrentState)
appEvent cs@(CS (Z.Zipper _ _ _) _) (T.VtyEvent e) =
    case e of
        V.EvKey V.KUp []         -> M.continue . moveUp $ cs
        V.EvKey (V.KChar 'k') [] -> M.continue . moveUp $ cs

        V.EvKey V.KDown []       -> M.continue . moveDown $ cs
        V.EvKey (V.KChar 'j') [] -> M.continue . moveDown $ cs

        V.EvKey V.KRight []      -> M.continue . moveRight $ cs
        V.EvKey (V.KChar 'l') [] -> M.continue . moveRight $ cs

        V.EvKey V.KLeft []       -> M.continue . moveLeft $ cs
        V.EvKey (V.KChar 'h') [] -> M.continue . moveLeft $ cs

        V.EvKey V.KEsc [] -> M.halt cs
        _ -> M.continue cs

moveUp :: CurrentState -> CurrentState
moveUp cs@(CS (Z.Zipper [] _ _) Lists) = cs
moveUp (CS (Z.Zipper left (TL name curr) right) Tasks) = CS (Z.Zipper left (TL name (Z.goLeft curr)) right) Tasks
moveUp (CS (Z.Zipper _ MyDay _) Tasks) = error "invariant violaton: focused on tasks for MyDay"
moveUp (CS (Z.Zipper _ AddList _) Tasks) = error "invariant violaton: focused on tasks for AddList"
moveUp (CS z focus) = CS (Z.goLeft z) focus

moveDown :: CurrentState -> CurrentState
moveDown cs@(CS (Z.Zipper _ _ []) Lists) = cs
moveDown (CS (Z.Zipper left (TL name curr) right) Tasks) = CS (Z.Zipper left (TL name (Z.goRight curr)) right) Tasks
moveDown (CS (Z.Zipper _ MyDay _) Tasks) = error "invariant violaton: focused on tasks for MyDay"
moveDown (CS (Z.Zipper _ AddList _) Tasks) = error "invariant violaton: focused on tasks for AddList"
moveDown (CS z focus) = CS (Z.goRight z) focus

moveLeft :: CurrentState -> CurrentState
moveLeft (CS z _) = CS z Lists

moveRight :: CurrentState -> CurrentState
moveRight (CS z@(Z.Zipper _ (TL _ _) _) _) = CS z Tasks
moveRight (CS z _) = CS z Lists

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

main :: IO ()
main = void $ M.defaultMain theApp initialState
