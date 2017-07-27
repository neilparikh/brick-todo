#! /usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

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
  , vBox
  , hBox
  , withAttr
  )
import Brick.Util (on)

data Task = Task String Bool -- name and if it is on the day list
    deriving Show

data TodoList = TL String [Task]
              | MyDay
              | AddList
    deriving Show

data Focus = Lists
           | Tasks
           deriving Eq

data CurrentState = CS [TodoList] TodoList [TodoList] Focus
    -- deriving Show

listToName :: TodoList -> String
listToName (TL name _) = name
listToName MyDay = "My Day"
listToName AddList = "Add List [+]"

taskToName :: Task -> String
taskToName (Task name _) = name

getCurrentTasks :: CurrentState -> [Task]
getCurrentTasks cs@(CS _ curr _ _) = case curr of
    MyDay -> filter isOnDay . concatMap getTasks . flattenState $ cs
    AddList -> []
    TL _ tasks -> tasks
    where
    isOnDay (Task _ onDay) = onDay

foo :: Task
foo = Task "Foo" True

bar :: Task
bar = Task "Bar" True

baz :: Task
baz = Task "Baz" False

initialState :: CurrentState
initialState = CS [MyDay] (TL "Todo" [foo]) [(TL "Work" [bar]), (TL "Projects" [baz]), AddList] Lists

flattenState :: CurrentState -> [TodoList]
flattenState (CS left curr right _) = (reverse left) ++ (curr:right)

getTasks :: TodoList -> [Task]
getTasks MyDay = []
getTasks AddList = []
getTasks (TL _ tasks) = tasks

addTaskToList :: TodoList -> Task -> TodoList
addTaskToList (TL name tasks) task = TL name (task:tasks)

drawUI :: CurrentState -> [Widget Int]
drawUI cs@(CS left curr right focus) = [ui]
    where
    -- listsWidget :: L.List Int String
    listsWidget = L.listMoveBy (length left) $ L.list 1 (Vec.fromList . map listToName . flattenState $ cs) 1
    tasksWidget = L.list 2 (Vec.fromList . map taskToName . getCurrentTasks $ cs) 1
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
appEvent cs@(CS _ _ _ focus) (T.VtyEvent e) =
    case e of
        -- FIXME: up/down is disabled when the focus is on tasks
        -- correct fix is to move through the task list
        V.EvKey V.KUp [] | focus == Lists -> M.continue . moveUp $ cs
        V.EvKey (V.KChar 'k') [] | focus == Lists -> M.continue . moveUp $ cs

        V.EvKey V.KDown [] | focus == Lists -> M.continue . moveDown $ cs
        V.EvKey (V.KChar 'j') [] | focus == Lists -> M.continue . moveDown $ cs

        V.EvKey V.KRight [] -> M.continue . moveRight $ cs
        V.EvKey (V.KChar 'l') [] -> M.continue . moveRight $ cs

        V.EvKey V.KLeft [] -> M.continue . moveLeft $ cs
        V.EvKey (V.KChar 'h') [] -> M.continue . moveLeft $ cs

        V.EvKey V.KEsc [] -> M.halt cs
        _ -> M.continue cs

moveUp :: CurrentState -> CurrentState
moveUp cs@(CS [] curr right _) = cs
moveUp (CS (x:left) curr right focus) = CS left x (curr:right) focus

moveDown :: CurrentState -> CurrentState
moveDown cs@(CS left curr [] _) = cs
moveDown (CS left curr (x:right) focus) = CS (curr:left) x right focus

moveLeft :: CurrentState -> CurrentState
moveLeft (CS left curr right _) = CS left curr right Lists

moveRight :: CurrentState -> CurrentState
moveRight (CS left curr right _) = CS left curr right Tasks

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
