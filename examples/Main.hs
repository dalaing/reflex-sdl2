{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Int
import Data.Foldable (traverse_)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)

import Reflex

import SDLEvent
import SDLEventLoop

import SDL hiding (Event(..))

import Linear
import Linear.Affine
import Foreign.C.Types (CInt(..))

data GameState = GameState {
    points :: [Point V2 Int32]
  }

addPoint :: Point V2 Int32 -> GameState -> GameState
addPoint p (GameState ps) =
  GameState (take 10 (p : ps))

renderBlock :: Renderer -> Point V2 Int32 -> IO ()
renderBlock r (P (V2 x y)) = do
  rendererDrawColor r $= V4 255 0 0 255
  fillRect r . Just . Rectangle (P (V2 (CInt x) (CInt y))) $ V2 (CInt 64) (CInt 64)

renderGameState :: Renderer -> GameState -> IO ()
renderGameState r (GameState ps) = do
  traverse_ (renderBlock r) ps 

render :: Renderer -> GameState -> IO ()
render r gs = do
  rendererDrawColor r $= V4 0 0 0 0 
  clear r
  renderGameState r gs
  present r

guest :: SDLApp t m
guest sel = do
  ePostBuild <- getPostBuild

  let 
    eMouseButton = 
      fmap mouseButtonEventPos . 
      ffilter ((== Pressed) . mouseButtonEventMotion) . 
      ffilter ((== ButtonLeft) . mouseButtonEventButton) . 
      select sel $ 
      SDLMouseButton

    eQuit = 
      fmap (const ()) . 
      ffilter ((== KeycodeQ) . keysymKeycode . keyboardEventKeysym) .
      select sel $
      SDLKeyboard

    eTime =
      select sel $
      SDLTimestamp

  eGameState <- accum (flip ($)) (GameState []) . leftmost $ [
      id <$ ePostBuild
    , addPoint <$> eMouseButton
    ]

  r <- ask

  performEvent_ $ liftIO . render r <$> eGameState
  performEvent_ $ liftIO quit       <$  eQuit
  performEvent_ $ liftIO . print    <$> eTime

  return eQuit

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  sdlHost renderer guest
