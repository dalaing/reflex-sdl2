{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Int
import Data.Word
import Data.Foldable (traverse_)

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Reflex

import Reflex.SDL.Event
import Reflex.SDL.Host

import SDL hiding (Event(..))

import Linear
import Linear.Affine
import Foreign.C.Types (CInt(..))

colours :: Int -> V4 Word8
colours 0 = V4 0 0 255 255
colours 1 = V4 0 255 255 255
colours 2 = V4 0 255 0 255
colours 3 = V4 255 255 0 255
colours 4 = V4 255 0 0 255
colours _ = V4 255 0 255 255

cycleColourIndexLeft :: Int -> Int
cycleColourIndexLeft i
  | i < 0 || i > 5 = 0
  | otherwise = (i - 1) `mod` 6

cycleColourIndexRight :: Int -> Int
cycleColourIndexRight i
  | i < 0 || i > 5 = 0
  | otherwise = (i + 1) `mod` 6

data Block = Block {
    bColourIndex :: Int
  , bRect :: Rectangle CInt
  }

mkBlock :: Int -> Point V2 Int32 -> Block
mkBlock i (P (V2 x y)) =
  let
    r = Rectangle (P (V2 (CInt x) (CInt y))) $ V2 (CInt 64) (CInt 64)
  in
    Block i r

renderBlock :: Renderer -> Block -> IO ()
renderBlock r (Block i rect) = do
  rendererDrawColor r $= colours i
  fillRect r . Just $ rect

data GameState = GameState {
    blocks :: [Block]
  }

addBlock :: Int -> Int -> Point V2 Int32 -> GameState -> GameState
addBlock n c p (GameState bs) =
  GameState (take n (mkBlock c p : bs))

trimBlocks :: Int -> GameState -> GameState
trimBlocks n (GameState bs) =
  GameState (take n bs)


-- Useful for debugging the FPS capping code
{-
cycleBlocks :: GameState -> GameState
cycleBlocks (GameState bs) =
    GameState . fmap cycleBlock $ bs
  where
    cycleBlock (Block i r) =
      Block (cycleColourIndexRight i) r
-}

renderGameState :: Renderer -> GameState -> IO ()
renderGameState r (GameState bs) = do
  traverse_ (renderBlock r) bs

render :: Renderer -> GameState -> IO ()
render r gs = do
  rendererDrawColor r $= V4 0 0 0 0
  clear r
  renderGameState r gs
  present r

guest :: Renderer -> SDLApp t m
guest r sel = do
  ePostBuild <- getPostBuild

  let
    eMouseButton =
      fmap mouseButtonEventPos .
      ffilter ((== Pressed) . mouseButtonEventMotion) .
      ffilter ((== ButtonLeft) . mouseButtonEventButton) .
      select sel $
      SDLMouseButton

    eKey =
      fmap (keysymKeycode . keyboardEventKeysym) .
      ffilter ((== Pressed) . keyboardEventKeyMotion) .
      select sel $
      SDLKeyboard

    eLimitUp     = void . ffilter (== KeycodeW) $ eKey
    eLimitDown   = void . ffilter (== KeycodeS) $ eKey
    eColourLeft  = void . ffilter (== KeycodeA) $ eKey
    eColourRight = void . ffilter (== KeycodeD) $ eKey
    eQuit        = void . ffilter (== KeycodeQ) $ eKey

    -- This is odd in the case where we're not capping the fps
    -- We get a tick for every SDL event that we're subscribed to,
    -- even if we're not interested in them (ie mouse moves and presses
    -- of unknown keys)
    eTick = select sel SDLTick

  dLimit <- accum (flip ($)) 10 . leftmost $ [
                succ <$ eLimitUp
    , (max 0 . pred) <$ eLimitDown
    ]

  bIndex <- accum (flip ($)) 0 . leftmost $ [
      cycleColourIndexLeft  <$ eColourLeft
    , cycleColourIndexRight <$ eColourRight
    ]

  -- bGameState <- accum (flip ($)) (GameState []) . leftmost $ [
  (bGameState :: Behavior t GameState) <- accum (flip ($)) (GameState []) . leftmost $ [
      id <$ ePostBuild
    -- This relies on https://github.com/reflex-frp/reflex/pull/66 
    -- , addBlock <$> current dLimit <*> bIndex <@> eMouseButton
    , attachWith (\l (i, p) -> addBlock l i p) (current dLimit) $ attachWith (,) bIndex eMouseButton
    , trimBlocks <$> updated dLimit
    -- , cycleBlocks <$ eTick
    ]

  -- This relies on https://github.com/reflex-frp/reflex/pull/66 
  -- performEvent_ $ liftIO . render r <$> bGameState <@ eTick
  performEvent_ $ liftIO . render r <$> tag bGameState eTick

  -- We could have accumulated the game state into an event and done:
  --   performEvent_ $ liftIO . render r <$> eGameState
  -- but then we'd also need to gather events in order to re-render
  -- whenever other windows were dragged over the top of this window,
  -- etc...
  -- It is simpler to just render every tick

  performEvent_ $ liftIO quit       <$  eQuit

  return eQuit

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  sdlHost (Just 30) $ guest renderer
