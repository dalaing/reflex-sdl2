{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
module Reflex.SDL.Host (
    SDLApp
  , sdlHost
  ) where

import           Data.Maybe               (isNothing, mapMaybe)

import           Control.Monad            (forM, unless)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.Ref
import           Control.Monad.Primitive
import           Data.Functor.Identity

import           Data.Dependent.Map       as M

import           Reflex
import           Reflex.Host.Class

import qualified SDL                      as S
import qualified SDL.Event                as SE

import           Reflex.SDL.Event

type SDLApp t m =
  ( MonadHold t m
  , Ref m ~ Ref IO
  , ReflexHost t
  , PrimMonad (HostFrame t)
  , MonadIO (HostFrame t)
  ) => EventSelector t SDLEvent
    -> PostBuildT t (PerformEventT t m) (Event t ())

newFanEventWithTriggerRef :: (MonadReflexCreateTrigger t m, MonadRef m, Ref m ~ Ref IO, GCompare k)
                          => m (EventSelector t k, Ref m (DMap k (EventTrigger t)))
newFanEventWithTriggerRef = do
  rt <- newRef empty
  es <- newFanEventWithTrigger $ \k t -> do
    modifyRef rt $ insert k t
    return $ modifyRef rt $ delete k
  return (es, rt)
{-# INLINE newFanEventWithTriggerRef #-}

findTrigger :: GCompare k
            => DMap k t
            -> DSum k f
            -> Maybe (DSum t f)
findTrigger m (k :=> v) =
  (:=> v) <$> M.lookup k m

sdlHost :: Maybe Int -> (forall t m. SDLApp t m) -> IO ()
sdlHost mFps guest =
  runSpiderHost $ do
    (ePostBuild, ePostBuildTriggerRef) <- newEventWithTriggerRef
    (eSdl, eSdlTriggerRef) <- newFanEventWithTriggerRef

    let
      g = guest eSdl

    (eQuit, FireCommand fire) <-
      hostPerformEventT .
      flip runPostBuildT ePostBuild $
      g

    hQuit <- subscribeEvent eQuit

    hasQuit <- newRef False

    let
      readPhase = readEvent hQuit >>= sequence

      continueWith a = do
        quit <- readRef hasQuit
        unless quit a

      fireOpen = do
        mPostBuildTrigger <- readRef ePostBuildTriggerRef
        q <- forM mPostBuildTrigger $ \t ->
          fire [t :=> Identity ()] readPhase
        unless (maybe True (all isNothing) q) $
          writeRef hasQuit True

      fireTick = do
        tick <- liftIO S.ticks
        sdlTriggers <- readRef eSdlTriggerRef
        q <- fire (mapMaybe (findTrigger sdlTriggers) [SDLTick :=> Identity tick]) readPhase
        unless (all isNothing q) $
          writeRef hasQuit True

      fireEvent e = do
        sdlTriggers <- readRef eSdlTriggerRef
        let triggers = mapMaybe (findTrigger sdlTriggers) [wrapEvent e]
        case triggers of
          [] -> return ()
          _ -> do
            q <- fire triggers readPhase
            unless (all isNothing q) $
              writeRef hasQuit True

      -- We _could_ only fire the tick event when we have
      -- events in this frame that we are registered for.
      --
      -- There are two problems with that.
      --
      -- If we are after tick-driven rendering, then we need
      -- to stay subscribed to the various window / WM events so
      -- that we re-render after our window is obscured / the user
      -- changes back and forth between some virtual desktops.
      --
      -- If we want things to happen only while events of interest are
      -- happening, we hit a snag if we're filtering events.
      -- For example, if we're looking for left mouse presses, we're also
      -- going to get right mouse releases causing a tick, since we're
      -- fanning the events to SDL granularity, but then the apps do
      -- further filtering on them.
      --
      -- The real solution would be to use a Dynamic for the game state,
      -- and `updated` for the rendering.
      loopFree = do
        e <- liftIO SE.waitEvent
        fireEvent e
        fireTick
        continueWith loopFree

      loopBoundInner targetTick = do
        currentTick <- liftIO S.ticks
        me <- if targetTick <= currentTick
              then return Nothing
              else S.waitEventTimeout . fromIntegral $ targetTick - currentTick
        case me of
          -- if Nothing then fire tick and return
          Nothing -> fireTick
          -- if Just then fire event and loop if not quit
          Just e -> do
            fireEvent e
            continueWith $ loopBoundInner targetTick

      loopBound fps = do
        currentTick <- liftIO S.ticks
        let targetTick = currentTick + (1000 `div` fromIntegral fps)
        loopBoundInner targetTick
        continueWith $ loopBound fps

      -- if no FPS is set, we use loopFree
      -- otherwise we use loopBound
      loop =
        maybe loopFree loopBound mFps

    fireOpen
    continueWith loop

    return ()
