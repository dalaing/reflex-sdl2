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
module SDLEventLoop (
    SDLApp
  , sdlHost
  ) where

import           Data.Maybe               (isNothing, isJust, mapMaybe)

import           Control.Concurrent       (forkIO, killThread, threadDelay)
import           Control.Concurrent.MVar  (MVar, newEmptyMVar, tryPutMVar,
                                           tryTakeMVar)
import           Control.Monad            (forM, void, forever, unless, when)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.Ref
import           Data.Functor.Identity

import           Data.Dependent.Map       as M
import           Data.Dependent.Sum

import           Reflex
import           Reflex.Host.Class
import           Reflex.PerformEvent.Base

import qualified SDL                      as S
import qualified SDL.Event                as SE

import           SDLEvent

type SDLApp t m =
  ( Reflex t
  , MonadHold t m
  , Ref m ~ Ref IO
  , ReflexHost t
  , MonadRef (HostFrame t)
  , Ref (HostFrame t) ~ Ref IO
  , MonadIO (HostFrame t)
  ) => EventSelector t SDLEvent
    -> PostBuildT t (PerformEventT t m) (Event t ())

{-
sdlHostOld :: S.Renderer -> Maybe Int -> (forall t m. SDLApp t m) -> IO ()
sdlHostOld r mFps guest =
  runSpiderHost $ do
    (ePostBuild, ePostBuildTriggerRef) <- newEventWithTriggerRef
    (eTick, eTickTriggerRef) <- newEventWithTriggerRef
    (eSdl, eSdlTriggerRef) <- newEventWithTriggerRef

    let
      eTick' = (\t -> [SDLTick :=> Identity t]) <$> eTick
      eSdl' = (\e -> [wrapEvent e]) <$> eSdl
      g = guest . fan . fmap fromList $ mergeWith (++) [eTick', eSdl']

    (eQuit, FireCommand fire) <-
      hostPerformEventT .
      flip runPostBuildT ePostBuild .
      flip runReaderT r $
      g

    hQuit <- subscribeEvent eQuit

    let
      readPhase = readEvent hQuit >>= sequence
      canContinue = maybe True (all isNothing)

      loop = do
        tStart <- liftIO S.ticks

        es <- liftIO SE.pollEvents

        mESdlTrigger <- readRef eSdlTriggerRef
        quit <- forM mESdlTrigger $ \t ->
          traverse (\e -> fire [t :=> Identity e] readPhase) es

        tEnd <- liftIO S.ticks

        let
          tElapsed = fromIntegral $ 1000 * (tEnd - tStart)
          tTarget = maybe tElapsed (1000000 `div`) mFps
        when (tElapsed < tTarget) $ liftIO $ threadDelay (tTarget - tElapsed)

        mETickTrigger <- readRef eTickTriggerRef
        q <- forM mETickTrigger $ \t ->
          fire [t :=> Identity tEnd] readPhase

        when (isNothing q && maybe True (all isNothing . mconcat) quit) loop

    mPostBuildTrigger <- readRef ePostBuildTriggerRef
    quit <- forM mPostBuildTrigger $ \t ->
      fire [t :=> Identity ()] readPhase
    when (canContinue quit) loop

    return ()
-}

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
      loopFreeBody = do
        e <- liftIO SE.waitEvent
        sdlTriggers <- readRef eSdlTriggerRef
        let triggers = mapMaybe (findTrigger sdlTriggers) [wrapEvent e]
        case triggers of
          [] -> return ()
          _ -> do
            q <- fire triggers readPhase
            unless (all isNothing q) $
              writeRef hasQuit True

      loopFree = do
        loopFreeBody
        fireTick
        continueWith loopFree

      -- timerLoop :: Int -> IO ThreadId
      -- we can kill this using killThread on the returned ThreadId
      timerLoop mTick delay = forkIO . forever $ do
        threadDelay delay
        void $ tryPutMVar mTick ()

      loopBoundBody = do
        es <- liftIO SE.pollEvents
        sdlTriggers <- readRef eSdlTriggerRef
        q <- traverse (\e -> fire (mapMaybe (findTrigger sdlTriggers) [wrapEvent e]) readPhase) es

        unless (all isNothing  . mconcat $ q) $ writeRef hasQuit True

      loopBoundStart fps = do
        mTick <- liftIO newEmptyMVar
        tId <- liftIO . timerLoop mTick $ 1000000 `div` fps
        loopBound mTick
        liftIO $ killThread tId

      loopBound mTick = do
        loopBoundBody
        t <- liftIO $ tryTakeMVar mTick
        when (isJust t) fireTick
        continueWith $ loopBound mTick

      -- if no FPS is set, we use loopFree
      -- otherwise we use loopBound
      loop =
        maybe loopFree loopBoundStart mFps

    fireOpen
    continueWith loop

    return ()
