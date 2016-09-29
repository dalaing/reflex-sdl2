{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module SDLEventLoop (
    SDLApp
  , sdlHost
  ) where

import Data.Maybe (isNothing, mapMaybe)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (forever, when, forM, void)
import Control.Monad.Ref
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader
import Control.Concurrent (threadDelay)
import Data.Functor.Identity

import Data.GADT.Show
import Data.Dependent.Sum
import Data.Dependent.Map as M

import Reflex
import Reflex.Host.Class
import Reflex.PerformEvent.Base
import Reflex.PostBuild.Class

import qualified SDL as S
import qualified SDL.Event as SE

import SDLEvent

type SDLApp t m =
  ( Reflex t
  , MonadHold t m
  , Ref m ~ Ref IO
  , ReflexHost t
  , MonadRef (HostFrame t)
  , Ref (HostFrame t) ~ Ref IO
  , MonadIO (HostFrame t)
  ) => EventSelector t SDLEvent
    -> ReaderT S.Renderer (PostBuildT t (PerformEventT t m)) (Event t ())

sdlHostOld :: S.Renderer -> Maybe Int -> (forall t m. SDLApp t m) -> IO ()
sdlHostOld r mFps guest =
  runSpiderHost $ do
    (ePostBuild, ePostBuildTriggerRef) <- newEventWithTriggerRef
    (eSdl, eSdlTriggerRef) <- newEventWithTriggerRef

    let
      g = guest (fan $ wrapEvent <$> eSdl)

    (eQuit, FireCommand fire) <- hostPerformEventT $ runPostBuildT (runReaderT g r) ePostBuild
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

sdlHost :: S.Renderer -> Maybe Int -> (forall t m. SDLApp t m) -> IO ()
sdlHost r mFps guest =
  runSpiderHost $ do
    (ePostBuild, ePostBuildTriggerRef) <- newEventWithTriggerRef
    (eSdl, eSdlTriggerRef) <- newFanEventWithTriggerRef

    let
      g = guest eSdl

    (eQuit, FireCommand fire) <- hostPerformEventT $ runPostBuildT (runReaderT g r) ePostBuild
    hQuit <- subscribeEvent eQuit

    let
      readPhase = readEvent hQuit >>= sequence
      canContinue = maybe True (all isNothing)

      loop = do
        tStart <- liftIO S.ticks

        es <- liftIO SE.pollEvents

        let ses = wrapEvent <$> es
        sdlTriggers <- readRef eSdlTriggerRef
        let triggers = fmap pure . mapMaybe (findTrigger sdlTriggers) $ ses
        quit <- traverse (\t -> fire t readPhase) triggers

        tEnd <- liftIO S.ticks

        let
          tElapsed = fromIntegral $ tEnd - tStart
          tTarget = maybe tElapsed (1000 `div`) mFps
        when (tElapsed < tTarget) $ liftIO $ S.delay . fromIntegral $ (tTarget - tElapsed)

        q <- fire (mapMaybe (findTrigger sdlTriggers) [SDLTick :=> Identity tEnd]) readPhase

        when (all isNothing q && (all isNothing . mconcat) quit) loop

    mPostBuildTrigger <- readRef ePostBuildTriggerRef
    quit <- forM mPostBuildTrigger $ \t ->
      fire [t :=> Identity ()] readPhase
    when (canContinue quit) loop

    return ()
