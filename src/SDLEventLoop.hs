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

import Data.Maybe (isNothing, fromMaybe, mapMaybe)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (forever, when, forM, void)
import Control.Monad.Ref
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader
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
  , MonadFix m
  , MonadRef m
  , Ref m ~ Ref IO
  , ReflexHost t
  , MonadRef (HostFrame t)
  , Ref (HostFrame t) ~ Ref IO
  , MonadIO (HostFrame t)
  ) => EventSelector t SDLEvent
    -> ReaderT S.Renderer (PostBuildT t (PerformEventT t m)) (Event t ())

sdlHost :: S.Renderer -> (forall t m. SDLApp t m) -> IO ()
sdlHost r myGuest = do
  runSpiderHost $ do
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
    (eSdl, eSdlTriggerRef) <- newEventWithTriggerRef

    let 
      g = myGuest (fan $ wrapEvent <$> eSdl)

    (eQuit, FireCommand fire) <- hostPerformEventT $ runPostBuildT (runReaderT g r) postBuild
    hQuit <- subscribeEvent eQuit

    let 
      readPhase = readEvent hQuit >>= sequence

      canContinue = maybe True (all isNothing)

      loop = do
        e <- liftIO SE.waitEvent
        mESdlTrigger <- readRef eSdlTriggerRef
        quit <- forM mESdlTrigger $ \t -> 
          fire [t :=> Identity e] $ readPhase
        when (canContinue quit) loop

    mPostBuildTrigger <- readRef postBuildTriggerRef
    postBuildQuit <- forM mPostBuildTrigger $ \t ->
      fire [t :=> Identity ()] $ readPhase

    when (canContinue postBuildQuit) loop

    return ()

{-
newFanEventWithTriggerRef :: (MonadReflexCreateTrigger t m, MonadRef m, Ref m ~ Ref IO, GCompare k) 
                          => m (EventSelector t k, Ref m (DMap k (EventTrigger t)))
newFanEventWithTriggerRef = do
  rt <- newRef empty
  es <- newFanEventWithTrigger $ \k t -> do
    modifyRef rt $ insert k t
    return . modifyRef rt $ delete k
  return (es, rt)
{-# INLINE newFanEventWithTriggerRef #-}

newSDLEventWithTriggerRef :: (MonadReflexCreateTrigger t m, MonadRef m, Ref m ~ Ref IO) 
                          => m (EventSelector t SDLEvent, Ref m (DMap SDLEvent (EventTrigger t)))
newSDLEventWithTriggerRef = do
  rt <- newRef empty
  es <- newFanEventWithTrigger $ \k t -> 
    case k of
      SDLTimestamp -> do
        return $ return ()
      _ -> do
        modifyRef rt $ insert k t
        return . modifyRef rt $ delete k
  return (es, rt)
{-# INLINE newSDLEventWithTriggerRef #-}

findTrigger :: DMap SDLEvent t 
            -> DSum SDLEvent Identity 
            -> Maybe (DSum t Identity)
findTrigger m (k :=> v) = 
  (\t -> t :=> v) <$> M.lookup k m

sdlHostNew :: S.Renderer -> (forall t m. SDLApp t m) -> IO ()
sdlHostNew r myGuest = do
  runSpiderHost $ do
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
    (eSdl, eSdlTriggerRef) <- newSDLEventWithTriggerRef

    let 
      g = myGuest eSdl

    (eQuit, FireCommand fire) <- hostPerformEventT $ runPostBuildT (runReaderT g r) postBuild
    hQuit <- subscribeEvent eQuit

    let 
      readPhase = readEvent hQuit >>= sequence
      canContinue = maybe True (all isNothing)
      loop = do
        e <- liftIO SE.waitEvent
        let se = wrapEvent e
        sdlTriggers <- readRef eSdlTriggerRef
        quit <- fire (mapMaybe (findTrigger sdlTriggers) . toList $ se) $ readPhase
        when (all isNothing quit) loop

    mPostBuildTrigger <- readRef postBuildTriggerRef
    postBuildQuit <- forM mPostBuildTrigger $ \t ->
      fire [t :=> Identity ()] $ readPhase
   
    when (canContinue postBuildQuit) loop

    return ()
-}
