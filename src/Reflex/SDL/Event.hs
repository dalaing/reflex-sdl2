{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Reflex.SDL.Event (
    SDLEvent(..)
  , wrapEvent
  ) where

import Data.Functor.Identity

import SDL
import Data.Word

import Data.GADT.Compare
import Data.GADT.Show
import Data.Dependent.Sum

-- | 'SDLEvent' is used to tag the various event payloads so that we can carry them around in a 'DSum' value.
--
-- The 'SDLTick' constructor is used to tag a synthetic "tick" event, with the intent that would be used to drive the rendering process.
--
-- The other constructors are used to pass along the various kinds of payloads that from the different kinds of 'Event'.
data SDLEvent a where
  SDLTick :: SDLEvent Word32
  SDLWindowShown :: SDLEvent WindowShownEventData
  SDLWindowHidden :: SDLEvent WindowHiddenEventData
  SDLWindowExposed :: SDLEvent WindowExposedEventData
  SDLWindowMoved :: SDLEvent WindowMovedEventData
  SDLWindowResized :: SDLEvent WindowResizedEventData
  SDLWindowSizeChanged :: SDLEvent WindowSizeChangedEventData
  SDLWindowMinimized :: SDLEvent WindowMinimizedEventData
  SDLWindowMaximized :: SDLEvent WindowMaximizedEventData
  SDLWindowRestored :: SDLEvent WindowRestoredEventData
  SDLWindowGainedMouseFocus :: SDLEvent WindowGainedMouseFocusEventData
  SDLWindowLostMouseFocus :: SDLEvent WindowLostMouseFocusEventData
  SDLWindowGainedKeyboardFocus :: SDLEvent WindowGainedKeyboardFocusEventData
  SDLWindowLostKeyboardFocus :: SDLEvent WindowLostKeyboardFocusEventData
  SDLWindowClosed :: SDLEvent WindowClosedEventData
  SDLKeyboard :: SDLEvent KeyboardEventData
  SDLTextEditing :: SDLEvent TextEditingEventData
  SDLTextInput :: SDLEvent TextInputEventData
  SDLMouseMotion :: SDLEvent MouseMotionEventData
  SDLMouseButton :: SDLEvent MouseButtonEventData
  SDLMouseWheel :: SDLEvent MouseWheelEventData
  SDLJoyAxis :: SDLEvent JoyAxisEventData
  SDLJoyBall :: SDLEvent JoyBallEventData
  SDLJoyHat :: SDLEvent JoyHatEventData
  SDLJoyButton :: SDLEvent JoyButtonEventData
  SDLJoyDevice :: SDLEvent JoyDeviceEventData
  SDLControllerAxis :: SDLEvent ControllerAxisEventData
  SDLControllerButton :: SDLEvent ControllerButtonEventData
  SDLControllerDevice :: SDLEvent ControllerDeviceEventData
  SDLQuit :: SDLEvent ()
  SDLUser :: SDLEvent UserEventData
  SDLSysWM :: SDLEvent SysWMEventData
  SDLTouchFinger :: SDLEvent TouchFingerEventData
  SDLMultiGesture :: SDLEvent MultiGestureEventData
  SDLDollarGesture :: SDLEvent DollarGestureEventData
  SDLDrop :: SDLEvent DropEventData
  SDLClipboardUpdate :: SDLEvent ClipboardUpdateEventData
  SDLUnknown :: SDLEvent UnknownEventData

-- | A helper function for use with the 'GEq' and 'GCompare' instances.
--
-- We need to be able to compare 'SDLEvent' values.  Since it is a sum
-- type then we come up with a canonical numbering of the constructors,
-- and then use that for the comparison.
numberEvent :: SDLEvent a
            -> Int
numberEvent e = case e of
  SDLTick -> 0
  SDLWindowShown -> 1
  SDLWindowHidden -> 2
  SDLWindowExposed -> 3
  SDLWindowMoved -> 4
  SDLWindowResized -> 5
  SDLWindowSizeChanged -> 6
  SDLWindowMinimized -> 7
  SDLWindowMaximized -> 8
  SDLWindowRestored -> 9
  SDLWindowGainedMouseFocus -> 10
  SDLWindowLostMouseFocus -> 11
  SDLWindowGainedKeyboardFocus -> 12
  SDLWindowLostKeyboardFocus -> 13
  SDLWindowClosed -> 14
  SDLKeyboard -> 15
  SDLTextEditing -> 16
  SDLTextInput -> 17
  SDLMouseMotion -> 18
  SDLMouseButton -> 19
  SDLMouseWheel -> 20
  SDLJoyAxis -> 21
  SDLJoyBall -> 22
  SDLJoyHat -> 23
  SDLJoyButton -> 24
  SDLJoyDevice -> 25
  SDLControllerAxis -> 26
  SDLControllerButton -> 27
  SDLControllerDevice -> 28
  SDLQuit -> 29
  SDLUser -> 30
  SDLSysWM -> 31
  SDLTouchFinger -> 32
  SDLMultiGesture -> 33
  SDLDollarGesture -> 34
  SDLDrop -> 35
  SDLClipboardUpdate -> 36
  SDLUnknown -> 37

instance GEq SDLEvent where
  geq a b =
    if numberEvent a /= numberEvent b
    then Nothing
    else case (a, b) of
      (SDLTick, SDLTick) ->
        Just Refl
      (SDLWindowShown, SDLWindowShown) ->
        Just Refl
      (SDLWindowHidden, SDLWindowHidden) ->
        Just Refl
      (SDLWindowExposed, SDLWindowExposed) ->
        Just Refl
      (SDLWindowMoved, SDLWindowMoved) ->
        Just Refl
      (SDLWindowResized, SDLWindowResized) ->
        Just Refl
      (SDLWindowSizeChanged, SDLWindowSizeChanged) ->
        Just Refl
      (SDLWindowMinimized, SDLWindowMinimized) ->
        Just Refl
      (SDLWindowMaximized, SDLWindowMaximized) ->
        Just Refl
      (SDLWindowRestored, SDLWindowRestored) ->
        Just Refl
      (SDLWindowGainedMouseFocus, SDLWindowGainedMouseFocus) ->
        Just Refl
      (SDLWindowLostMouseFocus, SDLWindowLostMouseFocus) ->
        Just Refl
      (SDLWindowGainedKeyboardFocus, SDLWindowGainedKeyboardFocus) ->
        Just Refl
      (SDLWindowLostKeyboardFocus, SDLWindowLostKeyboardFocus) ->
        Just Refl
      (SDLWindowClosed, SDLWindowClosed) ->
        Just Refl
      (SDLKeyboard, SDLKeyboard) ->
        Just Refl
      (SDLTextEditing, SDLTextEditing) ->
        Just Refl
      (SDLTextInput, SDLTextInput) ->
        Just Refl
      (SDLMouseMotion, SDLMouseMotion) ->
        Just Refl
      (SDLMouseButton, SDLMouseButton) ->
        Just Refl
      (SDLMouseWheel, SDLMouseWheel) ->
        Just Refl
      (SDLJoyAxis, SDLJoyAxis) ->
        Just Refl
      (SDLJoyBall, SDLJoyBall) ->
        Just Refl
      (SDLJoyHat, SDLJoyHat) ->
        Just Refl
      (SDLJoyButton, SDLJoyButton) ->
        Just Refl
      (SDLJoyDevice, SDLJoyDevice) ->
        Just Refl
      (SDLControllerAxis, SDLControllerAxis) ->
        Just Refl
      (SDLControllerButton, SDLControllerButton) ->
        Just Refl
      (SDLControllerDevice, SDLControllerDevice) ->
        Just Refl
      (SDLQuit, SDLQuit) ->
        Just Refl
      (SDLUser, SDLUser) ->
        Just Refl
      (SDLSysWM, SDLSysWM) ->
        Just Refl
      (SDLTouchFinger, SDLTouchFinger) ->
        Just Refl
      (SDLMultiGesture, SDLMultiGesture) ->
        Just Refl
      (SDLDollarGesture, SDLDollarGesture) ->
        Just Refl
      (SDLDrop, SDLDrop) ->
        Just Refl
      (SDLClipboardUpdate, SDLClipboardUpdate) ->
        Just Refl
      (SDLUnknown, SDLUnknown) ->
        Just Refl
      _ ->
        Nothing

instance GCompare SDLEvent where
  gcompare a b =
    case geq a b of
      Just Refl -> GEQ
      Nothing ->
        case compare (numberEvent a) (numberEvent b) of
          LT -> GLT
          GT -> GGT
          EQ -> error "instance GCompare SDLEvent: this should not happen"

instance GShow SDLEvent where
  gshowsPrec _ a = case a of
    SDLTick ->
      showString "Tick"
    SDLWindowShown ->
      showString "WindowShown"
    SDLWindowHidden ->
      showString "WindowHidden"
    SDLWindowExposed ->
      showString "WindowExposed"
    SDLWindowMoved ->
      showString "WindowMoved"
    SDLWindowResized ->
      showString "WindowResized"
    SDLWindowSizeChanged ->
      showString "WindowSizeChanged"
    SDLWindowMinimized ->
      showString "WindowMinimized"
    SDLWindowMaximized ->
      showString "WindowMaximized"
    SDLWindowRestored ->
      showString "WindowRestored"
    SDLWindowGainedMouseFocus ->
      showString "WindowGainedMouseFocus"
    SDLWindowLostMouseFocus ->
      showString "WindowLostMouseFocus"
    SDLWindowGainedKeyboardFocus ->
      showString "WindowGainedKeyboardFocus"
    SDLWindowLostKeyboardFocus ->
      showString "WindowLostKeyboardFocus"
    SDLWindowClosed ->
      showString "WindowClosed"
    SDLKeyboard ->
      showString "Keyboard"
    SDLTextEditing ->
      showString "TextEditing"
    SDLTextInput ->
      showString "TextInput"
    SDLMouseMotion ->
      showString "MouseMotion"
    SDLMouseButton ->
      showString "MouseButton"
    SDLMouseWheel ->
      showString "MouseWheel"
    SDLJoyAxis ->
      showString "JoyAxis"
    SDLJoyBall ->
      showString "JoyBall"
    SDLJoyHat ->
      showString "JoyHat"
    SDLJoyButton ->
      showString "JoyButton"
    SDLJoyDevice ->
      showString "JoyDevice"
    SDLControllerAxis ->
      showString "ControllerAxis"
    SDLControllerButton ->
      showString "ControllerButton"
    SDLControllerDevice ->
      showString "ControllerDevice"
    SDLQuit ->
      showString "Quit"
    SDLUser ->
      showString "User"
    SDLSysWM ->
      showString "SysWM"
    SDLTouchFinger ->
      showString "TouchFinger"
    SDLMultiGesture ->
      showString "MultiGesture"
    SDLDollarGesture ->
      showString "DollarGesture"
    SDLDrop ->
      showString "Drop"
    SDLClipboardUpdate ->
      showString "ClipboardUpdate"
    SDLUnknown ->
      showString "Unknown"

instance ShowTag SDLEvent Identity where
  showTaggedPrec t n (Identity a) = case t of
    SDLTick ->
      showsPrec n a
    SDLWindowShown ->
      showsPrec n a
    SDLWindowHidden ->
      showsPrec n a
    SDLWindowExposed ->
      showsPrec n a
    SDLWindowMoved ->
      showsPrec n a
    SDLWindowResized ->
      showsPrec n a
    SDLWindowSizeChanged ->
      showsPrec n a
    SDLWindowMinimized ->
      showsPrec n a
    SDLWindowMaximized ->
      showsPrec n a
    SDLWindowRestored ->
      showsPrec n a
    SDLWindowGainedMouseFocus ->
      showsPrec n a
    SDLWindowLostMouseFocus ->
      showsPrec n a
    SDLWindowGainedKeyboardFocus ->
      showsPrec n a
    SDLWindowLostKeyboardFocus ->
      showsPrec n a
    SDLWindowClosed ->
      showsPrec n a
    SDLKeyboard ->
      showsPrec n a
    SDLTextEditing ->
      showsPrec n a
    SDLTextInput ->
      showsPrec n a
    SDLMouseMotion ->
      showsPrec n a
    SDLMouseButton ->
      showsPrec n a
    SDLMouseWheel ->
      showsPrec n a
    SDLJoyAxis ->
      showsPrec n a
    SDLJoyBall ->
      showsPrec n a
    SDLJoyHat ->
      showsPrec n a
    SDLJoyButton ->
      showsPrec n a
    SDLJoyDevice ->
      showsPrec n a
    SDLControllerAxis ->
      showsPrec n a
    SDLControllerButton ->
      showsPrec n a
    SDLControllerDevice ->
      showsPrec n a
    SDLQuit ->
      showsPrec n a
    SDLUser ->
      showsPrec n a
    SDLSysWM ->
      showsPrec n a
    SDLTouchFinger ->
      showsPrec n a
    SDLMultiGesture ->
      showsPrec n a
    SDLDollarGesture ->
      showsPrec n a
    SDLDrop ->
      showsPrec n a
    SDLClipboardUpdate ->
      showsPrec n a
    SDLUnknown ->
      showsPrec n a

-- | Converts and 'Event' from the 'sdl' package into a 'DSum' tagged with the 'SDLEvent' type.
--
-- Each of the constructors of 'SDLEvent' has a different type, but 'DSum' lets us carry them
-- all around together.
wrapEvent :: Event
          -> DSum SDLEvent Identity
wrapEvent (Event _ e) = case e of
  WindowShownEvent p ->
    SDLWindowShown :=> Identity p
  WindowHiddenEvent p ->
    SDLWindowHidden :=> Identity p
  WindowExposedEvent p ->
    SDLWindowExposed :=> Identity p
  WindowMovedEvent p ->
    SDLWindowMoved :=> Identity p
  WindowResizedEvent p ->
    SDLWindowResized :=> Identity p
  WindowSizeChangedEvent p ->
    SDLWindowSizeChanged :=> Identity p
  WindowMinimizedEvent p ->
    SDLWindowMinimized :=> Identity p
  WindowMaximizedEvent p ->
    SDLWindowMaximized :=> Identity p
  WindowRestoredEvent p ->
    SDLWindowRestored :=> Identity p
  WindowGainedMouseFocusEvent p ->
    SDLWindowGainedMouseFocus :=> Identity p
  WindowLostMouseFocusEvent p ->
    SDLWindowLostMouseFocus :=> Identity p
  WindowGainedKeyboardFocusEvent p ->
    SDLWindowGainedKeyboardFocus :=> Identity p
  WindowLostKeyboardFocusEvent p ->
    SDLWindowLostKeyboardFocus :=> Identity p
  WindowClosedEvent p ->
    SDLWindowClosed :=> Identity p
  KeyboardEvent p ->
    SDLKeyboard :=> Identity p
  TextEditingEvent p ->
    SDLTextEditing :=> Identity p
  TextInputEvent p ->
    SDLTextInput :=> Identity p
  MouseMotionEvent p ->
    SDLMouseMotion :=> Identity p
  MouseButtonEvent p ->
    SDLMouseButton :=> Identity p
  MouseWheelEvent p ->
    SDLMouseWheel :=> Identity p
  JoyAxisEvent p ->
    SDLJoyAxis :=> Identity p
  JoyBallEvent p ->
    SDLJoyBall :=> Identity p
  JoyHatEvent p ->
    SDLJoyHat :=> Identity p
  JoyButtonEvent p ->
    SDLJoyButton :=> Identity p
  JoyDeviceEvent p ->
    SDLJoyDevice :=> Identity p
  ControllerAxisEvent p ->
    SDLControllerAxis :=> Identity p
  ControllerButtonEvent p ->
    SDLControllerButton :=> Identity p
  ControllerDeviceEvent p ->
    SDLControllerDevice :=> Identity p
  QuitEvent ->
    SDLQuit :=> Identity ()
  UserEvent p ->
    SDLUser :=> Identity p
  SysWMEvent p ->
    SDLSysWM :=> Identity p
  TouchFingerEvent p ->
    SDLTouchFinger :=> Identity p
  MultiGestureEvent p ->
    SDLMultiGesture :=> Identity p
  DollarGestureEvent p ->
    SDLDollarGesture :=> Identity p
  DropEvent p ->
    SDLDrop :=> Identity p
  ClipboardUpdateEvent p ->
    SDLClipboardUpdate :=> Identity p
  UnknownEvent p ->
    SDLUnknown :=> Identity p
