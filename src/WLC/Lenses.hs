{-# LANGUAGE TemplateHaskell #-}
module WLC.Lenses where

import Control.Lens

import WLC

makeLenses ''WLCInterface
makeLenses ''WLCOutput
makeLenses ''WLCView
makeLenses ''WLCKeyboard
makeLenses ''WLCPointer
makeLenses ''WLCTouch
makeLenses ''WLCRequest
