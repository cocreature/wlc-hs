module WLC.Wrapper
  ( wrapCreated
  , wrapDestroyed
  , wrapFocus
  , wrapResolution
  , wrapMoveToOutput
  , wrapGeometry
  , wrapState
  , wrapKey
  , wrapButton
  , wrapScroll
  , wrapMotion
  , wrapTouch
  ) where

import Foreign.C.Types
import Foreign

import WLC

foreign import ccall "wrapper"
  wrapCreated :: (WLCHandle -> IO CBool) -> IO (FunPtr (WLCHandle -> IO CBool))

foreign import ccall "wrapper"
  wrapDestroyed :: (WLCHandle -> IO ()) -> IO (FunPtr (WLCHandle -> IO ()))

foreign import ccall "wrapper"
  wrapFocus :: (WLCHandle -> CBool -> IO ()) -> IO (FunPtr (WLCHandle -> CBool -> IO ()))

foreign import ccall safe "wrapper" 
  wrapResolution ::
    (WLCHandle -> WLCSizePtr -> WLCSizePtr -> IO ()) ->
    IO (FunPtr (WLCHandle -> WLCSizePtr -> WLCSizePtr -> IO ()))

foreign import ccall safe "wrapper"
  wrapMoveToOutput ::
    (WLCHandle -> WLCHandle -> WLCHandle -> IO ()) ->
    IO (FunPtr (WLCHandle -> WLCHandle -> WLCHandle -> IO ()))

foreign import ccall safe "wrapper"
  wrapGeometry ::
    (WLCHandle -> WLCGeometryPtr -> IO ()) ->
    IO (FunPtr (WLCHandle -> WLCGeometryPtr -> IO ()))

foreign import ccall safe "wrapper"
  wrapState ::
    (WLCHandle -> WLCViewStateBit -> CBool -> IO ()) ->
    IO (FunPtr (WLCHandle -> WLCViewStateBit -> CBool -> IO ()))

foreign import ccall safe "wrapper"
  wrapKey ::
    (WLCHandle -> CUInt -> WLCModifiersPtr -> CUInt -> CUInt -> WLCKeyStateBit -> IO CBool) ->
    IO (FunPtr (WLCHandle -> CUInt -> WLCModifiersPtr -> CUInt -> CUInt -> WLCKeyStateBit -> IO CBool))

foreign import ccall safe "wrapper"
  wrapButton ::
    (WLCHandle -> CUInt -> WLCModifiersPtr -> CUInt -> WLCButtonStateBit -> IO CBool) ->
    IO (FunPtr (WLCHandle -> CUInt -> WLCModifiersPtr -> CUInt -> WLCButtonStateBit -> IO CBool))

foreign import ccall safe "wrapper"
  wrapScroll ::
    (WLCHandle -> CUInt -> WLCModifiersPtr -> CUChar -> Ptr CDouble -> IO CBool) ->
    IO (FunPtr (WLCHandle -> CUInt -> WLCModifiersPtr -> CUChar -> Ptr CDouble -> IO CBool))

foreign import ccall safe "wrapper"
  wrapMotion ::
    (WLCHandle -> CUInt -> WLCOriginPtr -> IO CBool) ->
    IO (FunPtr (WLCHandle -> CUInt -> WLCOriginPtr -> IO CBool))

foreign import ccall safe "wrapper"
  wrapTouch ::
    (WLCHandle -> CUInt -> WLCModifiersPtr -> WLCTouchTypeBit -> CInt -> WLCOriginPtr -> IO CBool) ->
    IO (FunPtr (WLCHandle -> CUInt -> WLCModifiersPtr -> WLCTouchTypeBit -> CInt -> WLCOriginPtr -> IO CBool))
