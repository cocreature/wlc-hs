{-# LANGUAGE ForeignFunctionInterface #-}

module WLC
  ( WLCGeometry(..)
  , WLCInterface(..)
  , WLCKeyState(..)
  , WLCKeyboard(..)
  , WLCModifiers(..)
  , WLCOrigin(..)
  , WLCOutput(..)
  , WLCPointer(..)
  , WLCRequest(..)
  , WLCSize(..)
  , WLCTouch(..)
  , WLCView(..)
  , WLCViewState(..)
  , WLCGeometryPtr
  , WLCModifiersPtr
  , WLCOriginPtr
  , WLCSizePtr
  , CBool
  , WLCHandle
  , WLCOutputPtr(..)
  , WLCViewPtr(..)
  , WLCModifier(..)
  , WLCButtonStateBit(..)
  , WLCKeyStateBit(..)
  , WLCModifierBit(..)
  , WLCTouchTypeBit(..)
  , WLCViewStateBit(..)
  , wlcInit
  , wlcOutputGetMask
  , wlcOutputGetViews
  , wlcOutputGetResolution
  , wlcOutputFocus
  , wlcOutputSetMask
  , wlcRun
  , wlcTerminate
  , wlcViewBringToFront
  , wlcViewClose
  , wlcViewFocus
  , wlcViewGetMask
  , wlcViewGetOutput
  , wlcViewSetGeometry
  , wlcViewSetMask
  , wlcViewSetState
  ) where

import Control.Lens
import Data.Default
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Text.PrettyPrint.HughesPJClass
import System.IO.Unsafe

#include <wlc/wlc.h>


data WLCInterface =
  WLCInterface {_output :: WLCOutput
               ,_view :: WLCView
               ,_keyboard :: WLCKeyboard
               ,_pointer :: WLCPointer
               ,_touch :: WLCTouch
               ,_compositor :: WLCCompositor}
  deriving (Show)

instance Default WLCInterface where
  def = WLCInterface def def def def def def

data WLCOutput =
  WLCOutput {_outputCreated :: FunPtr (WLCHandle -> IO CBool)
            ,_outputDestroyed :: FunPtr (WLCHandle -> IO ())
            ,_outputFocus :: FunPtr (WLCHandle -> CBool -> IO ())
            ,_outputResolution :: FunPtr (WLCHandle -> WLCSizePtr -> WLCSizePtr -> IO ())}
  deriving (Show)

instance Default WLCOutput where
  def = WLCOutput nullFunPtr nullFunPtr nullFunPtr nullFunPtr

data WLCView =
  WLCView {_viewCreated :: FunPtr (WLCHandle -> IO CBool)
          ,_viewDestroyed :: FunPtr (WLCHandle -> IO ())
          ,_viewFocus :: FunPtr (WLCHandle -> CBool -> IO ())
          ,_viewMoveToOutput :: FunPtr (WLCHandle -> WLCHandle -> WLCHandle -> IO ())
          ,_viewRequest :: WLCRequest}
  deriving (Show)

instance Default WLCView where
  def = WLCView nullFunPtr nullFunPtr nullFunPtr nullFunPtr def

data WLCKeyboard =
  WLCKeyboard {_keyboardKey :: FunPtr (WLCHandle ->
                                      CUInt ->
                                      WLCModifiersPtr ->
                                      CUInt ->
                                      CUInt ->
                                      WLCKeyStateBit ->
                                      IO CBool)}
  deriving (Show)

instance Default WLCKeyboard where
  def = WLCKeyboard nullFunPtr

data WLCPointer =
  WLCPointer {_pointerButton :: FunPtr (WLCHandle ->
                                       CUInt ->
                                       WLCModifiersPtr ->
                                       CUInt ->
                                       WLCButtonStateBit ->
                                       IO CBool)
             ,_pointerScroll :: FunPtr (WLCHandle -> CUInt -> WLCModifiersPtr -> CUChar -> Ptr CDouble -> IO CBool)
             ,_pointerMotion :: FunPtr (WLCHandle -> CUInt -> WLCOriginPtr -> IO CBool)}
  deriving (Show)

instance Default WLCPointer where
  def = WLCPointer nullFunPtr nullFunPtr nullFunPtr

data WLCTouch =
  WLCTouch {_touchTouch :: FunPtr (WLCHandle ->
                                  CUInt ->
                                  WLCModifiersPtr ->
                                  WLCTouchTypeBit ->
                                  CInt ->
                                  WLCOriginPtr ->
                                  IO CBool)}
  deriving (Show)

instance Default WLCTouch where
  def = WLCTouch nullFunPtr

data WLCCompositor =
  WLCCompositor {_ready :: FunPtr (IO ())}
  deriving (Show)

instance Default WLCCompositor where
  def = WLCCompositor nullFunPtr

data WLCSize =
  WLCSize CUInt
          CUInt
  deriving (Show,Eq,Ord)


data WLCRequest =
  WLCRequest {_requestGeomety :: FunPtr (WLCHandle -> WLCGeometryPtr -> IO ())
             ,_requestState :: FunPtr (WLCHandle -> WLCViewStateBit -> CBool -> IO ())}
  deriving (Show)

instance Default WLCRequest where
  def = WLCRequest nullFunPtr nullFunPtr

data WLCModifiers =
  WLCModifiers {modifierLEDs :: CUInt
               ,modifierMods :: CUInt}

data WLCOrigin =
  WLCOrigin {originX :: CInt
            ,originY :: CInt} deriving Show

instance Storable WLCOrigin where
  sizeOf _ = {#sizeof wlc_origin#}
  alignment _ = {#alignof wlc_origin#}
  peek p = do
    x <- {#get wlc_origin->x#} p
    y <- {#get wlc_origin->y#} p
    return (WLCOrigin x y)
  poke p (WLCOrigin x y) = do
    {#set wlc_origin.x#} p x
    {#set wlc_origin.y#} p y

data WLCGeometry =
  WLCGeometry {origin :: WLCOrigin
              ,size :: WLCSize}
  deriving (Show)

instance Storable WLCGeometry where
  sizeOf _ = {#sizeof wlc_geometry#}
  alignment _ = {#alignof wlc_geometry#}
  peek p = do
    origin <- peekByteOff p {#offsetof wlc_geometry->origin#}
    size <- peekByteOff p {#offsetof wlc_geometry->size#}
    return (WLCGeometry origin size)
  poke p (WLCGeometry origin size) = do
    pokeByteOff p {#offsetof wlc_geometry->origin#} origin
    pokeByteOff p {#offsetof wlc_geometry->size#} size

{#enum wlc_view_state_bit as WLCViewState {underscoreToCase} deriving (Eq,Show,Ord)#}
{#enum wlc_key_state as WLCKeyState {underscoreToCase} deriving (Eq,Show,Ord)#}
{#enum wlc_button_state as WLCButtonState {underscoreToCase} deriving (Eq,Show,Ord)#}
{#enum wlc_touch_type as WLCTouchType {underscoreToCase} deriving (Eq,Show,Ord)#}
{#enum wlc_modifier_bit as WLCModifier {underscoreToCase} deriving (Eq,Show,Ord)#}

{#pointer *wlc_modifiers as WLCModifiersPtr -> WLCModifiers#}
{#pointer *wlc_origin as WLCOriginPtr -> WLCOrigin#}
{#pointer *wlc_interface as WLCInterfacePtr -> WLCInterface#}
{#pointer *wlc_geometry as WLCGeometryPtr -> WLCGeometry#}
{#pointer *wlc_size as WLCSizePtr -> WLCSize#}

type WLCViewStateBit = CInt
type WLCKeyStateBit = CInt
type WLCButtonStateBit = CInt
type WLCTouchTypeBit = CInt
type WLCModifierBit = CInt
type WLCHandle = CULong
type CBool = CInt

newtype WLCOutputPtr = WLCOutputPtr { unwrapOutput ::  WLCHandle } deriving (Show,Eq,Ord)
newtype WLCViewPtr = WLCViewPtr { unwrapView :: WLCHandle } deriving (Show,Eq,Ord)

instance Pretty WLCOutputPtr where
  pPrint (WLCOutputPtr h) = text "WLCOutputPtr" <+> pPrint h

instance Pretty WLCViewPtr where
  pPrint (WLCViewPtr h) = text "WLCViewPtr" <+> pPrint h

instance Pretty CULong where
  pPrint = text . show

instance Pretty CUInt where
  pPrint = text . show

instance Storable WLCInterface where
  sizeOf _ = {#sizeof wlc_interface#}
  alignment _ = {#alignof wlc_interface#}
  peek p = do
    o_created <- {#get wlc_interface->output.created#} p
    o_destroyed <- {#get wlc_interface->output.destroyed#} p
    o_focus <- {#get wlc_interface->output.focus#} p
    o_resolution <- {#get wlc_interface->output.resolution#} p
    v_created <- {#get wlc_interface->view.created#} p
    v_destroyed <- {#get wlc_interface->view.destroyed#} p
    v_focus <- {#get wlc_interface->view.focus#} p
    v_move_to_output <- {#get wlc_interface->view.move_to_output#} p
    v_r_geometry <- {#get wlc_interface->view.request.geometry#} p
    v_r_state <- {#get wlc_interface->view.request.state#} p
    k_key <- {#get wlc_interface->keyboard.key#} p
    p_button <- {#get wlc_interface->pointer.button#} p
    p_scroll <- {#get wlc_interface->pointer.scroll#} p
    p_motion <- {#get wlc_interface->pointer.motion#} p
    t_touch <- {#get wlc_interface->touch.touch#} p
    c_ready <- {#get wlc_interface->compositor.ready#} p
    return (WLCInterface
              (WLCOutput o_created o_destroyed o_focus o_resolution)
              (WLCView v_created v_destroyed v_focus v_move_to_output
                       (WLCRequest v_r_geometry v_r_state))
              (WLCKeyboard k_key)
              (WLCPointer p_button p_scroll p_motion)
              (WLCTouch t_touch)
              (WLCCompositor c_ready))
  poke p (WLCInterface
          (WLCOutput o_created o_destroyed o_focus o_resolution)
          (WLCView v_created v_destroyed v_focus v_move_to_output
                   (WLCRequest v_r_geometry v_r_state))
          (WLCKeyboard k_key)
          (WLCPointer p_button p_scroll p_motion)
          (WLCTouch t_touch)
          (WLCCompositor c_ready)) = do
            {#set wlc_interface.output.created#} p o_created
            {#set wlc_interface.output.destroyed#} p o_destroyed
            {#set wlc_interface.output.focus#} p o_focus
            {#set wlc_interface.output.resolution#} p o_resolution
            {#set wlc_interface.view.created#} p v_created
            {#set wlc_interface.view.destroyed#} p v_destroyed
            {#set wlc_interface.view.focus#} p v_focus
            {#set wlc_interface.view.move_to_output#} p v_move_to_output
            {#set wlc_interface.view.request.geometry#} p v_r_geometry
            {#set wlc_interface.view.request.state#} p v_r_state
            {#set wlc_interface.keyboard.key#} p k_key
            {#set wlc_interface.pointer.button#} p p_button
            {#set wlc_interface.pointer.scroll#} p p_scroll
            {#set wlc_interface.pointer.motion#} p p_motion
            {#set wlc_interface.touch.touch#} p t_touch
            {#set wlc_interface.compositor.ready#} p c_ready

instance Storable WLCModifiers where
  sizeOf _ = {#sizeof wlc_modifiers#}
  alignment _ = {#alignof wlc_modifiers#}
  peek p = do
    leds <- {#get wlc_modifiers->leds#} p
    mods <- {#get wlc_modifiers->mods#} p
    return (WLCModifiers leds mods)
  poke p (WLCModifiers leds mods) = do
    {#set wlc_modifiers.leds#} p leds
    {#set wlc_modifiers.mods#} p mods

instance Storable WLCSize where
  sizeOf _ = {#sizeof wlc_size#}
  alignment _ = {#alignof wlc_size#}
  peek p = do
    w <- {#get wlc_size->w#} p
    h <- {#get wlc_size->h#} p
    return (WLCSize w h)
  poke p (WLCSize w h) = do
    {#set wlc_size.w#} p w
    {#set wlc_size.h#} p h

{#typedef size_t CSize#}

{#fun wlc_init as ^ {with* `WLCInterface', withStringListLen* `[String]'&} -> `Bool'#}
{#fun wlc_run as ^ {} -> `()'#}
{#fun wlc_terminate as ^ {} -> `()'#}
{#fun wlc_output_get_resolution as ^ {unwrapOutput `WLCOutputPtr'} -> `WLCSize' peek*#}
{#fun wlc_output_get_views as wlcOutputGetViews' {unwrapOutput `WLCOutputPtr', alloca- `CSize' peek*} -> `Ptr WLCHandle' id#}
{#fun wlc_output_get_mask as ^ {unwrapOutput `WLCOutputPtr'} -> `CUInt' id#}
{#fun wlc_output_set_mask as ^ {unwrapOutput `WLCOutputPtr', `CUInt'} -> `()' id#}
{#fun wlc_output_focus as ^ {unwrapOutput `WLCOutputPtr'} -> `()' id#}
{#fun wlc_view_bring_to_front as ^ {unwrapView `WLCViewPtr'} -> `()'#}
{#fun wlc_view_focus as ^ {unwrapView `WLCViewPtr'} -> `()'#}
{#fun wlc_view_get_mask as ^ {unwrapView `WLCViewPtr'} -> `CUInt' id#}
{#fun wlc_view_set_mask as ^ {unwrapView `WLCViewPtr', `CUInt'} -> `()' id#}
{#fun wlc_view_get_output as ^ {unwrapView `WLCViewPtr'} -> `WLCHandle' id#}
{#fun wlc_view_set_geometry as ^ {unwrapView `WLCViewPtr', with* `WLCGeometry'} -> `()'#}
{#fun wlc_view_set_state as ^ {unwrapView `WLCViewPtr', `WLCViewState', `Bool'} -> `()'#}
{#fun wlc_view_close as ^ {unwrapView `WLCViewPtr'} -> `()' id#}

wlcOutputGetViews :: WLCOutputPtr -> IO [WLCHandle]
wlcOutputGetViews handle = do
  (ptr,size) <- wlcOutputGetViews' handle
  peekArray (fromIntegral size) ptr

withStringListLen :: [String] -> ((CInt, Ptr (Ptr CChar)) -> IO a) -> IO a
withStringListLen args f = do
  cstrings <- mapM newCString args
  withArray cstrings (\array -> f (fromIntegral $ length args, array))
