{-# LANGUAGE TemplateHaskell          #-}

module Reflex.Vty.Test.Monad.Host.TH (
  declareStuff

  -- for testing
  , declareNetworkData
  , declareNetworkInstance
) where

import           Relude                   hiding (getFirst)

import Language.Haskell.TH

import           Reflex
import           Reflex.Vty.Test.Monad.Host


declareStuff :: String -> Q [Dec]
declareStuff name' = do
  let
    name = mkName name'
  declareNetworkData name


declareNetworkData :: Name -> Q [Dec]
declareNetworkData name = do
  let
    k_m = InfixT StarT ''(->) StarT
  tv_t <- newName "t"
  tv_m <- newName "m"
  -- not sure if KindedTV is necessary
  return $ [DataD [] name [PlainTV tv_t, KindedTV tv_m k_m] Nothing [] []]

declareNetworkInstance :: Name -> Q [Dec]
declareNetworkInstance name = do
  let
    v_t = VarT $ mkName "t"
    v_m = VarT $ mkName "m"
    -- TODO convert to [| |]
    cxt1 = AppT (AppT (ConT $ mkName "MonadVtyApp") v_t) $ AppT (AppT (ConT $ mkName "TestGuestT") v_t) v_m
    cxt2 = AppT (AppT (ConT $ mkName "TestGuestConstraints") v_t) $ v_m
    v_potatoNetwork = AppT (AppT (ConT $ name) v_t) v_m
    classinstance = AppT (AppT (AppT (ConT $ mkName "ReflexVtyTestApp") v_potatoNetwork) v_t) v_m
  return $ [InstanceD Nothing [cxt1,cxt2] classinstance []]


--ReflexVtyTestApp (PotatoNetwork t m) t m

{-

data BasicNetworkTest1 t (m :: Type -> Type)

instance (MonadVtyApp t (TestGuestT t m), TestGuestConstraints t m) => ReflexVtyTestApp (BasicNetworkTest1 t m) t m where

  -- I just wanted to try using VtyAppInputEvents/VtyAppInputTriggerRefs
  -- it would have been a lot easier to use a vty event to trigger the popup
  data VtyAppInputTriggerRefs (BasicNetworkTest1 t m) = BasicNetworkTest1_InputTriggerRefs {
      _basicNetworkTest1_InputTriggerRefs_makePopup :: Ref m (Maybe (EventTrigger t ()))
    }
  data VtyAppInputEvents (BasicNetworkTest1 t m) = BasicNetworkTest1_InputEvents {
      _basicNetworkTest1_InputEvents_makePopup :: Event t ()
    }
  data VtyAppOutput (BasicNetworkTest1 t m) =
    BasicNetworkTest1_Output {
        _basicNetworkTest1_Output_cancelEv :: Event t ()
        , _basicNetworkTest1_Output_popupOutEv :: Event t Int
        , _basicNetworkTest1_Output_popupStateDyn :: Dynamic t Bool
      }
  getApp BasicNetworkTest1_InputEvents {..} = do
    let
      someWidget = fmap (const 123) <$> key V.KEnter
      someWidgetEv = fmap (const someWidget) _basicNetworkTest1_InputEvents_makePopup
    -- popup that closes when you press enter
    (popupEv, popupStateDyn) <- popupPaneSimple def someWidgetEv
    -- gotta make the popup look pretty :D
    fill '#'
    return $ BasicNetworkTest1_Output never popupEv popupStateDyn
  makeInputs = do
    (ev, ref) <- newEventWithTriggerRef
    return (BasicNetworkTest1_InputEvents ev, BasicNetworkTest1_InputTriggerRefs ref)
-}
