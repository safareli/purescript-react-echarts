module React.ECharts where

import Prelude

import Control.Lazy (fix)
import Control.Monad.Aff (error, runAff_)
import Control.Monad.Aff.AVar (AVAR, AVar, putVar)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import DOM.HTML.Types (HTMLElement)
import DOM.Node.Types (Node)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Nullable (toNullable)
import Data.Tuple (Tuple(..))
import ECharts.Chart as EC
import ECharts.Event as EE
import ECharts.Types as ET
import Partial.Unsafe (unsafeCrashWith)
import React as R
import React.DOM as D
import React.DOM.Props as P
import ReactDOM (refToNode)
import Unsafe.Coerce (unsafeCoerce)

data Commands
  = Dispose
  | Set ET.Option
  | Reset ET.Option
  | Resize
  | Clear
  | AskOptions (AVar Foreign)
  | Dispatch ET.EChartsEvent

data Messages
  = IsInitialised (Bus.BusW Commands)
  | EventRaised ET.EChartsEvent

type Props =
  { messages :: Bus.BusW Messages
  , width :: Int
  , height :: Int
  }

data State
  = Idle
  | Initialised
      { command :: Bus.BusW Commands
      }

klass :: R.ReactClass Props
klass =
  R.createClass spec
    { componentDidMount = componentDidMount
    , componentWillUnmount = componentWillUnmount
    , shouldComponentUpdate = shouldComponentUpdate
    }
  where
    shouldComponentUpdate :: forall eff. R.ShouldComponentUpdate Props State (avar :: AVAR | eff)
    shouldComponentUpdate this nextProps nextState = do
      props <- R.getProps this
      case nextState of
        Idle -> do
          unsafeCrashWith "in shouldComponentUpdate component must be initialised (React.ECharts.klass)"
        Initialised state -> do
          if nextProps.width /= props.width || nextProps.height /= props.height
            then do
              runAff_ mempty $ Bus.write Resize state.command
              pure true
            else
              pure false

    componentWillUnmount :: forall eff. R.ComponentWillUnmount Props State (avar :: AVAR | eff)
    componentWillUnmount this = R.readState this >>= case _ of
      Idle ->
        unsafeCrashWith "in componentWillUnmount component must be initialised (React.ECharts.klass)"
      Initialised { command } ->
        runAff_ mempty $ do
          props <- liftEff $ R.getProps this
          Bus.kill (error "kill from componentWillUnmount") command

    componentDidMount :: forall eff. R.ComponentDidMount Props State (avar :: AVAR, dom ∷ DOM, echarts ∷ ET.ECHARTS, exception ∷ EXCEPTION |eff)
    componentDidMount this = do
      nodeRef <- R.readRef this nodeRefName
      let
        node = case refToNode $ toNullable nodeRef of
          Nothing -> unsafeCrashWith "in `componentDidMount` ref must be non null value (React.ECharts.klass)"
          Just n -> (unsafeCoerce :: Node -> HTMLElement) n
      runAff_ mempty do
        Tuple commandR commandW <- Bus.split <$> Bus.make
        chart <- liftEff $ EC.init node
        EE.listenAll chart \e -> do
          props <- R.getProps this
          runAff_ mempty $ Bus.write (EventRaised e) props.messages
        _ <- liftEff $ R.writeState this $ Initialised { command: commandW }
        props <- liftEff $ R.getProps this
        Bus.write (IsInitialised commandW) props.messages

        fix \loop -> do
          Bus.read commandR >>= case _ of
            Dispose -> liftEff $ EC.dispose chart
            Set opts -> liftEff $ EC.setOption opts chart
            Reset opts -> liftEff $ EC.resetOption opts chart
            Resize -> liftEff $ EC.resize chart
            Clear -> liftEff $ EC.clear chart
            AskOptions var -> EC.getOption chart >>= flip putVar var
            Dispatch ev -> liftEff $ EE.dispatch ev chart
          loop

      pure unit
    spec = R.spec Idle render
    render this = do
      props <- R.getProps this
      pure $
        D.div [ P.ref nodeRefName, P.style { width: props.width, height: props.height } ] []
    nodeRefName :: String
    nodeRefName = "node-ref"
