module React.ECharts where

import Prelude

import Control.Lazy (fix)
import Control.Monad.Aff (error, runAff_)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import DOM.Node.Types (Node)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Nullable (toNullable)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)
import React as R
import React.DOM as D
import React.DOM.Props as P
import ReactDOM (refToNode)


data Commands = DoStaff
data Messages = IsInitialised (Bus.BusW Commands) | Unmounting

type Props =
  { messages :: Bus.BusW Messages
  }


data State
  = Idle
  | Initialised
      { node :: Node
      , command :: Bus.BusW Commands
      }

klass :: R.ReactClass Props
klass = R.createClass spec
    { componentDidMount = componentDidMount
    , componentWillUnmount = componentWillUnmount
    }
  where
    componentWillUnmount :: forall eff. R.ComponentWillUnmount Props State (avar :: AVAR | eff)
    componentWillUnmount this = R.readState this >>= case _ of
      Idle ->
        unsafeCrashWith "in componentWillUnmount component must be initialised (React.ECharts.echart)"
      Initialised { command } ->
        runAff_ mempty $ do
          props <- liftEff $ R.getProps this
          Bus.write Unmounting props.messages
          Bus.kill (error "kill from componentWillUnmount") command

    componentDidMount :: forall eff. R.ComponentDidMount Props State (avar :: AVAR |eff)
    componentDidMount this = do
      nodeRef <- R.readRef this nodeRefName
      let 
        node = case refToNode $ toNullable nodeRef of
          Nothing -> unsafeCrashWith "in `componentDidMount` ref must be non null value (React.ECharts.echart)"
          Just n -> n
      props <- R.getProps this
      runAff_ mempty do
        Tuple commandR commandW <- Bus.split <$> Bus.make
        _ <- liftEff $ R.writeState this $ Initialised { node, command: commandW }
        Bus.write (IsInitialised commandW) props.messages
        
        fix \loop -> do
          Bus.read commandR >>= case _ of
            DoStaff -> do
              -- some staff
              pure unit
          loop
      
      pure unit
    spec = R.spec Idle render
    render _ = pure $
      D.div [ P.ref nodeRefName ] []
    nodeRefName :: String
    nodeRefName = "node-ref"


