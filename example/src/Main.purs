module Main where

import Prelude

import Control.Lazy (fix)
import Control.Monad.Aff (forkAff)
import Control.Monad.Aff.AVar (makeEmptyVar, putVar, takeVar)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (randomInt, RANDOM)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import Data.Array (fold, foldMap)
import Data.Array as Arr
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEArr
import Data.Foldable (for_, maximum)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import ECharts.Monad (interpret)
import Options (options)
import Partial.Unsafe (unsafeCrashWith)
import React as R
import React.DOM as D
import React.DOM.Props as P
import React.ECharts as EC
import ReactDOM as RD
import Thermite as T
import Unsafe.Coerce (unsafeCoerce)

main :: forall eff. Eff (dom :: DOM, exception :: EXCEPTION | eff) Unit
main = do
  elem <- getElem
  for_ elem $ RD.render $ R.createFactory appClass unit

getElem :: forall eff. Eff (dom :: DOM | eff) (Maybe Element)
getElem = do
  win <- window
  doc <- document win
  getElementById (ElementId "app") (documentToNonElementParentNode (htmlDocumentToDocument doc))


randomInArray ∷ ∀ e a. NonEmptyArray a → Eff (random ∷ RANDOM|e) a
randomInArray arr = do
  randomInt 0 (NEArr.length arr - 1) <#> \n -> case arr NEArr.!! n of
    Just a -> a
    Nothing -> unsafeCrashWith "impossible"

type State = 
  Array
    { idx :: Int
    , messagesBus :: Bus.BusW EC.Messages
    , commandBus :: Maybe (Bus.BusW EC.Commands)
    }

data Action
  = AddChart
  | SetRandomOption (Bus.BusW EC.Commands)
  | RemoveChart Int


_TODO :: ∀ a. String -> a
_TODO _ = unsafeCoerce unit

appClass :: forall props. R.ReactClass props
appClass = R.createClass $ _.spec $ T.createReactSpec spec []
  where
    spec :: forall eff. T.Spec (avar :: AVAR, random :: RANDOM |eff) State props Action
    spec = T.simpleSpec performAction render

    performAction :: forall eff. T.PerformAction (avar :: AVAR, random :: RANDOM |eff) State props Action
    performAction action _ state = case action of 
      AddChart -> do
        let 
          idx = maybe 0 (_ + 1) $ maximum $ _.idx <$> state 
        Tuple messagesBusR messagesBusW <- lift $ Bus.split <$> Bus.make
        busVar <- lift $ makeEmptyVar
        void $ lift $ forkAff $ fix \loop -> do
          Bus.read messagesBusR >>= case _ of
            EC.IsInitialised commandBus -> do
              putVar commandBus busVar
              opt ← liftEff $ randomInArray options
              Bus.write (EC.Set $ interpret opt) commandBus
            EC.EventRaised eChartsEvent -> do
              pure unit
          loop
        void $ T.writeState $ Arr.snoc state { idx, commandBus: Nothing, messagesBus: messagesBusW }
        commandBus <- lift $ takeVar busVar
        void $ T.modifyState \state' -> state' <#> \x -> if x.idx == idx then x{commandBus = Just commandBus} else x
      SetRandomOption commandBus -> do
        opt ← lift $ liftEff $ randomInArray options
        lift $ Bus.write (EC.Reset $ interpret opt) commandBus
      RemoveChart idx -> do
        void $ T.writeState $ Arr.filter (\x -> x.idx /= idx) state

    render :: forall eff. T.Render State props Action
    render dispatch _ state _ = pure $ 
      D.div'
        $ [ D.h1' [ D.text "purescript-react-echarts" ]
          , D.button
              [ P.onClick $ const $ dispatch $ AddChart ]
              [ D.text "Add chart" ]
          ]
        <> map renderOne state
      where
      renderOne {idx, commandBus, messagesBus} =
        D.div [ P.key $ show idx ] $ fold
          [ pure $ D.div' 
            [ R.createFactory EC.klass
                { messages: messagesBus
                , width: 400
                , height: 400
                }
            ]
          , commandBus # foldMap \ bus -> pure $ D.button
              [ P.onClick $ const $ dispatch $ SetRandomOption bus ]
              [ D.text "Set random option" ]
          , pure $ D.button
              [ P.onClick $ const $ dispatch $ RemoveChart idx ]
              [ D.text "Remove" ]
          ]
