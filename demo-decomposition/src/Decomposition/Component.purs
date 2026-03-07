-- | Decomposition demo — annotated graph + multiple chimera styles.
module Decomposition.Component where

import Prelude

import Data.Array (mapWithIndex, snoc, (!!))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Hylograph.HATS.InterpreterTick (clearContainer, rerender)

import Decomposition.ForceLayout (loadMiserables)
import Decomposition.Graphs (ShowcaseGraph, allShowcaseGraphs)
import Decomposition.Render as R

type Entry =
  { showcase :: ShowcaseGraph
  , info :: R.DecompInfo
  }

-- | Chimera rendering styles
data ChimeraStyle
  = StyleExtracted   -- blocks extracted side by side (original)
  | StyleChord       -- chord diagrams + rings
  | StyleSpine       -- block-cut tree as skeleton
  | StyleOrbital     -- blocks as positioned bodies in space

chimeraStyles :: Array { style :: ChimeraStyle, label :: String, desc :: String }
chimeraStyles =
  [ { style: StyleExtracted, label: "Extracted", desc: "Blocks extracted and rendered independently" }
  , { style: StyleChord, label: "Chord", desc: "Dense blocks as chord diagrams, cycles as rings" }
  , { style: StyleSpine, label: "Spine", desc: "Block-cut tree as horizontal skeleton" }
  , { style: StyleOrbital, label: "Orbital", desc: "Blocks as bodies in space, form matches structure" }
  ]

mkEntry :: ShowcaseGraph -> Entry
mkEntry sg = { showcase: sg, info: R.analyzeGraph sg.graph }

type State =
  { currentIndex :: Int
  , chimeraStyle :: ChimeraStyle
  , graphs :: Array Entry
  }

data Action
  = Initialize
  | GoTo Int
  | SetChimeraStyle ChimeraStyle

derive instance Eq ChimeraStyle

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ ->
        { currentIndex: 0
        , chimeraStyle: StyleExtracted
        , graphs: map mkEntry allShowcaseGraphs
        }
    , render
    , eval: H.mkEval H.defaultEval
        { initialize = Just Initialize
        , handleAction = handleAction
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let
    current = state.graphs !! state.currentIndex
  in
    HH.div [ HP.class_ (HH.ClassName "decomp-container") ]
      [ -- Header
        HH.div [ HP.class_ (HH.ClassName "decomp-header") ]
          [ HH.h1_ [ HH.text "Graph Decomposition" ]
          , HH.p [ HP.class_ (HH.ClassName "decomp-subtitle") ]
              [ HH.text "Structural analysis reveals the chimera skeleton" ]
          ]
      -- Tab bar
      , HH.div [ HP.class_ (HH.ClassName "decomp-tabs") ]
          (mapWithIndex (\i entry ->
            HH.button
              [ HP.class_ (HH.ClassName $ "decomp-tab" <> if i == state.currentIndex then " active" else "")
              , HE.onClick \_ -> GoTo i
              ]
              [ HH.text entry.showcase.title ]
          ) state.graphs)
      -- Main content
      , case current of
          Nothing -> HH.text ""
          Just entry ->
            HH.div [ HP.class_ (HH.ClassName "decomp-main") ]
              [ HH.div [ HP.class_ (HH.ClassName "decomp-content") ]
                  [ -- Annotated graph
                    HH.div [ HP.class_ (HH.ClassName "decomp-section") ]
                      [ HH.h2_ [ HH.text "Decomposition" ]
                      , HH.div [ HP.class_ (HH.ClassName "decomp-viz"), HP.id "decomp-annotated" ] []
                      ]
                  -- Before/after matrices
                  , HH.div [ HP.class_ (HH.ClassName "decomp-row") ]
                      [ HH.div [ HP.class_ (HH.ClassName "decomp-section decomp-matrix-section") ]
                          [ HH.h2_ [ HH.text "Raw Matrix" ]
                          , HH.p [ HP.class_ (HH.ClassName "chimera-desc") ]
                              [ HH.text "Alphabetical node order — structure hidden" ]
                          , HH.div [ HP.class_ (HH.ClassName "decomp-viz"), HP.id "decomp-matrix-raw" ] []
                          ]
                      , HH.div [ HP.class_ (HH.ClassName "decomp-section decomp-matrix-section") ]
                          [ HH.h2_ [ HH.text "Block-Ordered Matrix" ]
                          , HH.p [ HP.class_ (HH.ClassName "chimera-desc") ]
                              [ HH.text "Nodes grouped by block — structure on the diagonal" ]
                          , HH.div [ HP.class_ (HH.ClassName "decomp-viz"), HP.id "decomp-matrix-ordered" ] []
                          ]
                      ]
                  -- Chimera view with style selector
                  , HH.div [ HP.class_ (HH.ClassName "decomp-section") ]
                      [ HH.div [ HP.class_ (HH.ClassName "chimera-header") ]
                          [ HH.h2_ [ HH.text "Chimera" ]
                          , HH.div [ HP.class_ (HH.ClassName "chimera-styles") ]
                              (map (\s ->
                                HH.button
                                  [ HP.class_ (HH.ClassName $ "chimera-style-btn" <> if s.style == state.chimeraStyle then " active" else "")
                                  , HE.onClick \_ -> SetChimeraStyle s.style
                                  ]
                                  [ HH.text s.label ]
                              ) chimeraStyles)
                          ]
                      , HH.p [ HP.class_ (HH.ClassName "chimera-desc") ]
                          [ HH.text (chimeraDesc state.chimeraStyle) ]
                      , HH.div [ HP.class_ (HH.ClassName "decomp-viz"), HP.id "decomp-chimera" ] []
                      , R.renderChimeraLegend entry.info
                      ]
                  ]
              -- Sidebar
              , HH.div [ HP.class_ (HH.ClassName "decomp-sidebar") ]
                  [ HH.h3_ [ HH.text entry.showcase.title ]
                  , HH.p [ HP.class_ (HH.ClassName "decomp-description") ]
                      [ HH.text entry.showcase.description ]
                  , R.renderMetrics entry.info
                  , R.renderLegend
                  ]
              ]
      -- Footer
      , HH.div [ HP.class_ (HH.ClassName "decomp-footer") ]
          [ HH.p_
              [ HH.text "The "
              , HH.strong_ [ HH.text "block-cut tree" ]
              , HH.text " decomposes a graph into biconnected components (blocks) "
              , HH.text "joined at articulation points (cut vertices). "
              , HH.text "Each block has a structural shape — dense, bipartite, cyclic, sparse — "
              , HH.text "that maps naturally to a visualization form."
              ]
          , HH.p_
              [ HH.text "The chimera view shows this in action: dense cliques become chord diagrams, "
              , HH.text "bipartite sections become two-column layouts, "
              , HH.text "cycles become rings, "
              , HH.text "and each piece is perfectly legible because form matches structure."
              ]
          ]
      ]

chimeraDesc :: ChimeraStyle -> String
chimeraDesc StyleExtracted = "Each block extracted and rendered in the form that suits its structure"
chimeraDesc StyleChord = "Dense blocks as chord diagrams, cycles as rings — richer per-block forms"
chimeraDesc StyleSpine = "The block-cut tree itself becomes the layout skeleton, with blocks hanging off it"
chimeraDesc StyleOrbital = "Blocks as bodies in space — shape encodes structure, connections trace the block-cut tree"

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Load Les Miserables asynchronously, add as a tab when ready
    result <- liftAff loadMiserables
    case result of
      Right sg -> do
        H.modify_ \s -> s { graphs = snoc s.graphs (mkEntry sg) }
      Left _ -> pure unit  -- silently skip if fetch fails
    renderCurrentHATS

  GoTo i -> do
    state <- H.get
    when (i /= state.currentIndex) do
      H.modify_ _ { currentIndex = i }
      renderCurrentHATS

  SetChimeraStyle style -> do
    state <- H.get
    when (style /= state.chimeraStyle) do
      H.modify_ _ { chimeraStyle = style }
      renderChimera

renderCurrentHATS :: forall o m. MonadAff m => H.HalogenM State Action () o m Unit
renderCurrentHATS = do
  state <- H.get
  case state.graphs !! state.currentIndex of
    Nothing -> pure unit
    Just entry -> liftEffect do
      clearContainer "#decomp-annotated"
      _ <- rerender "#decomp-annotated"
        (R.annotatedGraphTree entry.showcase.graph entry.showcase.positions entry.info)
      clearContainer "#decomp-matrix-raw"
      _ <- rerender "#decomp-matrix-raw"
        (R.rawMatrixTree entry.showcase.graph entry.info)
      clearContainer "#decomp-matrix-ordered"
      _ <- rerender "#decomp-matrix-ordered"
        (R.blockMatrixTree entry.showcase.graph entry.info)
      pure unit
  renderChimera

renderChimera :: forall o m. MonadAff m => H.HalogenM State Action () o m Unit
renderChimera = do
  state <- H.get
  case state.graphs !! state.currentIndex of
    Nothing -> pure unit
    Just entry -> liftEffect do
      clearContainer "#decomp-chimera"
      _ <- rerender "#decomp-chimera"
        (chimeraTreeFor state.chimeraStyle entry)
      pure unit

chimeraTreeFor :: ChimeraStyle -> Entry -> R.HatsTree
chimeraTreeFor StyleExtracted entry = R.chimeraTree entry.info
chimeraTreeFor StyleChord entry = R.chimeraChordTree entry.info
chimeraTreeFor StyleSpine entry = R.chimeraSpineTree entry.showcase.graph entry.info
chimeraTreeFor StyleOrbital entry = R.chimeraOrbitalTree entry.showcase.graph entry.info
