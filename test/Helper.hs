-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Layout
-- Copyright   :  (c) Tim Docker 2006, 2014
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- This module glues together axes and plots to actually create a renderable
-- for a chart.
--
-- Note that Template haskell is used to derive accessor functions
-- (see 'Control.Lens') for each field of the following data types:
--
--     * 'Layout'
--     
--     * 'LayoutLR'
-- 
--     * 'StackedLayouts'
-- 
--     * 'LayoutAxis'
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}

module Helper
  ( -- * Types
    Layout(..)
  , LayoutLR(..)
  , LayoutAxis(..)
  , LayoutPick(..)
  , StackedLayouts(..)
  , StackedLayout(..)
  -- , LegendItem  haddock complains about this being missing, but from what?
  , MAxisFn
  
    -- * Rendering
  , layoutToRenderable
  , layoutToGrid  
  , layoutLRToRenderable
  , layoutLRToGrid  
  , renderStackedLayouts
  , stackedsToGrid

    -- * LayoutAxis lenses
  , laxis_title_style
  , laxis_title
  , laxis_style
  , laxis_generate
  , laxis_override
  , laxis_reverse
    
    -- * Layout lenses
  , layout_background
  , layout_plot_background
  , layout_title
  , layout_title_style
  , layout_x_axis
  , layout_top_axis_visibility
  , layout_bottom_axis_visibility
  , layout_y_axis
  , layout_left_axis_visibility
  , layout_right_axis_visibility
  , layout_margin
  , layout_plots
  , layout_legend
  , layout_grid_last

  , layout_axes_styles
  , layout_axes_title_styles
  , layout_all_font_styles
  , layout_foreground
      
    -- * LayoutLR lenses
  , layoutlr_background
  , layoutlr_plot_background
  , layoutlr_title
  , layoutlr_title_style
  , layoutlr_x_axis
  , layoutlr_top_axis_visibility
  , layoutlr_bottom_axis_visibility
  , layoutlr_left_axis
  , layoutlr_right_axis
  , layoutlr_left_axis_visibility
  , layoutlr_right_axis_visibility
  , layoutlr_plots
  , layoutlr_legend
  , layoutlr_margin
  , layoutlr_grid_last

  , layoutlr_axes_styles
  , layoutlr_axes_title_styles
  , layoutlr_all_font_styles
  , layoutlr_foreground

    -- * StackedLayouts lenses
  , slayouts_layouts
  , slayouts_compress_legend
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Axis
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Utils
import Graphics.Rendering.Chart.Plot
import Graphics.Rendering.Chart.Legend
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Grid
import Control.Monad
import Control.Lens hiding (at)
import Data.Colour
import Data.Colour.Names (white)
import Data.Default.Class
import Data.Maybe (fromMaybe)

-- | A @MAxisFn@ is a function that generates an (optional) axis
--   given the points plotted against that axis.


-- | Type of axis that is used in 'Layout' and 'LayoutLR'.
--   
--   To generate the actual axis type ('AxisData' and 'AxisT')
--   the '_laxis_generate' function is called and custom settings
--   are applied with '_laxis_override'. Note that the 'AxisVisibility'
--   values in 'Layout' and 'LayoutLR' override visibility related 
--   settings of the axis.


-- | Information on what is at a specifc location of a 'Layout' or 'LayoutLR'.
--   This is delivered by the 'PickFn' of a 'Renderable'.


type LegendItem = (String,Rect -> BackendProgram ())

-- | A Layout value is a single plot area, with single x and y
--   axis. The title is at the top and the legend at the bottom. It's
--   parametrized by the types of values to be plotted on the x
--   and y axes.



-- | Render the given 'Layout'.



getLayoutXVals :: Layout x y -> [x]
getLayoutXVals l = concatMap (fst . _plot_all_points) (_layout_plots l)

-- | Extract all 'LegendItem's from the plots of a 'Layout'.
getLegendItems :: Layout x y -> [LegendItem]
getLegendItems l = concat [ _plot_legend p | p <- _layout_plots l ]

-- | Render the given 'LegendItem's for a 'Layout'.
renderLegend :: Layout x y -> [LegendItem] -> Renderable (LayoutPick x y y)
renderLegend l legItems = gridToRenderable g
  where
    g      = besideN [ tval $ mkLegend (_layout_legend l) (_layout_margin l) legItems
                     , weights (1,1) $ tval emptyRenderable ]

-- | Render the plot area of a 'Layout'. This consists of the 
--   actual plot area with all plots, the axis and their titles.
layoutPlotAreaToGrid :: forall x y. (Ord x, Ord y) =>
                        Layout x y -> Grid (Renderable (LayoutPick x y y))
layoutPlotAreaToGrid l = buildGrid LayoutGridElements{
  lge_plots = mfill (_layout_plot_background l) $ plotsToRenderable l,
  lge_taxis = (tAxis,_laxis_title $ _layout_x_axis l, _laxis_title_style $ _layout_x_axis l),
  lge_baxis = (bAxis,_laxis_title $ _layout_x_axis l, _laxis_title_style $ _layout_x_axis l),
  lge_laxis = (lAxis,_laxis_title $ _layout_y_axis l, _laxis_title_style $ _layout_y_axis l),
  lge_raxis = (rAxis,"", def),
  lge_margin = _layout_margin l
  }
  where
    xvals = [ x | p <- _layout_plots l, x <- fst $ _plot_all_points p]
    yvals = [ y | p <- _layout_plots l, y <- snd $ _plot_all_points p]

    bAxis = mkAxis E_Bottom (overrideAxisVisibility l _layout_x_axis _layout_bottom_axis_visibility) xvals
    tAxis = mkAxis E_Top    (overrideAxisVisibility l _layout_x_axis _layout_top_axis_visibility   ) xvals
    lAxis = mkAxis E_Left   (overrideAxisVisibility l _layout_y_axis _layout_left_axis_visibility  ) yvals
    rAxis = mkAxis E_Right  (overrideAxisVisibility l _layout_y_axis _layout_right_axis_visibility ) yvals
    axes = (bAxis,lAxis,tAxis,rAxis)

    plotsToRenderable lxy = Renderable {
        minsize = return (0,0),
        render  = renderPlots lxy
    }

    -- | Render the plots of a 'Layout' to a plot area of given size.
    renderPlots :: Layout x y -> RectSize -> BackendProgram (PickFn (LayoutPick x y y))
    renderPlots lxy sz@(w,h) = do
        unless (_layout_grid_last lxy) (renderGrids sz axes)
        withClipRegion (Rect (Point 0 0) (Point w h)) $
          mapM_ rPlot (_layout_plots lxy)
        when (_layout_grid_last lxy) (renderGrids sz axes)
        return pickfn
      where
        rPlot = renderSinglePlot sz bAxis lAxis

        xr = (0, w)
        yr = (h, 0)

        pickfn :: PickFn (LayoutPick x y y)
        pickfn (Point x y) = do  -- Maybe monad
            xat <- mxat
            yat <- myat
            return (LayoutPick_PlotArea (mapx xat x) (mapy yat y) (mapy yat y))
          where
            mxat = case (bAxis,tAxis) of
                (Just at,_)       -> Just at
                (_,Just at)       -> Just at
                (Nothing,Nothing) -> Nothing
            myat = case (lAxis,rAxis) of
                (Just at,_)   -> Just at
                (_,Just at)   -> Just at
                (Nothing,Nothing)   -> Nothing
            mapx (AxisT _ _ rev ad) = _axis_tropweiv ad (optPairReverse rev xr)
            mapy (AxisT _ _ rev ad) = _axis_tropweiv ad (optPairReverse rev yr)

-- | Empty 'Layout' without title and plots. The background is white and 
--   the grid is drawn beneath all plots. There will be a legend. The top
--   and right axis will not be visible.

----------------------------------------------------------------------
  
-- | A LayoutLR value is a single plot area, with an x axis and
--   independent left and right y axes, with a title at the top;
--   legend at the bottom. It's parametrized by the types of values
--   to be plotted on the x and two y axes.



-- | Render the given 'LayoutLR'.


getLayoutLRXVals :: LayoutLR x yl yr -> [x]
getLayoutLRXVals l = concatMap deEither $ _layoutlr_plots l
  where
    deEither :: Either (Plot x yl) (Plot x yr) -> [x]
    deEither (Left x)  = fst $ _plot_all_points x
    deEither (Right x) = fst $ _plot_all_points x

-- | Extract all 'LegendItem's from the plots of a 'LayoutLR'.
--   Left and right plot legend items are still separated.
getLegendItemsLR :: LayoutLR x yl yr -> ([LegendItem],[LegendItem])
getLegendItemsLR l = (
    concat [ _plot_legend p | (Left p ) <- _layoutlr_plots l ],
    concat [ _plot_legend p | (Right p) <- _layoutlr_plots l ]
    )

-- | Render the given 'LegendItem's for a 'LayoutLR'.
renderLegendLR :: LayoutLR x yl yr -> ([LegendItem],[LegendItem]) -> Renderable (LayoutPick x yl yr)
renderLegendLR l (lefts,rights) = gridToRenderable g
  where
    g      = besideN [ tval $ mkLegend (_layoutlr_legend l) (_layoutlr_margin l) lefts
                     , weights (1,1) $ tval emptyRenderable
                     , tval $ mkLegend (_layoutlr_legend l) (_layoutlr_margin l) rights ]
    -- lm     = _layoutlr_margin l

layoutLRPlotAreaToGrid :: forall x yl yr. (Ord x, Ord yl, Ord yr) 
                       => LayoutLR x yl yr 
                       -> Grid (Renderable (LayoutPick x yl yr))
layoutLRPlotAreaToGrid l = buildGrid LayoutGridElements{
  lge_plots = mfill (_layoutlr_plot_background l) $ plotsToRenderable l,
  lge_taxis = (tAxis,_laxis_title $ _layoutlr_x_axis l, _laxis_title_style $ _layoutlr_x_axis l),
  lge_baxis = (bAxis,_laxis_title $ _layoutlr_x_axis l, _laxis_title_style $ _layoutlr_x_axis l),
  lge_laxis = (lAxis,_laxis_title $ _layoutlr_left_axis l, _laxis_title_style $ _layoutlr_left_axis l),
  lge_raxis = (rAxis,_laxis_title $ _layoutlr_right_axis l, _laxis_title_style $ _layoutlr_right_axis l),
  lge_margin = _layoutlr_margin l
  }
  where
    xvals =  [ x | (Left p)  <- _layoutlr_plots l, x <- fst $ _plot_all_points p]
          ++ [ x | (Right p) <- _layoutlr_plots l, x <- fst $ _plot_all_points p]
    yvalsL = [ y | (Left p)  <- _layoutlr_plots l, y <- snd $ _plot_all_points p]
    yvalsR = [ y | (Right p) <- _layoutlr_plots l, y <- snd $ _plot_all_points p]
    
    bAxis = mkAxis E_Bottom (overrideAxisVisibility l _layoutlr_x_axis _layoutlr_bottom_axis_visibility) xvals
    tAxis = mkAxis E_Top    (overrideAxisVisibility l _layoutlr_x_axis _layoutlr_top_axis_visibility   ) xvals
    lAxis = mkAxis E_Left   (overrideAxisVisibility l _layoutlr_left_axis  _layoutlr_left_axis_visibility ) yvalsL
    rAxis = mkAxis E_Right  (overrideAxisVisibility l _layoutlr_right_axis _layoutlr_right_axis_visibility) yvalsR
    axes = (bAxis,lAxis,tAxis,rAxis)

    plotsToRenderable llr = Renderable {
        minsize = return (0,0),
        render  = renderPlots llr
    }

    renderPlots :: LayoutLR x yl yr -> RectSize -> BackendProgram (PickFn (LayoutPick x yl yr))
    renderPlots llr sz@(w,h) = do
        unless (_layoutlr_grid_last llr) (renderGrids sz axes)
        withClipRegion (Rect (Point 0 0) (Point w h)) $
          mapM_ rPlot (_layoutlr_plots llr)
        when (_layoutlr_grid_last llr) (renderGrids sz axes)
        return pickfn
      where
        rPlot (Left  p) = renderSinglePlot sz bAxis lAxis p
        rPlot (Right p) = renderSinglePlot sz bAxis rAxis p

        xr = (0, w)
        yr = (h, 0)

        pickfn (Point x y) = do  -- Maybe monad
            xat <- mxat
            (yatL,yatR) <- myats
            return (LayoutPick_PlotArea (mapx xat x) (mapy yatL y) (mapy yatR y))
          where
            mxat = case (bAxis,tAxis) of
                (Just at,_)       -> Just at
                (_,Just at)       -> Just at
                (Nothing,Nothing) -> Nothing
            myats = case (lAxis,rAxis) of
                (Just at1,Just at2) -> Just (at1,at2)
                (_,_)   -> Nothing
            mapx (AxisT _ _ rev ad) = _axis_tropweiv ad (optPairReverse rev xr)
            mapy (AxisT _ _ rev ad) = _axis_tropweiv ad (optPairReverse rev yr)

----------------------------------------------------------------------





-- | Render several layouts with the same x-axis type and range,
--   vertically stacked so that their origins and x-values are aligned.
--
--   The legends from all the charts may be optionally combined, and shown
--   once on the bottom chart. See 'StackedLayouts' for further information.

stackedsToGrid :: forall x. (Ord x) => StackedLayouts x -> Grid (Renderable ())
stackedsToGrid (StackedLayouts{_slayouts_layouts=[]}) = empty
stackedsToGrid slp@(StackedLayouts{_slayouts_layouts=sls@(sl1:_)}) = g
  where
    g = fullOverlayUnder (fillBackground bg emptyRenderable)
      $ foldr (above.mkGrid) nullt (zip sls [0,1..])
    
    mkGrid :: (StackedLayout x, Int) -> Grid (Renderable ())
    mkGrid (sl, i)
       | i==0 = weights (0,0.001) tempg
       | otherwise = tempg
        
      where
        tempg = titleR
          `wideAbove`
          addMarginsToGrid (lm,lm,lm,lm) (mkPlotArea usedAxis)
          `aboveWide`
          (if showLegend then legendR else emptyRenderable)

        titleR = case sl of
                   StackedLayout l -> noPickFn $ titleToRenderable (_layout_margin l) (_layout_title_style l) (_layout_title l)
                   StackedLayoutLR l -> noPickFn $ titleToRenderable (_layoutlr_margin l) (_layoutlr_title_style l) (_layoutlr_title l)
        legendR = case sl of
                    StackedLayout l -> noPickFn $ renderLegend l $ fst legenditems
                    StackedLayoutLR l -> noPickFn $ renderLegendLR l legenditems
        
        legenditems = case (_slayouts_compress_legend slp,isBottomPlot) of
            (False,_) -> case sl of
                           StackedLayout l -> (getLegendItems l, [])
                           StackedLayoutLR l -> getLegendItemsLR l
            (True,True) -> allLegendItems
            (True,False) -> ([],[])
        
        mkPlotArea :: LayoutAxis x -> Grid (Renderable ())
        mkPlotArea axis = case sl of
          StackedLayout l -> fmap noPickFn 
                           $ layoutPlotAreaToGrid 
                           $ l { _layout_x_axis = axis }
          StackedLayoutLR l -> fmap noPickFn 
                             $ layoutLRPlotAreaToGrid 
                             $ l { _layoutlr_x_axis = axis }

        showLegend = not (null (fst legenditems)) || not (null (snd legenditems))

        isBottomPlot = i == length sls - 1

        lm = case sl of
          StackedLayout l -> _layout_margin l
          StackedLayoutLR l -> _layoutlr_margin l
        
        xAxis :: LayoutAxis x
        xAxis = case sl of
          StackedLayout l -> _layout_x_axis l
          StackedLayoutLR l -> _layoutlr_x_axis l
        
        usedAxis :: LayoutAxis x
        usedAxis = xAxis 
          { _laxis_generate = const (_laxis_generate xAxis all_xvals) }
        
    bg = case sl1 of
           StackedLayout l -> _layout_background l
           StackedLayoutLR l -> _layoutlr_background l
    
    getXVals :: StackedLayout x -> [x]
    getXVals (StackedLayout l) = getLayoutXVals l
    getXVals (StackedLayoutLR l) = getLayoutLRXVals l
    
    all_xvals = concatMap getXVals sls

    allLegendItems = (concatMap (fst.legendItems) sls, concatMap (snd.legendItems) sls)
    
    legendItems :: StackedLayout x -> ([LegendItem], [LegendItem])
    legendItems (StackedLayout l)   = (getLegendItems l, [])
    legendItems (StackedLayoutLR l) = getLegendItemsLR l
    
    noPickFn :: Renderable a -> Renderable ()
    noPickFn = mapPickFn (const ())

----------------------------------------------------------------------
    
addMarginsToGrid :: (Double,Double,Double,Double) -> Grid (Renderable a)
                 -> Grid (Renderable a)
addMarginsToGrid (t,b,l,r) g = aboveN [
     besideN [er, ts, er],
     besideN [ls, g,  rs],
     besideN [er, bs, er]
  ]
  where
    er = empty
    ts = tval $ spacer (0,t)
    ls = tval $ spacer (l,0)
    bs = tval $ spacer (0,b)
    rs = tval $ spacer (r,0)

titleToRenderable :: Double -> FontStyle -> String -> Renderable (LayoutPick x yl yr)
titleToRenderable _  _  "" = emptyRenderable
titleToRenderable lm fs s = addMargins (lm/2,0,0,0) (mapPickFn LayoutPick_Title title)
  where
    title = label fs HTA_Centre VTA_Centre s

mkLegend :: Maybe LegendStyle -> Double -> [LegendItem] -> Renderable (LayoutPick x yl yr)
mkLegend mls lm vals = case mls of
    Nothing -> emptyRenderable
    Just ls ->  case filter ((/="").fst) vals of
        []  -> emptyRenderable ;
        lvs -> addMargins (0,lm,lm,lm) $
                   mapPickFn LayoutPick_Legend $ legendToRenderable (Legend ls lvs)


data LayoutGridElements x yl yr = LayoutGridElements {
  lge_plots :: Renderable (LayoutPick x yl yr),
  
  lge_taxis :: (Maybe (AxisT x),String,FontStyle),
  lge_baxis :: (Maybe (AxisT x),String,FontStyle),
  lge_laxis :: (Maybe (AxisT yl),String,FontStyle),
  lge_raxis :: (Maybe (AxisT yr),String,FontStyle),

  lge_margin :: Double
}

buildGrid :: (Ord x, Ord yl, Ord yr) => LayoutGridElements x yl yr -> Grid (Renderable (LayoutPick x yl yr))
buildGrid lge = layer2 `overlay` layer1
  where
    layer1 = aboveN
         [ besideN [er,     er,  er,    er   ]
         , besideN [er,     er,  er,    weights (1,1) plots ]
         ]

    layer2 = aboveN
         [ besideN [er,     er,  tl,    taxis,  tr,    er,  er       ]
         , besideN [ltitle, lam, laxis, er,     raxis, ram, rtitle   ]
         , besideN [er,     er,  bl,    baxis,  br,    er,  er       ]
         , besideN [er,     er,  er,    btitle, er,    er,  er       ]
         ]

    er = tval emptyRenderable

    plots = tval $ lge_plots lge

    (tdata,_,_)         = lge_taxis lge
    (bdata,blbl,bstyle) = lge_baxis lge
    (ldata,llbl,lstyle) = lge_laxis lge
    (rdata,rlbl,rstyle) = lge_raxis lge

    -- (ttitle,_) = mktitle HTA_Centre VTA_Bottom   0 tlbl tstyle LayoutPick_XTopAxisTitle
    (btitle,_) = mktitle HTA_Centre VTA_Top      0 blbl bstyle LayoutPick_XBottomAxisTitle
    (ltitle,lam) = mktitle HTA_Right  VTA_Centre 270 llbl lstyle LayoutPick_YLeftAxisTitle
    (rtitle,ram) = mktitle HTA_Left   VTA_Centre 270 rlbl rstyle LayoutPick_YRightAxisTitle
    
    baxis = tval $ maybe emptyRenderable
                         (mapPickFn LayoutPick_XBottomAxis . axisToRenderable) bdata
    taxis = tval $ maybe emptyRenderable
                         (mapPickFn LayoutPick_XTopAxis . axisToRenderable) tdata
    laxis = tval $ maybe emptyRenderable
                         (mapPickFn LayoutPick_YLeftAxis . axisToRenderable) ldata
    raxis = tval $ maybe emptyRenderable
                         (mapPickFn LayoutPick_YRightAxis . axisToRenderable) rdata

    tl = tval $ axesSpacer fst tdata fst ldata
    bl = tval $ axesSpacer fst bdata snd ldata
    tr = tval $ axesSpacer snd tdata fst rdata
    br = tval $ axesSpacer snd bdata snd rdata

    mktitle :: HTextAnchor -> VTextAnchor 
            -> Double
            -> String -> FontStyle
            -> (String -> LayoutPick x yl yr) 
            -> ( Grid (Renderable (LayoutPick x yl yr))
               , Grid (Renderable (LayoutPick x yl yr)) )
    mktitle ha va rot lbl style pf = if lbl == "" then (er,er) else (labelG,gapG)
      where
        labelG = tval $ mapPickFn pf $ rlabel style ha va rot lbl
        gapG = tval $ spacer (lge_margin lge,0)

-- | Render the grids of the given axis to a plot area of given size.
renderGrids :: RectSize -> (Maybe (AxisT x), Maybe (AxisT yl), Maybe (AxisT x), Maybe (AxisT yr)) -> BackendProgram ()
renderGrids sz (bAxis, lAxis, tAxis, rAxis) = do
  maybeM () (renderAxisGrid sz) tAxis
  maybeM () (renderAxisGrid sz) bAxis
  maybeM () (renderAxisGrid sz) lAxis
  maybeM () (renderAxisGrid sz) rAxis

-- | Swap the contents of the pair depending on the flag.
optPairReverse :: Bool -> (a,a) -> (a,a)
optPairReverse rev (a,b) = if rev then (b,a) else (a,b)

-- | Render a single set of plot data onto a plot area of given size using
--   the given x and y axis.
renderSinglePlot :: RectSize -> Maybe (AxisT x) -> Maybe (AxisT y) -> Plot x y -> BackendProgram ()
renderSinglePlot (w, h) (Just (AxisT _ _ xrev xaxis)) (Just (AxisT _ _ yrev yaxis)) p =
  let xr = optPairReverse xrev (0, w)
      yr = optPairReverse yrev (h, 0)
      -- yrange = if yrev then (0, h) else (h, 0)
      pmfn (x,y) = Point (mapv xr (_axis_viewport xaxis xr) x)
                         (mapv yr (_axis_viewport yaxis yr) y)
      mapv lims _ LMin       = fst lims
      mapv lims _ LMax       = snd lims
      mapv _    f (LValue v) = f v
  in _plot_render p pmfn
renderSinglePlot _ _ _ _ = return ()

axesSpacer :: (Ord x, Ord y) 
           => ((Double, Double) -> Double) -> Maybe (AxisT x)
           -> ((Double, Double) -> Double) -> Maybe (AxisT y)
           -> Renderable a
axesSpacer f1 a1 f2 a2 = embedRenderable $ do
    oh1 <- maybeM (0,0) axisOverhang a1
    oh2 <- maybeM (0,0) axisOverhang a2
    return (spacer (f1 oh1, f2 oh2))

-- | Construct a axis for the given edge using the attributes 
--   from a 'LayoutAxis' the given values.
mkAxis :: RectEdge -> LayoutAxis z -> [z] -> Maybe (AxisT z)
mkAxis edge laxis vals = case axisVisible of
    False -> Nothing
    True  -> Just $ AxisT edge style rev adata
  where
    style = _laxis_style laxis
    rev   = _laxis_reverse laxis
    adata = _laxis_override laxis (_laxis_generate laxis vals)
    vis   = _axis_visibility adata
    axisVisible = _axis_show_labels vis || _axis_show_line vis || _axis_show_ticks vis

-- | Override the visibility of a selected axis with the selected 'AxisVisibility'.
overrideAxisVisibility :: layout 
                       -> (layout -> LayoutAxis z) 
                       -> (layout -> AxisVisibility) 
                       -> LayoutAxis z 
overrideAxisVisibility ly selAxis selVis = 
  let vis = selVis ly
  in (selAxis ly) { _laxis_override = (\ad -> ad { _axis_visibility = vis }) 
                                    . _laxis_override (selAxis ly)
                  }

mfill :: Maybe FillStyle -> Renderable a -> Renderable a
mfill Nothing   = id
mfill (Just fs) = fillBackground fs

-- | Empty 'LayoutLR' without title and plots. The background is white and 
--   the grid is drawn beneath all plots. There will be a legend. The top
--   axis will not be visible.


main :: IO ()
main = putStrLn "hey"
