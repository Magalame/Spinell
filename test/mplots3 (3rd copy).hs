{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}


import Control.Monad(void)
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Grid
import Graphics.Rendering.Chart.Backend.Cairo(renderableToFile)
import qualified Helper


--constructs the residuals chart
residualsChart xs ys sigma unboxedmodel p = layout
  where

    residuals :: [(Double,Double)]
    residuals = [ ( xs !! i , ((ys !! i) - (unboxedmodel p (xs !! i)))/( sigma !! i) ) | i <- [0..((length xs)-1)]] 

    errors = plot_errbars_values .~ [ symErrPoint x resid 0 1 | (x,resid) <- residuals ]
         $ plot_errbars_title .~"Error on data"
         $ def

    dat = plot_points_style .~ filledCircles 2 (opaque blue)
           $ plot_points_values .~ residuals
           $ plot_points_title .~ "Data"
           $ def

    fit = plot_lines_values .~ [[ (x,0) | x <- xs]]
         $ plot_lines_style  . line_color .~ opaque red
         $ plot_lines_title .~ "Fit"
         $ def

    layout = layout_title .~ ""
           $ layout_plots .~ [toPlot dat, toPlot errors, toPlot fit]
           $ layout_legend .~ Nothing
           $ layout_margin.~ 0
           $ layout_x_axis .~ x_axis
           $ layout_y_axis .~ y_axis
           $ def

    x_axis = def

    y_axis = laxis_title .~ "default y"
             $ def

-- constructs the data and fit chart
fitChart xs ys sigma unboxedmodel p = layout
  where

    vals :: [(Double,Double,Double,Double)]
    vals = [ (xs !! i,ys !! i, 0 , sigma !! i) | i <- [0..((length xs)-1)]]

    errors = plot_errbars_values .~ [ symErrPoint x y dx dy | (x,y,dx,dy) <- vals ]
         $ plot_errbars_title .~"Error on data"
         $ def

    dat = plot_points_style .~ filledCircles 2 (opaque blue)
           $ plot_points_values .~ [ (x,y) |  (x,y,dx,dy) <- vals ]
           $ plot_points_title .~ "Data"
           $ def

    fit = plot_lines_values .~ [[ (x,(unboxedmodel p x)) | (x,y,dx,dy) <- vals ]]
         $ plot_lines_style  . line_color .~ opaque red
         $ plot_lines_title .~ "Fit"
         $ def

    layout = layout_title .~ ""
           $ layout_plots .~ [toPlot dat, toPlot errors, toPlot fit]
           $ layout_x_axis .~ x_axis
           $ layout_y_axis .~ y_axis
           $ def

    x_axis = laxis_title .~ "default x"
             $ def

    y_axis = laxis_title .~ "default y"
             $ def


mkStack ls = 
  renderStackedLayouts 
  $ slayouts_layouts .~ ls
  $ def



-- Construct a grid of charts, with a single title accross the top
grid = mkStack [StackedLayout (residualsChart xs ys sigma model params),StackedLayout (fitChart xs ys sigma model params)]
  where

    xs = [1..50]
    ys = [1..50]
    sigma = repeat 1
    model p x = x*(p !! 0) + (p !! 1)
    params = [1,5]
    title = setPickFn nullPickFn $ label ls HTA_Centre VTA_Centre "Black Scholes Option Values"
    ls = def { _font_size   = 15 , _font_weight = FontWeightBold }

-- from https://github.com/timbod7/haskell-chart/issues/194
splitGrid :: Grid (Renderable a) -> Grid (Renderable a) -> Grid(Renderable a)
splitGrid top bottom = aboveN [
    besideN [
        aboveN $ replicate 2 $ tval $ spacer (0, (fromIntegral (height bottom))*5),
        tspan ( gridToRenderable top ) (width bottom, 2)
     ],
     bottom
  ]

main :: IO ()
main = do
  void $ renderableToFile def "example13_big.png" $ fillBackground def grid



---------------------------


