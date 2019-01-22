import Control.Monad(void)
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Grid
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Backend.Cairo
import Numeric.LinearAlgebra

--constructs the residuals chart
residualsChart xs ys sigma unboxedmodel p = layout
  where

    residuals :: [(Double,Double)]
    residuals = [ ( xs !! i , ((ys !! i) - (unboxedmodel p (xs !! i)))/( sigma !! i) ) | i <- [0..((length xs)-1)]] 

    errors = plot_errbars_values .~ [ symErrPoint x resid 0 1 | (x,resid) <- residuals ]
  --       $ plot_errbars_title .~"Error on data"
         $ def

    dat = plot_points_style .~ filledCircles 4 (opaque blue)
           $ plot_points_values .~ residuals
    --       $ plot_points_title .~ "Data"
           $ def

    fit = plot_lines_values .~ [[ (x,0) | x <- xs]]
         $ plot_lines_style  . line_color .~ opaque red
         $ plot_lines_style  . line_width .~ 3
     --    $ plot_lines_title .~ "Fit"
         $ def

    layout = layout_title .~ ""
           $ layout_plots .~ [toPlot fit, toPlot errors, toPlot dat]
           $ layout_legend .~ Nothing
           $ layout_bottom_axis_visibility .~ bottom_res_visibility
     --      $ layout_margin.~ 0
           $ layout_y_axis .~ y_axis
           $ def


    y_axis = laxis_title .~ "Std. Res."
             $ laxis_title_style . font_name .~ "Latin Modern Roman"
       --      $ laxis_title_style . font_slant .~ FontSlantItalic
             $ laxis_title_style . font_size .~ 25
             $ laxis_style . axis_label_style . font_size .~ 18 $ def

    bottom_res_visibility = axis_show_labels .~ False
                            $ def

-- constructs the data and fit chart
fitChart xs ys sigma unboxedmodel p = layout
  where

    vals :: [(Double,Double,Double,Double)]
    vals = [ (xs !! i,ys !! i, 0 , sigma !! i) | i <- [0..((length xs)-1)]]

    errors = plot_errbars_values .~ [ symErrPoint x y dx dy | (x,y,dx,dy) <- vals ]
         $ plot_errbars_title .~"Error on data"
         $ def

    dat = plot_points_style .~ filledCircles 4 (opaque blue)
           $ plot_points_values .~ [ (x,y) |  (x,y,dx,dy) <- vals ]
           $ plot_points_title .~ "Data"
           $ def

    fit = plot_lines_values .~ [[ (x,(unboxedmodel p x)) | (x,y,dx,dy) <- vals ]]
         $ plot_lines_style  . line_color .~ opaque red
         $ plot_lines_style  . line_width .~ 3
         $ plot_lines_title .~ "Fit"
         $ def

    layout = layout_title .~ ""
           $ layout_plots .~ [toPlot fit, toPlot errors, toPlot dat]
           $ layout_x_axis .~ x_axis
           $ layout_y_axis .~ y_axis
           $ layout_legend .~ Nothing
           $ layout_margin .~ 0
           $ def

    x_axis = laxis_title .~ "x"
             $ laxis_title_style . font_name .~ "Latin Modern Roman"
             $ laxis_title_style . font_slant .~ FontSlantItalic
             $ laxis_title_style . font_size .~ 30
             $ laxis_style . axis_label_style . font_size .~ 20 $ def 

    y_axis = laxis_title .~ "y"
             $ laxis_title_style . font_name .~ "Latin Modern Roman"
             $ laxis_title_style . font_slant .~ FontSlantItalic
             $ laxis_title_style . font_size .~ 30
             $ laxis_style . axis_label_style . font_size .~ 20 $ def

-- Construct a grid of charts, with a single title accross the top
grid = aboveN [weights (0,0.001) (layoutToGrid (residualsChart xs ys sigma model params)), layoutToGrid (fitChart xs ys sigma model params) ]
  where

    xs = [1..50]
    ys = toList $ fromList [1..50] + scalar 1 * (randomVector 0 Gaussian 50)
    sigma = repeat 1
    model p x = x*(p !! 0) + (p !! 1)
    params = [1,0]


--main :: IO ()
--main = do
--  void $ renderableToFile (def{_fo_format = PDF}) "example13_big.pdf" $ fillBackground def $ addMargins (0,0,0,50)  $ gridToRenderable $  grid

main :: IO ()
main = do
    let
        tmp = renderableToWindow $ fillBackground def $ addMargins (0,0,0,50)  $ gridToRenderable $  grid
    
    void $ tmp 500 300
    




