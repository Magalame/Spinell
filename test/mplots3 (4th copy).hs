import Control.Monad(void)
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Grid
import Graphics.Rendering.Chart.Backend.Cairo


-- constructs the data and fit chart
fitChart xs = layout
  where

    fit = plot_lines_values .~ [[ (x,x-5) | x <- xs ]]
         $ plot_lines_style  . line_color .~ opaque red
         $ plot_lines_title .~ "Fit"
         $ def

    layout = layout_title .~ ""
           $ layout_plots .~ [toPlot fit]
           $ layout_x_axis .~ x_axis
           $ layout_y_axis .~ y_axis
           $ def

    x_axis = laxis_title .~ "default x"
             $ def

    y_axis = laxis_title .~ "default y"
             $ def


main :: IO ()
main = do
  void $ renderableToFile (def{_fo_format = PDF}) "example13_big.pdf" $ fillBackground def $ gridToRenderable $ layoutToGrid (fitChart xs) 
  where

    xs = [1..50]::[Double]


