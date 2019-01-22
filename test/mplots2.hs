import Control.Monad(void)
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Grid
import Graphics.Rendering.Chart.Backend.Cairo(renderableToFile)

-- haskell black scholes (see http://www.espenhaug.com/black_scholes.html)



-- Construct a single chart for the grid
bsChart :: Double -> Double -> Double -> Layout Double Double
bsChart t r v = execEC $ do
    layout_y_axis . laxis_generate .= scaledAxis def (-10,80)
    plot $ line "" [[(s,r*s) | s <- ss]] 
  where    
    ss = [50,51..150]
    lbl = "t = " ++ show t ++ ", r = " ++ show r



fitChart xs ys unboxedmodel p = execEC $ do
     layout_y_axis . laxis_generate .= scaledAxis def (-10,80)
     plot $ line "fit" [ zip xs ysfit ]
     plot $ points "data" $ zip xs ys 
  where 
    ysfit = map (unboxedmodel p) xs

-- Construct a grid of charts, with a single title accross the top
grid = title `wideAbove` aboveN [ layoutToGrid (bsChart 1 0.5 v) , layoutToGrid (fitChart xs ys model params) ]
  where
    ts = [1,2,5]
    rs = [0.05,0.10,0.20]
    v = 0.10
    xs = [1..50]
    ys = [1..50]
    model p x = x*(p !! 0) + (p !! 1)
    params = [1,5]
    title = setPickFn nullPickFn $ label ls HTA_Centre VTA_Centre "Black Scholes Option Values"
    ls = def { _font_size   = 15 , _font_weight = FontWeightBold }
    
main :: IO ()
main = do
  void $ renderableToFile def "example13_big.png" $ fillBackground def $ gridToRenderable $  grid
