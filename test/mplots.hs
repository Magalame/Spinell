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
    plot $ points "" [(s,r*s) | s <- ss] 
  where    
    ss = [50,51..150]
    lbl = "t = " ++ show t ++ ", r = " ++ show r

-- Construct a grid of charts, with a single title accross the top
grid = title `wideAbove` aboveN [ layoutToGrid (bsChart 1 0.25 v), layoutToGrid (bsChart 1 0.5 v)]
  where
    ts = [1,2,5]
    rs = [0.05,0.10,0.20]
    v = 0.10
    title = setPickFn nullPickFn $ label ls HTA_Centre VTA_Centre "Black Scholes Option Values"
    ls = def { _font_size   = 15 , _font_weight = FontWeightBold }
    
main :: IO ()
main = do
  void $ renderableToFile def "example13_big.png" $ fillBackground def $ gridToRenderable $  grid
