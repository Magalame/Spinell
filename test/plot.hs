import Numeric.LinearAlgebra
import Graphics.Plot
import Numeric.GSL.Special(erf_Z, erf)

sombrero n = f x y where 
    (x,y) = meshdom range range
    range = linspace n (-2,2)
    f x y = exp (-r2) * cos (2*r2) where 
        r2 = x*x+y*y

f x =  sin x + 0.5 * sin (5*x)

gaussianPDF = erf_Z
cumdist x = 0.5 * (1+ erf (x/sqrt 2))

-- to fit data
expModel [a,lambda,b] [t] = [a * exp (-lambda * t)+b]

expModelDer [a,lambda,b] [t] = [[exp (-lambda * t), -t * a * exp(-lambda*t) , 1]]

-- to generate data
expModel2 [a,lambda,b] t = a * exp (-lambda * t)+b

expModelDer2 [a,lambda,b] t = [[exp (-lambda * t), -t * a * exp(-lambda*t) , 1]]

xs = [a | a <- [0..100]]
sigma = [0.1]
ys = (map (expModel2 [5,0.1,1]) xs)

main = do
    --mplot [f x]
    mplot [fromList xs,fromList ys]
    --mesh (sombrero 40)
   -- print [x, cmap cumdist x, cmap gaussianPDF x]
