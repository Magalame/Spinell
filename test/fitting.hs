-- nonlinear least-squares fitting


import Spinell.Fitting
import Spinell.Graphing
import Numeric.LinearAlgebra

model [a,lambda,b] t = a * exp (-lambda * t)+b --the function we will be fitting

jac [a,lambda,b] [t] = [[exp (-lambda * t), -t * a * exp(-lambda*t) , 1]]

xs = [0..100]

sigma = replicate (length xs) 0.1 -- will create an array of repeating '0.1': [0.1,0.1...]

noise = toList $ scalar 0.1 * (randomVector 0 Gaussian (length xs)) --a list of random small values, that will play the role of the noise in the data

ys = map (model [5,0.1,1]) xs --we plainly apply the model to the data

ysNoisy = [ y + err | (y,err) <- zip ys noise] -- and then add the noise, just to make it look more realistic

res = fit xs ys sigma model [1,1,1] defFitOpt{jacob = ManualJacob jac}

main :: IO()
main = plotFit xs ysNoisy sigma model (getParams res) defGraphOpt
