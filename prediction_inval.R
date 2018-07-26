fit = lm(mpg ~ hp + wt, data = mtcars)
newcar = data.frame(hp = 90, wt = 2.2)
predict(fit, newdata = newcar)
predict(fit, newdata = newcar, interval = "confidence") #The confidence interval at the given point of the prediction surface.
predict(fit, newdata = newcar, interval = "prediction") #The prediction interval at the point of the prediction surface.

library(dplyr)
y = mtcars$mpg
x = as.matrix(cbind(1, select(mtcars, hp, wt)))
n = length(y)
p = ncol(x)
xtxinv = solve(t(x) %*% x)
beta = xtxinv %*% t(x) %*% y
