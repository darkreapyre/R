attach(mtcars)

#Basic Plotting
pairs(mtcars, main = "mtcars data")
coplot(mpg ~ am , data = mtcars,
panel = panel.smooth, rows = 1)
