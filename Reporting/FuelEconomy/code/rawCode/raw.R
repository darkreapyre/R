#Pre-requisites
attach(mtcars)
library(ggplot2)

#Basic Plotting
pairs(mtcars, main = "mtcars data")
coplot(mpg ~ am , data = mtcars,
panel = panel.smooth, rows = 1)

#Qplot 
#Not really good to show scatter
qplot(mpg, am == 1, data = mtcars, geom=c( "point" ,
                                      "smooth" ),method =  "lm")


# 3D Scatterplot with Coloring and Vertical Drop Lines
library(scatterplot3d) 
attach(mtcars) 
scatterplot3d(mpg, am, pch=16, highlight.3d=TRUE,
              type="h", main="3D Scatterplot")


library(Rcmdr)
attach(mtcars)
scatter3d(am, mpg)


library(ggplot2)

ggplot(mtcars, aes(factor(am), mpg, fill=factor(am))) + geom_boxplot()


############################################################################
#                                   Plot 1                                 #
############################################################################
par(mfrow = c(1, 2))
boxplot(mtcars$mpg ~ mtcars$gear, subset = (mtcars$am == 0),
        xlab = "Gears", ylab = "Miles per Gallon", col = "red",
        main = "Automatic")
boxplot(mtcars$mpg ~ mtcars$gear, subset = (mtcars$am == 1),
        xlab = "Gears", ylab = "Miles per Gallon", col = "blue",
        main = "Manual")
#############################################################################

mean_mpg <- round(tapply(mtcars$mpg, mtcars$am, mean), 2)
vehicle_sd <- round(tapply(mtcars$mpg, mtcars$am, sd), 2)

sum_auto <- sum(mtcars$am == 0)
sum_manual <- sum(mtcars$am == 1)

#Factorize the variables with catagories
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)
levels(mtcars$am) <- c("Auto", "Man") #Rename Transmission Type
mtcars$gear <- as.factor(mtcars$gear)
mtcars$carb <- as.factor(mtcars$carb)

#Assumption: Transmission type affect mpg
fit <- lm(mpg ~ am, data = mtcars)
summary(fit)

#Fit a regressin model on ALL the data
full_fit <- lm(mpg~., data = mtcars)
summary(full_fit)

#Show the highest corrleated coeficients
summary(full_fit)$coefficients

#Find the best model using the step() funciton
best_fit <- step(full_fit, direction = "both")
best_call <- as.list(best_fit$call)[[2]]

#Show the coefficents of the best fit
summary(best_fit)
summary(best_fit)$coefficients

#ANOVA Test
results <- anova(fit, full_fit, best_fit)

#plot best fit
par(mfrow = c(2,2))
plot(best_fit)



#Interaction plot
attach(mtcars)
gears <- factor(gears)
trans <- factor(am)
interaction.plot(trans, gear, mpg, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22), 
                 xlab="Number of Cylinders", 
                 ylab="Mean Miles Per Gallon", 
                 main="Interaction Plot")
#############################################################################
#                              Appendix A                                   #
#############################################################################

pairs(mtcars, panel=panel.smooth, main="Pair-wise Plot")

############################################################################





one <- as.list(fit$call)
two <- as.list(full_fit$call)
three <- as.list(best_fit$call)