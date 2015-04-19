###############################################################################
#                           Data Processing                                   #
###############################################################################
data(mtcars)

###############################################################################
#                          Initial Analysis                                   #
###############################################################################

#Summaries
str(mtcars)
summary(mtcars)

#Box Plot
mean_mpg <- as.list(round(tapply(mtcars$mpg, mtcars$am, mean), 2))
par(mfrow = c(1, 2))
boxplot(mtcars$mpg ~ mtcars$gear, subset = (mtcars$am == 0),
        xlab = "Gears", ylab = "Miles per Gallon", col = "red",
        main = "Automatic")
boxplot(mtcars$mpg ~ mtcars$gear, subset = (mtcars$am == 1),
        xlab = "Gears", ylab = "Miles per Gallon", col = "blue",
        main = "Manual")

#############################################################################
#                       Data Transformation                                 #
#############################################################################
#Factorize the variables with catagories
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)
levels(mtcars$am) <- c("Auto", "Man") #Rename Transmission Type
mtcars$gear <- as.factor(mtcars$gear)
mtcars$carb <- as.factor(mtcars$carb)

#############################################################################
#                       Regression Analysis                                 #
#############################################################################
##Initial Assumed fit
#Fit the model
fit <- lm(mpg~am, data = mtcars)
summary(fit)

##full_fit##
#Fit a regression model on ALL the data
full_fit <- lm(mpg~., data = mtcars)

#Show the highest corrleated coeficients
summary(full_fit)

##best_fit##
#Find the best model using the step() funciton
best_fit <- step(full_fit, direction = "both")

#Show the coefficents of the best fit
summary(best_fit)

#############################################################################
#                              Appendix A                                   #
#############################################################################
#Pair-wise plot
pairs(mtcars, panel=panel.smooth, main="Pair-wise Plot")

#############################################################################
#                              Appendix B                                   #
#############################################################################
#Plot the best fit
par(mfrow = c(2,2))
plot(best_fit)
