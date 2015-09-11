## XOR in R
## errors should be reported to chang.franklin@gmail.com
## August 2014

## Place backpropLib.R and other files in the same folder on your desktop
## you need to set the working directory to this folder
# one way is to go to the Session menu, Set Working Directory, To Source File location
# or set the folder directly like this
#setwd("~/Desktop/neural/") 
source("backpropLib.R")

# this is the function that the model is trying to learn.  
# Exclusive OR is difficult, because it is a non-linear mapping
#            Inputs  Target
xor = matrix(c(0,0,   0, 
               0,1,   1, 
               1,0,   1, 
               1,1,   0  ), nrow=4, byrow=TRUE)
## tanh requires range of [-1,1]
xor2 = convertRange(xor,-0.9,0.9)
Inputs = xor2[,1:2]
print(Inputs)
Targets = matrix(xor2[,3])
print(Targets)

# To see that it is non-linear, we can use regression to learn this mapping
xor2.df = data.frame(xor2)
names(xor2.df) = c("Input1","Input2","Target")
fit = lm(Target ~ Input1 + Input2, xor2.df)
print(summary(fit))
xor2.df$lmpred = predict(fit)
xor2.df$lmerror = abs(xor2.df$lmpred - xor2.df$Target)
print(xor2.df)
# the pred column should be the same as the Target (error should be 0), but it is not.
# XOR is not a learnable as a linear combination of weights
# if interactions are introduced, the regression can learn this mapping.
fitInt = lm(Target ~ Input1 + Input2 + Input1:Input2, xor2.df)
xor2.df$lmpredInt = predict(fitInt)
xor2.df$lmerrorInt = round(abs(xor2.df$lmpredInt - xor2.df$Target),3)
print(xor2.df)
# Neural networks can learns internal representations 
# that act like this interaction term

### Neural network with three layers
# this initializes a model with 2 input, 2 hidden, 1 output
NumInputs = 2
NumHidden = 2
NumOutputs = 1
layerList <- list()    # reset network
getLayerList <- list() # reset hash for layers
makeLayer("input", NumInputs)
makeLayer("hidden", NumHidden)
makeLayer("output", NumOutputs)
makeLink("input","hidden") # create weights from input to hidden
makeLink("hidden","output") # create weights from hidden to output
class(layerList[[getLayer("output")]] ) <- c("output","layer") # set output layer as output class

# Traning Parameters
##############
numEpochs<-1000    # number of training cycles thru whole batch
setParamAll("lrate",0.01) # learning rate: speed of learning
setParamAll("momentum",0.9) # amount of previous weight changes that are added
setParamAll("patn",4)  # four patterns
resetNetworkWeights()  # randomize network weights
layerList[[getLayer("input")]]$output <- Inputs    # set inputs
layerList[[getLayer("output")]]$target <- Targets  # set target

# network is in layerList variable
print(layerList)
layerList[[2]]   # this accesses the hidden layer
layerList[[2]]$output  # this accesses the output field in the hidden layer 

# this plots the model so that you can see the architecture and initial weights
# each panel represents one of the four patterns in XOR
# input and target are set by the patterns, the rest get their activation from the weights
layerList[[getLayer("output")]]$verticalPos = 0.5
modelout = addModel("output")
arr=createWeightArrows(modelout)
plotModel(modelout,xlabel=c("Input","", "Hidden","", "Output","Target"),arr=arr,col=1) 

# Train model 1000 times
############
history=data.frame()  # stores evaluation data during training
for (epoch in 1:numEpochs){
  
  forwardPass()  # spread activation forward for all patterns in training set
  backpropagateError()  # back propagate error and update weights

  slice = evaluateModelFit(epoch)  # save model parameters for figures
  slice$Hidden1 = layerList[[length(layerList)]]$weights[1]
  slice$Hidden2 = layerList[[length(layerList)]]$weights[2]
  slice$Bias = layerList[[length(layerList)]]$weights[3]
  history = rbind(history,slice)
}
# this shows how Cross entropy loss falls over time
costPlot = ggplot(history,aes(x=time,y=Cost))+geom_line()+ylab("Cross-entropy")
print(costPlot)
# this shows the changes in the weight change matrix (delta).  
# there is a peak in the middle of training as the model recodes hidden layer for XOR
deltaPlot = ggplot(subset(history, time > 5),aes(x=time,y=MSDelta,colour=Layer))+geom_line()
deltaPlot = deltaPlot + theme(legend.position="top",legend.direction ="horizontal")
print(deltaPlot)

# Here is model after training (output and target match)
modelout = addModel("output")
arr=createWeightArrows(modelout)
plotModel(modelout,xlabel=c("Input","", "Hidden","", "Output","Target"),arr=arr,col=1) 

## plots for hidden1,hidden2,bias to output unit
## dots show path through space (darker as learning takes place) 
weight12fig = mapOutWeightSpace("Hidden1","Hidden2","output",history)   
weight13fig = mapOutWeightSpace("Hidden1","Bias","output",history)  
layout <- matrix(c(1,2,3,4),ncol = 2, nrow = 2)
print(multiplot(costPlot,deltaPlot,weight12fig,weight13fig , layout=layout))

# this function is not used, but shows you how to access the parts of the model
examineNetwork <- function(){
  # The model is stored in the list layerList
  layerList  # this prints out the whole model
  # or you can look at the layer names 
  getVal("name")
  # you can also access a particular layer like this 
  # this prints out the delta matrix for the output layer
  layerList[[getLayer("output")]]$delta
  # to change the learning rate for the output directly
  layerList[[getLayer("output")]]$lrate <- 0.2
  # to change the global variable we need to do this
  layerList[[getLayer("output")]]$lrate <<- 0.2
}

# we add the model's predicted output to the xor2.df data frame to compare with regression
forwardPass()
xor2.df$nnpred = layerList[[length(layerList)]]$output
xor2.df$nnerror = abs(xor2.df$nnpred - xor2.df$Target)
print(xor2.df)
# nnpred shows the model's predicted output
# nnerror shows the difference from target.  Values are small
