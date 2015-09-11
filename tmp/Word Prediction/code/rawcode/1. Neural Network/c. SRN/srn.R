source("backpropLib.R")

# this is the input set of sentences
sentences <- c(
  "the boy eats the cake .",
  "the boy chases the girl .",
  "the dog eats the apple .",
  "the dog chases the cat .",
  "the girl eats the apple .",
  "the girl chases the boy .",
  "the cat chases the dog .",
  "the cat eats the cake .")

# these sentences are then converted into codes for the model to use
wordseqlabels = unlist(str_split(sentences, " "))
input = data.frame(prevword=c(".",wordseqlabels), nextword = c(wordseqlabels,"."))
vocabulary = c(".","the","girl","boy","dog","cat","cake","apple", "chases","eats" )    
input$prevword = factor(input$prevword,levels = vocabulary)
input$nextword = factor(input$nextword,levels = vocabulary)
vocabulary = levels(input$nextword)
print(vocabulary)

# parameters for the size of layers in the model
NumInputs=length(vocabulary)
NumCompress = 4
NumHidden=8
NumOutputs=length(vocabulary)

convertWordVector= function(w,s){
  vec = rep(0,NumInputs)
  vec[w] = 1
  vec2 = rep(0,NumInputs)
  vec2[s] = 1
  return(c(vec,vec2))
}
InputOutput = t(mapply(convertWordVector,input$prevword,input$nextword))
periodList = InputOutput[,1]

Inputs = InputOutput[,1:NumInputs]
Inputs = convertRange(Inputs,0.-0.9,0.9) 
# inputs range between -1 and 1
print(Inputs[1:6,])
Targets = InputOutput[,(NumInputs+1):(dim(InputOutput)[2])]
# output units use softmax, so targets are between 0 and 1
print(Targets[1:6,])

layerList <- list()    # reset network
getLayerList <- list() # reset hash for layers
# this function creates SRN model
createSRN <- function(){  
  makeLayer("input", NumInputs)
  makeLayer("ccompress", NumCompress)
  makeContextLayer("context", NumHidden, Inputs[,1])
  makeLayer("hidden", NumHidden)
  makeLayer("compress", NumCompress)
  makeLayer("output", NumOutputs)
  
  makeLink("input","ccompress")
  makeLink("ccompress","hidden")
  makeLink("hidden","context")
  makeLink("context","hidden")
  makeLink("hidden","compress")
  makeLink("compress","output")  
}

# this function sets the inputs nad targets
# the context layer gets periodList, which tells it to reset the context to 0.5 
# at start of sentence
setInputs <- function(inp,tar,perlist){
  # set number of patterns in network
  setParamAll("patn",dim(tar)[1])  

  layerList[[getLayer("input")]]$output <<- inp  # set input
  
  layerList[[getLayer("context")]]$reset <<- perlist  # this tells context when to reset
  hidlay = layerList[[getLayer("hidden")]]
  hidlay$output <- matrix(0.5,hidlay$patn,hidlay$unitn) # hidden layer also needs to be reset
  layerList[[getLayer("hidden")]] <<- hidlay            # since context copies from hidden
  
  layerList[[getLayer("output")]]$target <<- tar
  class(layerList[[getLayer("output")]]) <<- c("softmax","output","layer")  
}

# Training Parameters
##############
numEpochs<-10000
## create and setup model
createSRN()
setParamAll("lrate",0.1) # learning rate: speed of learning
setParamAll("momentum",0.9) # amount of previous weight changes that are added
setParamAll("boundedDescent",TRUE)  # bounded descent algorithm is used
setParamAll("hysteresis",0.5)  # use half of previous context activation
setParamAll("zeroErrorRadius",0.1)  # error < 0.1 is set to zero
setInputs(Inputs,Targets,periodList)
resetNetworkWeights()

ptm <- proc.time()

## train model
history=data.frame()
for (epoch in 1:numEpochs){
  forwardPass() # spread activation forward for all patterns in training set       
  backpropagateError()  # back propagate error and update weights

  if (epoch %% 1000==0){ # every 1000 epochs, test the model and plot MSDelta graph
    slice = evaluateModelFit(epoch)  # save model parameters for figures
    history = rbind(history,slice)
    # plot delta during training to see how layers are changing
  #  deltaPlot = ggplot(history,aes(x=time,y=MSDelta,colour=Layer))+geom_line()
  #  print(deltaPlot)
  }
}  

print(proc.time() - ptm)
# plot cost function
costPlot <- ggplot(history,aes(x=time,y=Cost))+geom_line()+ylab("Cross-entropy")
print(costPlot)

# plot the model output for the first 6 patterns
layerList[[getLayer("compress")]]$verticalPos = 3.5
layerList[[getLayer("ccompress")]]$verticalPos = 3.5
layerList[[getLayer("hidden")]]$verticalPos = 1
input$prevword = factor(input$prevword,levels = vocabulary)
input$nextword = factor(input$nextword,levels = vocabulary)
sentseq = paste(input$prevword ,input$nextword,sep="->")
modelout = addModel("output",restrict=TRUE,labels=sentseq)
dd = subset(modelout, pattern %in% 1:12)
plotModel(dd,ylabel=vocabulary,axisfontsize=10)

