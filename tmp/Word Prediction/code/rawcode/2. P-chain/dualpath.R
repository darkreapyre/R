## Dual-path Model in R
## errors should be reported to chang.franklin@gmail.com
## August 2014

## Place backpropLib.R and other files in the same folder on your desktop
## you need to set the working directory to this folder
# one way is to go to the Session menu, Set Working Directory, To Source File location
# or set the folder directly like this
#setwd("~/Desktop/neural/") 
source("backpropLib.R")

set.seed(10)
# the Japanese input language is randomly generated using this code
# data frame is constructed with various columns that are used to generate sentences
npat = 100
actors = c("boy","girl","dog","cat")
foods = c("apple","cake")
constrsent.df = data.frame(str=ifelse(runif(npat) < 0.3,"P","A"),act=ifelse(runif(npat) < 0.5,"eats","chases"))
constrsent.df$agent = actors[sample(1:4,npat,replace=TRUE)]
constrsent.df$patient = actors[sample(1:4,npat,replace=TRUE)]
constrsent.df$patient[constrsent.df$act == "eats"] = foods[sample(1:2,length(constrsent.df$patient[constrsent.df$act == "eats"]),replace=TRUE)]
constrsent.df$mess = paste("action=",constrsent.df$act," agent=",constrsent.df$agent," patient=",constrsent.df$patient,sep="")
constrsent.df$sent = paste(constrsent.df$agent,"ga",constrsent.df$patient,"o",constrsent.df$act,".")
constrsent.df$psent = paste(constrsent.df$patient,"o",constrsent.df$agent,"ga",constrsent.df$act,".")
constrsent.df$sent[constrsent.df$str == "P"] = constrsent.df$psent[constrsent.df$str == "P"]
constrsent.df$psent = NULL
print(constrsent.df)


## convert sentences into input for model
sentences = constrsent.df$sent
messages = constrsent.df$mess
roles = c("action","agent","patient")

wordseqlabels = unlist(str_split(sentences, " "))
input = data.frame(prevword=c(".",wordseqlabels), nextword = c(wordseqlabels,"."))
input = input[1:(length(input[,1])-1),]
vocabulary = c(".","ga","o","boy","girl","dog","cat","cake","apple", "chases","eats")    
input$prevword = factor(input$prevword,levels = vocabulary)
input$nextword = factor(input$nextword,levels = vocabulary)
vocabulary = levels(input$nextword)
print(vocabulary)

## Parameters for size of layers in model
NumInputs=length(vocabulary)
NumCompress = 3
NumHidden=10
NumOutputs=length(vocabulary)
NumConcepts=NumOutputs
NumRoles=4

convertWordVector= function(w,s){
  vec = rep(0,NumInputs)
  vec[w] = 1
  vec2 = rep(0,NumInputs)
  vec2[s] = 1
  return(c(vec,vec2))
}

InputOutput = t(mapply(convertWordVector,input$prevword,input$nextword))
periodList = InputOutput[,1]
periodPosition = which(periodList==1)
periodPosition = c(periodPosition,length(periodList)+1)
sentlen = periodPosition[2:(length(periodPosition))]-periodPosition[1:(length(periodPosition)-1)]
xx = 0
sentnum = c(unlist(lapply(sentlen,function(x) {xx<<-xx+1; rep(xx,x)})),10)
sentnum= sentnum[1:length(sentnum)-1]
input$prevword = factor(input$prevword,levels = vocabulary)
input$nextword = factor(input$nextword,levels = vocabulary)
sentseq = paste(input$prevword ,input$nextword,sep="->")

Targets = InputOutput[,(NumInputs+1):(dim(InputOutput)[2])]
# targets are localist, because softmax function on output
print(Targets[1:6,])
Inputs = InputOutput[,1:NumInputs]
Inputs = convertRange(Inputs,0.-0.9,0.9)
print(Inputs[1:6,])

## We also want to test the model on test sentences 
## that the model has never seen in training
## these sentences violate the animal eats food constraint
## and also uses scrambling with that violation
tmessagesSentences =c(
  "dog ga boy o eats .","action=eats agent=dog patient=boy",
  "girl o cat ga eats .","action=eats agent=cat patient=girl"  
)

testsentences = tmessagesSentences[c(1,3)]
tmessages = tmessagesSentences[c(2,4)]

twordseqlabels = unlist(str_split(testsentences, " "))
tinput = data.frame(prevword=c(".",twordseqlabels), nextword = c(twordseqlabels,"."))
tinput = tinput[1:(length(tinput[,1])-1),]
tinput$prevword = factor(tinput$prevword,levels = vocabulary)
tinput$nextword = factor(tinput$nextword,levels = vocabulary)
tsentseq = paste(tinput$prevword ,tinput$nextword,sep="->")

tInputOutput = t(mapply(convertWordVector,tinput$prevword,tinput$nextword))
tPeriodList = tInputOutput[,1]
tTargets = tInputOutput[,(NumInputs+1):(dim(tInputOutput)[2])]
print(tTargets[1:6,])
tInputs = tInputOutput[,1:NumInputs]
tInputs = convertRange(tInputs,0.-0.9,0.9)
print(tInputs[1:6,])

## This is a function that converts the message into numbers for each role/concept
## uses same order as vocabulary for concepts
convertMessageCodes <- function(mess){
  allMesList = list()
  for (m in mess){
    #  print(m)
    mesList = ""
    if (m != ""){
      pairs = str_split(m, " ")
      for (p in pairs[[1]]){
        rc=str_split_fixed(p,"=",2)
        pairList = paste(which(roles == rc[1]), which(vocabulary == rc[2]),sep=",")
        #     print(pairList)
        mesList = paste(mesList, pairList, sep=",")
      }
    }
    allMesList = append(allMesList,mesList)
  }
  unlist(allMesList)
}

## this creates a weight matrix for each message
## roles on rows, concepts on columns
makeMessages <- function(m,r,c,rc){
  ml = str_split(m[[1]],",")
  ml2 = as.numeric(ml[[1]])
  wts = matrix(0,r,c)
  wts[r,] = -2
  if (length(ml2)>2){
    for (i in seq(2,length(ml2),2)){
      if (rc == TRUE){
        wts[ml2[i],ml2[i+1]] <- 4
      }else{
        wts[ml2[i+1],ml2[i]] <- 4      
      }
    }
  }
  wts
}

# this creates the event-semantic part of the message (e.g., number of roles)
makeEventSem <- function(mes, inp,slen){
  
  rolelist = str_extract_all(mes,"(agent|patient)")
  mlist=NULL
  for (rl in 1:length(rolelist)){
    if (!is.na(slen[rl])){
      mat = matrix(-0.9,slen[rl],NumRoles)
      for (r in rolelist[rl][[1]]){
        for (i in 1:length(roles)){
          if (roles[i]==r){
            mat[,i]=0.9
          }
        }
      }
      mlist = rbind(mlist,mat)
    }
  }
  rbind(mlist , mlist[dim(mlist)[1],])
}

layerList <- list()    # reset network
getLayerList <- list() # reset hash for layers

# setup the dual-path architecture
createDualpath <- function(){
  pat = dim(Targets)[1]

  makeLayer("input", NumInputs)
  makeLayer("cconcepts", NumOutputs)  
  makeLayer("croles", NumRoles)
  makeLayer("eventsem", NumRoles)
  makeContextLayer("context", NumHidden, periodList)
  makeLayer("hidden", NumHidden)
  makeLayer("compress", NumCompress)
  makeLayer("roles", NumRoles)
  makeLayer("concepts", NumOutputs)  
  makeLayer("output", NumOutputs)
  
  makeLink("input","hidden")
  makeLink("input","cconcepts")
  makeLink("cconcepts","croles")
  makeLink("croles","hidden")
  makeLink("hidden","context")
  makeLink("context","hidden")
  makeLink("eventsem","hidden")
  makeLink("hidden","roles")  
  makeLink("roles","concepts")
  makeLink("concepts","output")
  makeLink("hidden","compress")
  makeLink("compress","output")
  
  # cconcepts gets target from concepts
  makeCopyTargetLink("concepts","cconcepts")
}

# sets the input to the model
setInputs <- function(inp,tar,mes, slen, perlist){
  # set number of patterns in network
  setParamAll("patn",dim(tar)[1])  
  
  layerList[[1]]$output <<- inp
  class(layerList[[1]]) <<- c("input","layer")
  
  layerList[[getLayer("context")]]$reset <<- perlist # reset context at start of sentence
  hidlay = layerList[[getLayer("hidden")]]
  hidlay$output <- matrix(0.5,hidlay$patn,hidlay$unitn) # hidden layer also needs to be reset
  layerList[[getLayer("hidden")]] <<- hidlay            # since context copies from hidden
  
  # set the event semantics
  evsem = makeEventSem(mes,inp,slen)
  #evsem2 = evsem - rowMeans(evsem)
  layerList[[getLayer("eventsem")]]$output <<-  evsem
  class(layerList[[getLayer("eventsem")]]) <<- c("input","layer")
  
  # put message codes into concept layer
  mlist = convertMessageCodes(mes)
  rc = getLayer("concepts")
  lay = layerList[[rc]]
  lay$messages <- lapply(mlist,function(m) makeMessages(m,NumRoles+1,NumConcepts,TRUE) )  
  lay$sentlen = slen
  class(lay) <- c("message","layer")
  layerList[[rc]] <<- lay
  
  # put reverse messages in croles layer
  cr = getLayer("croles")
  lay = layerList[[cr]]
  lay$messages <- lapply(mlist,function(m) makeMessages(m,NumConcepts+1,NumRoles,FALSE) )  
  lay$sentlen = slen
  class(lay) <- c("message","layer")
  layerList[[cr]] <<- lay
  class(layerList[[getLayer("cconcepts")]]) <<- c("output","layer")
  
  layerList[[getLayer("output")]]$target <<- tar
  class(layerList[[getLayer("output")]]) <<- c("softmax","output","layer")
  
  # these are used to format the look of the model
  layerList[[getLayer("croles")]]$verticalPos <<- 7
  layerList[[getLayer("cconcepts")]]$verticalPos <<- 3
  layerList[[getLayer("roles")]]$verticalPos <<- 7
  layerList[[getLayer("concepts")]]$verticalPos <<- 3
  layerList[[getLayer("context")]]$verticalPos <<- 0
  layerList[[getLayer("eventsem")]]$verticalPos <<- 10
  layerList[[getLayer("hidden")]]$verticalPos <<- 3
}

# this function trains the model for numEpochs and tests it, reporting MSDelta and test set results
trainTest <- function(numEpochs){
  history=data.frame()  # stores data during training
  for (epoch in 1:numEpochs){
    
    # use small batches of the training sentences.
    sentsample = sort(sample(1:max(sentnum),max(sentnum)/3))
    sset = which(sentnum %in% sentsample)
    setInputs(Inputs[sset,],Targets[sset,],messages[sentsample],sentlen[sentsample],periodList[sset])
    
    forwardPass() # spread activation forward for all patterns in training set
    
    backpropagateError()  # back propagate error and update weights
    
    if (epoch %% 1000==0){ # print delta plot every 1000 epochs
      print(epoch)
      saveLayerList = layerList     
      slice = evaluateModelFit(epoch)  # save model parameters for figures
      slice$test=-1
      print(slice)
      history = rbind(history,slice)
      # plot cost function
      costPlot <- ggplot(history,aes(x=time,y=Cost))+geom_line()
      print(costPlot)
      # plot MSDelta
      deltaPlot <- ggplot( history,aes(x=time,y=MSDelta, shape=Layer, colour=Layer))+geom_line()+geom_point()
      print(deltaPlot)
      
      if (epoch %% 2000==0){  # test model on novel sentences every 2000 epochs
        setInputs(tInputs,tTargets,tmessages,c(6,6),tPeriodList)
        modelout = addModel("output",restrict=TRUE,labels=tsentseq)
        slice$test = computeMeanCrossEntropyLoss()
        history = rbind(history,slice)
        dd = subset(modelout, pattern %in% 1:12)
        print(plotModel(dd,ylabel=vocabulary))
      } 
      layerList <<- saveLayerList
    }
  }
  return(history)
}

# This function sets up network and then trains it.
trainOne <- function(){
  numEpochs = 6000
  # setup network and set inputs
  createDualpath()
  setParamAll("lrate",0.1) # learning rate: speed of learning
  setParamAll("momentum",0.9) # amount of previous weight changes that are added
  setParamAll("boundedDescent",TRUE)  # bounded descent algorithm is used
  setParamAll("hysteresis",0.5)  # use half of previous context activation
  setParamAll("zeroErrorRadius",0.2)  # error < radius is set to zero
  setInputs(Inputs,Targets,messages,sentlen,periodList)  
  resetNetworkWeights()
 
  # show model activations before learning
  modelout = addModel("output",restrict=TRUE,labels=sentseq)
  dd = subset(modelout, pattern %in% 1:6)
  plotModel(dd,ylabel=vocabulary,axisfontsize=10)
  
  # train model
  history=trainTest(numEpochs)  
}

ptm <- proc.time()

history=trainOne()

print(proc.time() - ptm)



