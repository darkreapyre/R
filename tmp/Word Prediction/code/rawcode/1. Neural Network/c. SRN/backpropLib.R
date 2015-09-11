# Backpropagation Library
# errors should be reported to chang.franklin@gmail.com
# August 2014
# Version 0.8

require(ggplot2)
require(grid)
require(stringr)

########## Network setup functions ############
makeLayer <- function(name,size){
  lay = list(name=name, num = length(layerList)+1, unitn = size, inputlayer=c(), inunitn=1, # layer shape
             weights=NULL,input=NULL,netinput=NULL,output=NULL,targcopy=NULL,    # matricies
             boundedDescent = FALSE, zeroErrorRadius = 0,                  # parameters
             lrate=0.01,momentum=0.1,bias=1.0,randmean=0,randsd=NA,         # parameters
             reset=NULL, resetVal=0.5,hysteresis=0,         # context layer parameters/matrices
             verticalPos = 0, horizontalPos = length(layerList)*2)      # visualization parameters
  class(lay) =  "layer"  
  layerList[[lay$num]] <<- lay 
}

makeContextLayer <- function(name,size,reset){
  makeLayer(name,size)
  n = getLayer(name)
  layerList[[n]]$reset <<- reset
  class(layerList[[n]]) <<-  c("context", "layer")  
}

makeLink <- function(l1,l2){
  num1 = getLayer(l1)
  num2 = getLayer(l2)
  lay1 = layerList[[num1]]
  lay2 = layerList[[num2]]
  lay2$inunitn = lay2$inunitn + lay1$unitn
  lay2$inputlayer = c(lay2$inputlayer,num1)
  layerList[[num2]] <<- lay2
}

makeCopyTargetLink <- function(l1,l2){
  num1 = getLayer(l1)
  num2 = getLayer(l2)
  layerList[[num2]]$targcopy <<- num1
}

resetLayerWeights <- function(lay){
  # Haykin (1997, pg 184) argues that you should initialize to sd = 1/numberofinputs
  if (is.na(lay$randsd) && !is.null(lay$inputlayer)){
    # count inputs
    inpcount = 0
    for (j in lay$inputlayer){
      inpcount = inpcount + layerList[[j]]$unitn 
    }
    lay$randsd <- 1/inpcount   # set randsd to reciprocal of number of inputs
  }
  if (!is.na(lay$randsd)){
    # randomize weights
    randw = runif(lay$inunitn*lay$unitn,min=-lay$randsd,max=lay$randsd) # generate uniform
    lay$weights<-matrix(randw,lay$inunitn,lay$unitn) # format as matrix
  }
  lay
}

resetNetworkWeights <- function(){
  layList = layerList
  layList = lapply(layList,function(lay){ resetLayerWeights(lay)})
  layerList <<- layList

  ##  getLayerList is used to get layers, but needs to be reset when building new network
  getLayerList <<- list()
}


################ Forward Pass Functions ######################
tanhActivationFunction <- function(f){
  return(tanh(f))   # tanh is a sigmoid with range -1,1
}

forwardPassOne = function(obj) UseMethod("forwardPassOne")

# default forward pass function
forwardPassOne.layer <- function(lay){
  # spread input activation through weights (dot/inner product %*%)
  lay$netinput <-lay$input %*% lay$weights
  # input activation at each unit passed through output function (tanh)
  lay$output <- tanhActivationFunction(lay$netinput)
  layerList[[lay$num]] <<- lay
}

# normalize output units
forwardPassOne.normout <- function(lay){
  NextMethod()
  lay = layerList[[lay$num]]
  out = (lay$output+1)
  out2 = out/rowSums(out)
  lay$output = out2*2-1
  layerList[[lay$num]] <<- lay
}

forwardPassOne.input <- function(lay){
  # input layer hold inputs in output
}

forwardPassOne.message <- function(lay){  
  tlay = lay
  lay$netinput = NULL
  lay$output = NULL
  ind = 1
  # each message is separately forward passed
  for (m in 1:length(lay$messages)){
    nind = ind + lay$sentlen[m]-1  # sentlen specifices the length
    if (nind > dim(lay$input)[1]){
      nind = dim(lay$input)[1]
    }
    tlay$input = lay$input[ind:nind,]
    tlay$weights = lay$messages[[m]]
    lay2 <- forwardPassOne.layer(tlay)
    # combine results in lay
    lay$output = rbind(lay$output,lay2$output)
    lay$netinput = rbind(lay$netinput,lay2$netinput)
    lay$weights = tlay$weights
    ind = nind+1
  }
  layerList[[lay$num]] <<- lay
}

forwardPassOne.context <- function(lay){
  reset = matrix(lay$resetVal,1,lay$unitn) # make reset matrix
  lay$netinput = rbind(reset, lay$input[1:(lay$patn - 1),1:lay$unitn]) # create t-1 hidden
  lay$netinput[lay$reset==1] = reset                            # reset at start of sequence
  lay$netinput2 = rbind(reset, lay$netinput[1:(lay$patn - 1),])        # create t-2 hidden
  lay$output = (1-lay$hysteresis)*lay$netinput + lay$hysteresis*lay$netinput2  # combine using hysteresis
  lay$output[lay$reset==1] = reset                            # reset at start of sequence
  layerList[[lay$num]] <<- lay
}

forwardPassOne.softmax <- function(lay){
  # spread input activation through weights (dot/inner product %*%)
  lay$netinput <- lay$input %*% lay$weights
  # exponentiate netinput
  expinputact = exp(lay$netinput)
  # sum by rows
  sumexp = rowSums(expinputact)
  # softmax is exponetial inputact normalized (round to four places)
  lay$output <- round(expinputact/sumexp,4)
  layerList[[lay$num]] <<- lay
}

# main forward pass function
forwardPass = function(){
  for (laynum in 2:length(layerList)){  
 #  print(paste("forward ",laynum))
    lay = layerList[[laynum]]
    ## for each input layer copy inputs into input field
    if (!is.null(lay$inputlayer)){ 
      lay$input = matrix()
      for (ilay in lay$inputlayer){
        if (is.na(lay$input[1,1])){
          lay$input = layerList[[ilay]]$output[1:lay$patn,]   # one input 
        }else{          # more than one input
          lay$input <- cbind(lay$input,layerList[[ilay]]$output[1:lay$patn,])
        }
      }
    }
    lay$input<-as.matrix(cbind(lay$input, lay$bias) ) # add bias
    layerList[[laynum]] <<- lay  # save results
    forwardPassOne(lay)
  }
}

############  Backward Pass Functions ##############

derivativeFunction = function(obj) UseMethod("derivativeFunction")

derivativeFunction.layer <- function(lay){
  return(1 - lay$output^2)  # derivative of tanh
}

derivativeFunction.softmax <- function(lay){
  return(lay$output * (1-lay$output))  # derivative of softmax
}

backPropOne = function(obj) UseMethod("backPropOne")

# this is the default backward pass function
backPropOne.layer <- function(lay){
  
  lay$deriv <- derivativeFunction(lay) # compute derivative
  
  lay$gradient <- lay$error * lay$deriv # gradient is error * derivative

  deltaWeights <- t(lay$input) %*% lay$gradient # compute weight change matrix

  # do bounded descent
  if (lay$boundedDescent){
    len = sqrt(sum(deltaWeights^2))
    if (len > 1){
      deltaWeights = deltaWeights/len
    }
  }
  lay$delta <- deltaWeights   # save deltas
  
  deltaWeights <- deltaWeights * lay$lrate # modulate with lrate
  
  # steepest descent 
  weights <- lay$weights + deltaWeights 
  # momentum descent: model continues in same direction as previous weight change
  if (!is.null(lay$prevDW)){
    weights <- weights +  lay$momentum * lay$prevDW
  }
  lay$prevDW<-deltaWeights    # save delta weights with learning rate adjustment
  
  lay$weights <- as.matrix(weights)     # save new weights

  # error is back-propagated and saved in backerror
  W = as.matrix(weights[1:(dim(weights)[1]-1),]) # remove bias weight
  lay$backerror = lay$gradient %*% t(W)   # backprop gradient
  # copy to all input layers
  ind = 1
  for (j in lay$inputlayer){
    nind = ind+layerList[[j]]$unitn
    layerList[[j]]$error <<- lay$backerror[,ind:(nind-1)] 
    ind = nind
  }
  lay
}

backPropOne.input <- function(lay){
## input layers do not learn 
  lay
}

backPropOne.context <- function(lay){
  ## context layers do not learn 
  lay
}

backPropOne.message <- function(lay){  
  ## backprop for each message separately and then pasted back together
  tlay = lay
  lay$backerror = NULL
  ind = 1
  for (m in 1:length(lay$messages)){
    nind = ind + lay$sentlen[m]-1
    tlay$input = lay$input[ind:nind,]
    tlay$weights = lay$messages[[m]]
    tlay$error= lay$error[ind:nind,]
    tlay$output= lay$output[ind:nind,]
    lay2 <- backPropOne.layer(tlay)
    lay$backerror = rbind(lay$backerror,lay2$backerror)
    ind = nind + 1 
    #   print(lay$backerror)
  }
  ### copy error to all input layers
  ind = 1
  for (j in lay$inputlayer){
    nind = ind+layerList[[j]]$unitn
    layerList[[j]]$error <<- lay$backerror[,ind:(nind-1)] 
    ind = nind
  }
  lay
}

# error function for different layers
errorFunc = function(obj) UseMethod("errorFunc")

errorFunc.layer <- function(lay){
  # only output layers have error function, other layers get error from back-prop
}

errorFunc.output <- function(lay){
  if (!is.null(lay$targcopy)){
    # some layers get targets from other layers, so copy the targets in those cases
    reset = matrix(0,1,lay$unitn) # make reset matrix
    out = layerList[[lay$targcopy]]$output
    lay$target = rbind(reset, out[1:(lay$patn - 1),1:lay$unitn]) # create t-1 hidden
#    lay$target = layerList[[lay$targcopy]]$output
  }
  # compute error
  lay$error =  lay$target-lay$output
  # zero error radius sets any error that is close enough to the target to be 0
  # this helps to reduce extreme weights and keeps values within sensitive region of activation function
#  lay$error[abs(lay$error) < lay$zeroErrorRadius] = 0
  layerList[[lay$num]] <<- lay
}



# main backward pass function
backpropagateError <- function(){
  for (laynum in length(layerList):2){
 #    print(paste("backward ",laynum))
    errorFunc(layerList[[laynum]])  
    
    # apply backprop in layer specific manner
    lay <- backPropOne(layerList[[laynum]])
    
    #    bres[[i]] <<- bresults
    layerList[[laynum]]<<-lay
  }
}

computeLoss = function(obj) UseMethod("computeLoss")

computeLoss.softmax <- function(lay){  
#  print("loss soft")
  # only computes -log for output values that are associated with positive targets
  loss = -log(rowSums(lay$target*lay$output))
  # if output is 0, then -log is Infinite, so change the value to this large value
  loss[is.infinite(loss)] = -log(0.00001)
  loss
}

computeLoss.output <- function(lay){  
  out = lay$output/2+0.5  # convert [-1,1] to [0,1]
  tar = lay$target/2+0.5  # convert [-1,1] to [0,1]
  loss = -tar*log(out) - (1-tar)*log(1-out) 
  # if output is 0, then -log is Infinite, so change the value to this large value
  loss[is.infinite(loss)] = -log(0.00001)
  loss
}

# compute cross entropy loss
computeMeanCrossEntropyLoss <- function(){
  # use for binary and multinomial classification
  lay = layerList[[getLayer("output")]]
  lay$loss = computeLoss(lay)
  layerList[[getLayer("output")]] <<- lay
  
  meanloss = mean(lay$loss)
  meanloss
}


################### Utility Functions #############################

# converts range to fit between minv and maxv
convertRange <- function(m,minv,maxv){
  m <-(maxv-minv)*(m-min(m))/(max(m)-min(m))+minv  
  return(m)
}

getLayerList <- list()
# returns the number of the layer with a matching name
getLayer = function(name){
  n = getLayerList[[name]]
  if (is.null(n)){
  n = -1
  for (i in 1:length(layerList)){
    if (layerList[[i]]$name == name){
      n = i;
    }
  }
    getLayerList[[name]] <<- n
  }
  n
}

# get a value from all layers
getVal = function(name){
  lapply(layerList,function(x) x[[name]])
}

# get a value for a pattern from all layers
getValPat = function(name, pat){
  lapply(layerList,function(x) x[[name]][pat,])
}

# set a value in all layers
setParamAll = function(name,val){
  for (i in 1:length(layerList)){
    lay = layerList[[i]]
    lay[name] <- val
    layerList[[i]] <<- lay
  }
}

################ Function for Evaluation and Graphing ###################

addState = function(obj,pat) UseMethod("addState")

# add one layer to data frame
addState.layer = function(lay,dim){
  # print(lay$name)
  my2 = unlist( lapply(lay,function(x) if (length(x) == 1){ x } ) )
  m3 = as.data.frame(my2)
  m4 = as.data.frame(t(m3))
  m5 = lay[[dim]]
  patn = dim(m5)[1]
  unitn = dim(m5)[2]
  act = c(t(m5))
  m6 = m4[rep(1, times=length(act)),]
  rownames(m6) <- 1:length(act)
  m6$pattern = rep(1:patn, each=unitn)
  m6$unitnum = rep(1:unitn, times=patn)
  m6$val = act
  return(m6)
  #  d1 = data.frame(pattern=pat, x=1:length(lay$output[pat,]), laynum=lay$num*2,layer=lay$name, m = c(lay$output[pat,]),stringsAsFactors=FALSE)
  #  return(d1)
}

# saves model state in data frame for graphing
addModel = function(dim,restrict=FALSE,labels=NULL){
  forwardPass()
  forwardPass()
  parts = lapply(layerList,function(x) addState(x,dim))
  partnames = lapply(parts,function(x) names(x))
  taract = layerList[[length(layerList)]]
  df = addState(taract,"target")  
  na = names(df)
  for (i in 1:length(parts)){
    if (length(partnames[[i]]) < length(na)){
      na = partnames[[i]][partnames[[i]] %in% na]
    }
  }
  mdf = data.frame()
  for (i in 1:length(parts)){
    mdf = rbind(mdf,parts[[i]][na])
  }
  mdf$type = dim
  df = df[na]
  df$type = "target"
  mdf = rbind(mdf,df)
  
  mdf$verticalPos = as.numeric(as.character(mdf$verticalPos))
  mdf$horizontalPos = as.numeric(as.character(mdf$horizontalPos))
  mdf$unitnum = as.numeric(as.character(mdf$unitnum))         
  
  mdf$laynum = mdf$horizontalPos
  #2*as.numeric(mdf$num)-1
  mdf$unitpos = mdf$unitnum+mdf$verticalPos
  lnum = unique(mdf$laynum)
  if (restrict){
    matchlay = lnum[c(length(lnum))]
    mdf$val[mdf$laynum %in% matchlay] = 2*(mdf$val[mdf$laynum %in% matchlay]-0.5)
  }
  mdf$laynum[mdf$type == "target"] = mdf$laynum[mdf$type == "target"] + 1
  mdf$name = as.character( mdf$name)
  mdf$name[mdf$type == "target"] = "target"
  if (!is.null(labels)){
    lab = unique(mdf$pattern)
    lab = lab - min(lab) + 1
    mdf$patternLabel = factor(mdf$pattern,labels=paste(lab,labels))
  }  
  return(mdf)
}

addWeights = function(pat){
  mdf = data.frame()
  for (lay in 1:length(weightList)){
    w = weightList[[lay]]
    for(i in 1:dim(w)[1]){
      mdf = addState(pat,paste("L",lay,"-u",i),w[i,],mdf)
    }
  }
  return(mdf)
}

# create horizontal errors showing connectivity
createLayerArrows <- function(dd){
  lnum = unique(as.integer(as.character(dd$laynum)))
  lname = unique(dd$name)
  lsize = unlist(getVal("unitn"))
  lvpos = unlist(getVal("verticalPos"))
  inputlay = getVal("inputlayer")
  arr.df = data.frame()
  mu = mean(dd$unitpos)
  for (i in 1:length(inputlay)){
    ilist = inputlay[[i]]
    outlay = layerList[[i]]
    inc = 0
    for (j in ilist){
      inlay = layerList[[j]]
      yy = 0.5+lvpos[j]
      yylen = yy + lsize[j]
      yyend = inc+0.5+lvpos[i]
      yyendlen = yyend+lsize[i]
      
      bottom = yy
      if (yy < yyend){
        bottom = yyend
      }
      top = yylen
      if (yylen > yyendlen){
        top = yyendlen
      }
      byy = (top+bottom)/2
#      byy = (yyend+yyendlen+yy+ylen)/4
#       if (yy > yyend && yy < yyendlen ){
#         byy = (yy+yyendlen)/2
#       }
#       if (yyend > yyend && yylen < yyendlen ){
#         byy = (yylen+yyend)/2
#       }
      
      jj =which(lname==inlay$name)
      ii = which(lname==outlay$name)
      
      df = data.frame(type="A",x=lnum[jj]+0.5, xend=lnum[ii]-0.5, y = byy, yend = byy)
      if (lnum[j] > lnum[i]){
        df$y = lvpos[j]+1
        df$yend = lvpos[i]+1
        df$type = "B"
        df$x = df$x-1
        df$xend = df$xend+1
      }
      #   inc=inc-0.5
      arr.df=rbind(arr.df,df)
    }
  }
  arr.df$arrshape = 0.2
  return(arr.df)
}

# creates arrows showing weights
createWeightArrows <- function(dd){
  lnum = unique(dd$laynum)
  lname = unique(dd$name)
  lsize = unlist(getVal("unitn"))
  lvpos = unlist(getVal("verticalPos"))
  layname = unlist(getVal("name"))
  inputlay = getVal("inputlayer")
  arr.df = data.frame()
  mu = mean(dd$unitpos)
  for (i in 1:length(inputlay)){
    ilist = inputlay[[i]]
    outlay = layerList[[i]]
    inc = 0
    for (j in ilist){
      inlay = layerList[[j]]
      for (k in 1:outlay$unitn){
        for (m in 1:inlay$unitn){
          w = round(outlay$weight[m,k],2)
          jj =which(lname==inlay$name)
          ii = which(lname==outlay$name)
          df = data.frame(type="A",weight = w, x=lnum[jj]+0.5, xend=lnum[ii]-0.5, y = m+lvpos[j], yend=k+lvpos[i])
          arr.df=rbind(arr.df,df)
        }
      }
      
    }
  }
  
  arr.df$arrshape = 0.2
  return(arr.df)
}

# plot model as a network
plotModel <- function(dd,xlabel=NULL,ylabel=NULL,arr=NULL,col=2,weightfontsize = 3,axisfontsize=12){
  if (sum(names(dd)=="patternLabel") == 0){
    dd$patternLabel = factor(dd$pattern)
  }
  p =  ggplot(data=dd,aes(x=laynum,y=unitpos,fill=val,width=1,height=1))
  if (is.null(arr)){
    arr.df = createLayerArrows(dd)
    p = p + geom_segment(data=arr.df,aes(x=x,y=y, xend = xend, yend = yend,fill=1,
                                         linetype=type), arrow = arrow(length = unit(arr.df$arrshape,"cm")))  
  }else{
    p = p + geom_segment(data=arr,colour="grey50",aes(x=x,y=y, xend = xend, yend = yend,
                                      fill=1,linetype=type,arrow = arrow(length = unit(arr.df$arrshape,"cm"))))      
    p = p + geom_text(data=arr,aes(x=(x+x+xend)/3,y=0.15+(y+y+yend)/3,label = weight,fill=1),size=weightfontsize)
  }
  p = p + geom_tile(colour=1)
  if (nlevels(dd$patternLabel) > 1){
   p = p + facet_wrap(~ patternLabel, ncol=col)
  }
  p = p + theme_bw()
  p = p  + scale_fill_gradient(low="grey90", high="blue",limits=c(-1,1)) 
  p = p + theme(axis.title.y = element_blank())
  p = p + theme(axis.title.x = element_blank())
  p = p + theme(axis.line=element_blank()) 
  p = p + theme(axis.ticks=element_blank()) 
  p = p + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
  p = p + scale_linetype_discrete(guide=FALSE)
  if (!is.null(ylabel)){
    p = p + scale_y_discrete(limits=ylabel)
    p = p + theme(axis.text.y  = element_text(size=axisfontsize))
  }else{
    p = p + theme(axis.text.y =element_blank())
  }
  p = p + scale_x_continuous(breaks=unique(dd$laynum),labels=unique(dd$name))
  p = p + theme(axis.text.x  = element_text(angle=65, hjust=1,size=axisfontsize))
  p = p + guides(linetype=NULL,size=NULL,colour=NULL)
  p
}


### do forward pass with particular weights
errWts = function(x,y,d1,d2,lay){  
  layerList[[lay]]$weights[d1,1] <<- x
  layerList[[lay]]$weights[d2,1] <<- y
  forwardPass()
  loss = computeMeanCrossEntropyLoss()
  return(loss)
}

# creates weight space heatmap with model's path 
mapOutWeightSpace <- function(dim1,dim2,layname,mdf){
  # names of dimensions that we are simulating
#  dim1 = names(mdf[4+d1])
#  dim2 = names(mdf[4+d2])
  weightnames = c("Hidden1","Hidden2","Bias")
  d1 = which(weightnames == dim1)
  d2 = which(weightnames == dim2)
  laynum = getLayer(layname)
  # save weights because we are replacing them with simulated weights
  saveweightList = layerList[[laynum]]
  # simulate weights and collect error
  w = seq(-3,3,by=0.05)
  w.df = data.frame(x=rep(w, length(w)),y=rep(w,each= length(w)))
  w.df$Loss = mapply(errWts,w.df$x,w.df$y,d1,d2,laynum)
  # save weights back to layer
  layerList[[laynum]] <<-saveweightList
  
  # draw weight space with color for cost and contour lines
  names(w.df)<-c(dim1,dim2,"Cost")
  p = ggplot(w.df,aes_string(x=dim1,y=dim2,z="Cost"))
  p = p + geom_tile(aes(fill=Cost))
  p = p + stat_contour()
  p = p + scale_fill_gradient(low="yellow",high="red")
  p = p +theme_bw() +theme(legend.position="top")
  
  mdf = subset(mdf, lay == laynum)
  ## this code draws errors showing the path of model during training 
  maxtime = max(mdf$time)
  mdf2 = mdf[seq(1,maxtime,by=maxtime/10),] # pick point 10 points
  mdf2$lHidden1 = c(mdf2$Hidden1[1],mdf2$Hidden1[1:(length(mdf2$Hidden1)-1)])
  mdf2$lHidden2 = c(mdf2$Hidden2[1],mdf2$Hidden2[1:(length(mdf2$Hidden2)-1)])
  mdf2$lBias = c(mdf2$Bias[1],mdf2$Bias[1:(length(mdf2$Bias)-1)])
  mdf2 = mdf2[2:length(mdf2$Hidden1),]   # remove first point
  pp = p + geom_segment(data=mdf2,aes_string(xend=dim1,yend=dim2,x=paste("l",dim1,sep=""),y=paste("l",dim2,sep="")),arrow = arrow(length = unit(0.1,"cm")))
  pp = pp + guides(colour=FALSE,size=FALSE)
  return(pp)
}

# computes cross-entropy loss and creates data frame for evaluating models
evaluateModelFit <- function(epoch){
  cost = computeMeanCrossEntropyLoss()
  slice = data.frame()
  ssDW = lapply(layerList,function(lay2){ 
    if (!is.null(lay2$input) && !is.null(lay2$delta)) { 
      data.frame(time=epoch,Cost=cost,lay=lay2$num,Layer=lay2$name,MSDelta=mean(lay2$delta^2) ) 
    } 
  })  
  slice = do.call("rbind", ssDW)  
  return(slice)
}

# Multiple plot function
# copied from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

