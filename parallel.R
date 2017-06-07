library(randomForest)
library(parallel)
options(mc.cores = detectCores())
train <->
  train[,1] <->
  
parRandomForest2 <- function(xx,="" ...,="" ntree="500," mc="getOption("mc.cores"," 2l),="" seed="">
{
  cl <->
    if(!is.null(seed)) clusterSetRNGStream(cl, seed)
  clusterEvalQ(cl, library(randomForest))
  rfwrap <- function(ntree,="" xx,="" ...)="" randomforest(x="xx," ntree="ntree,">
  rfpar <- parlapply(cl,="" rep(ceiling(ntree/mc),="" mc),="" rfwrap,="" xx="xx,">
  stopCluster(cl)
  do.call(combine, rfpar)
}

rf <- foreach(ntree="rep(250," 4),="" .combine="combine," .packages='randomForest' )="">
  randomForest(x, y, ntree=ntree ,