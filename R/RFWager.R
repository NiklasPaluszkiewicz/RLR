#Function related to RFWager

#' @export
Get.Def.Par.RFWager <- function(){
  #Identifier
  name <- "Random.Forest.Wager"

  #Functions
  setup <- Setup.RFWager
  predict <- Predict.RFWager
  train <- Train.RFWager

  #Parameters
  mtry <- 3
  num.trees <- 1000

  model.def.par <- nlist(name,setup,predict,train,mtry, num.trees)

  return(model.def.par)
}

#' @export
Setup.RFWager <- function(model.par, game.par){
  #initialize with 0
  x_train <- matrix(0,ncol=game.par$input.nodes+game.par$output.nodes,nrow=(game.par$input.nodes+game.par$output.nodes)*3)
  y_train <- matrix(0,nrow=(game.par$input.nodes+game.par$output.nodes)*3)

  model <- regression_forest(x_train, y_train,mtry=model.par$mtry,num.trees=model.par$num.trees)

  return(model)
}

#' @export
Predict.RFWager <- function(model, model.par, state){
  restore.point("Predict.RFWager")
  no.action <- length(model$feature.indices) - ncol(state)
  states.l <- lapply(1:no.action,FUN=function(x){
    m <- matrix(0,nrow=nrow(state),ncol=no.action, byrow=TRUE)
    m[,x] <- 1
    res <- cbind(state,m)
    return(res)
  })
  states <- do.call(rbind,states.l)

  res <- matrix(predict(model,states)$predictions,ncol=no.action)
  return(res)
}

#' @export
Train.RFWager <- function(model, model.par, x_train, y_train){
  restore.point("Train.RFWager")
  no.action <- length(model$feature.indices) - ncol(x_train)
  x_train.RF.l <- lapply(1:no.action,FUN=function(x){
    m <- matrix(0,nrow=nrow(x_train),ncol=no.action, byrow=TRUE)
    m[,x] <- 1
    res <- cbind(x_train,m)
    return(res)
  })
  x_train.RF <- do.call(rbind,x_train.RF.l)

  y_train.RF.l <- lapply(1:no.action,FUN=function(x){
    return(as.matrix(y_train[,x]))
  })
  y_train.RF <- do.call(rbind,y_train.RF.l)

  #Remove NAs
  na.vals <- is.na(y_train.RF)
  x_train.RF <- x_train.RF[!na.vals,]
  y_train.RF <- y_train.RF[!na.vals]

  model <- regression_forest(x_train.RF, y_train.RF,mtry=model.par$mtry,num.trees=model.par$num.trees)
  fit.obj <- NA
  return(nlist(model,fit.obj))
}
