#Play the Prisoners Dilemma of StratTourn

#' Defines model parameters for 'Simple Game'
#'
#' Public Function which might be called by algorithm functions.
#' Output is a list of the following structure:
#' \itemize{
#' \item input.nodes - Length of array as presented by state.2.array
#' }
#' @param game.object A game object as defined by Get.Game.Object.Simple.Game
#' @export
Get.Par.PD <- function(game.object){
  restore.point("Get.Par.PD")
  input.nodes <- length(State.2.Array.PD(Generate.Start.State.PD(game.object),game.object=game.object))
  output.nodes <- Action.Encoding.Info.PD(game.object$encoding.action)$output.nodes
  game.param <- list(input.nodes=input.nodes, output.nodes=output.nodes)
  return(game.param)
}

#' Generates Start State for Prisoners Dilemma Game
#'
#' A state consisting of a human-readable start state. Here all information which are actually state dependend should be saved. All information which does not change from game to game of the same type of game should be saved in game.object.\cr \cr
#' Note that states have to be structurally identical to each other, even if not all information is needed at the beginning.\cr \cr
#' Public function, which might be called by an algorithm.
#' @export
Generate.Start.State.PD <- function(game.object){
  t <- 1 # first round
  me.last.see <- "Default"
  other.last.see <- "Default"
  game.finished <- FALSE

  # Draw number of periods from a negative binominal distribution if not defined
  if (is.null(game.object$game.pars$T)) {
    ret = sample.T(delta=game.object$game.pars$delta, sample.delta=game.object$game.pars$delta)
    T = ret$T
    if (!is.null(game.object$game.pars$T.max))
      T = pmin(T, game.object$game.pars$T.max)
  }

  history.see <- data.frame(me=rep(NA,T), other=rep(NA,T))
  history.real <- data.frame(me=rep(NA,T), other=rep(NA,T))

  #Determine starting variables of other strategy
  par.other.full <- formals(game.object$game.pars$other.strategy)
  par.other <- par.other.full[!(names(par.other.full) %in% c("obs", "i", "t", "..."))]

  U = rep(0,2)

  res <- nlist(round=t, me.last.see, other.last.see, game.finished, history.see, history.real, par.other, T=T)
  return(res)
}

#' State to Array for Prisoners Dilemma
#'
#' Transforms Game State to readable array. Here a lot of optimization might take place, as different algorithms might like different encodings.
#' @param game.state Game State in human readable form.
#' @param game.object as specified by Get.Game.Object
#' Public Function which might be called by algorithms.
#' @export
State.2.Array.PD <- function(game.state,game.object){
  restore.point("State.2.Array.PD")
  if(is.null(game.object$encoding)){
    encoding <- "main"
  } else {
    encoding <- game.object$encoding.state
  }

  if(encoding == "main"){
    #[1] Bit - See C (other)
    #[2] Bit - See D (other)
    #[3] Bit - See C (me)
    #[4] Bit - See D (other)
    #[5] Int - Round/100
    #[6] Bit - is first round
    #[7] Int - number of D of other per round
    #[8] Int - number of D of me per round

    arr <- vector("numeric",length=8)

    if(game.state$other.last.see == "C"){
      arr[1] <- TRUE
    }
    if(game.state$other.last.see == "D"){
      arr[2] <- TRUE
    }
    if(game.state$me.last.see == "C"){
      arr[3] <- TRUE
    }
    if(game.state$me.last.see == "D"){
      arr[4] <- TRUE
    }
    arr[5] <- game.state$round/100
    if(game.state$round == 1){
      arr[6] <- TRUE
    }
    if(game.state$round > 1){
      arr[7] <- sum(game.state$history.see[1:(game.state$round-1),2]=="D")/game.state$round
    }
    if(game.state$round > 1){
      arr[8] <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")/game.state$round
    }
    return(arr)
  } else {
    stop("Wrong encoding specified.")
  }
}

#' Array to Action for Prisoners Dilemma
#'
#' Transforms the output of an machine learning algorithm to a readable game state \cr \cr
#' Internal Function - There should be no need to call this function by an algorithm.
#' @param output.choice Number of chosen action.
#' @param game.object as specified by Get.Game.Object
Choice.2.Action.PD <- function(output.choice, game.object){
  if(is.null(game.object$encoding.action)){
    encoding <- "main"
  } else {
    encoding <- game.object$encoding.action
  }

  if(encoding == "main"){
    if(output.choice==1){
      chosen <- "C"
    } else {
      chosen <- "D"
    }
    res <- list(chosen=chosen)
    return(res)
  } else {
    stop("Wrong encoding specified.")
  }
}

#' Get Info of Action Encoding
#'
#' Internal Function delivering info of action encoding, as e.g. number of nodes.
Action.Encoding.Info.PD <- function(game.object){
  if(is.null(game.object$encoding.action)){
    encoding <- "main"
  } else {
    encoding <- game.object$encoding.action
  }

  if(encoding=="main"){
    res <- list(output.nodes=2)
    return(res)
  } else {
    stop("Wrong encoding specified.")
  }
}

#' Get next State of Prisoners Dilemma Game
#'
#' Outputs List with three elements:
#' \itemize{
#' \item next.state - next game state (in Complete Form)
#' \item reward - What did we get from transitioning to the next state?
#' \item game.finished - Boolean; is the game over?
#' }
#' Public Function which might be called by algorithms.
#' @param game.state A complete game state, non encoded
#' @param action The choice of action.
#' @param game.object as specified by Get.Game.Object
#' @export
State.Transition.PD <- function(game.state, action, game.object){
  action.me <- Choice.2.Action.PD(action,game.object)
  reward <- 0
  game.finished <- FALSE

  if(game.state$round==1){
    obs.other <- list(a=c("C","C"))
  } else {
    obs.other <- list(a=game.state$history.see[game.state$round-1,])
  }

  args = c(list(obs = obs.other,i=2,t=game.state$round),game.state$par.other)
  strat.res <- do.call(game.object$game.pars$other.strategy,args)
  action.other <- strat.res$a
  par.other <- strat.res[-c(1)]

  a <- c(action.me, action.other)

  rand = runif(1)
  if(a[1]=="D" && rand<game.object$game.pars$err.D.prob){
    mine.seen <- "C"
  } else if (a[1]=="C" && rand<game.object$game.pars$err.C.prob){
    mine.seen <- "D"
  } else { #correct
    mine.seen <- a[1]
  }

  rand = runif(1)
  if(a[2]=="D" && rand<game.object$game.pars$err.D.prob){
    other.seen <- "C"
  } else if (a[2]=="C" && rand<game.object$game.pars$err.C.prob){
    other.seen <- "D"
  } else { #correct
    other.seen <- a[2]
  }

  #Update state
  game.state$round <- game.state$round+1
  game.state$me.last.see <- mine.seen
  game.state$other.last.see <- other.seen
  game.state$history.see[game.state$round-1,] <- c(mine.seen, other.seen)
  game.state$history.real[game.state$round-1,] <- c(a[1], a[2])
  game.state$par.other <- par.other

  #Last round
  if(game.state$round>game.state$T){
    reward <- sum(sapply((1:game.state$T),FUN=function(x){
      if(game.state$history.real[x,1]=="C" && game.state$history.real[x,2]=="C"){
        round.reward <- game.object$game.pars$uCC
      } else if (game.state$history.real[x,1]=="C"&&game.state$history.real[x,2]=="D"){
        round.reward <- game.object$game.pars$uCD
      } else if (game.state$history.real[x,1]=="D"&&game.state$history.real[x,2]=="C"){
        round.reward <- game.object$game.pars$uDC
      } else if (game.state$history.real[x,1]=="D"&&game.state$history.real[x,2]=="D"){
        round.reward <- game.object$game.pars$uDD
      } else {
        stop("something bad happened when calculating payoff")
      }
      return(round.reward)
    }))/game.state$T
    game.finished <- TRUE
    game.state$game.finished <- TRUE
  }

  return(nlist(next.state=game.state,reward,game.finished))
}

Calculate.Game.Param.PD <- function(game.object){
  g.par <- game.object$game.given

  # Draw number of periods from a negative binominal distribution
  if (is.null(g.par$game.par$T)) {
    ret = sample.T(delta=g.par$game.par$delta, sample.delta=g.par$game.par$delta)
    T = ret$T
    if (!is.null(g.par$game.par$T.max))
      T = pmin(T, g.par$game.par$T.max)
  }

  #Here we deliberatedly choose that our Strategy is always in Slot 1!
  si = get.strat.info(i=1, strat=g.par$game.par$other.strategy, game=g.par$game, game.states=game.states, human=FALSE)

  strat.states = list()

  return(nlist(T, game.states, obs, si, strat.states))
}

#' Get Game Object which fully defines Prisoners Dilemma.
#' @param encoding.state Which method should be used to encode the game? Currently supported:
#' \itemize{
#' \item main \itemize{
#' \item [1] Bit - See C (other)
#' \item [2] Bit - See D (other)
#' \item[3] Bit - See C (me)
#' \item[4] Bit - See D (other)
#' \item[5] Int - Round/100
#' \item[6] Bit - is first round
#' \item[7] Int - number of D of other per round
#' \item[8] Int - number of D of me per round
#' }
#' }
#' @param encoding.action Which method should be used to encode the action? Currently supported:
#' \itemize{
#' \item main - [C,D]
#' }
#' @export
Get.Game.Object.PD <- function(encoding.state=NULL, encoding.action=NULL){
  game.pars <- Get.Game.Param.PD()

  game.par <- Get.Par.PD
  state.transition <- State.Transition.PD
  start.state <- Generate.Start.State.PD
  state.2.array <- State.2.Array.PD

  game.object <- nlist(game.pars, game.par, state.transition, start.state, state.2.array, encoding.state, encoding.action)
  return(game.object)
}

Get.Game.Param.PD <- function(){
  other.strategy <- tit.for.tat
  uCC <- 1
  uCD <- -1
  uDC <- 2
  uDD <- 0
  err.D.prob <- 0
  err.C.prob <- 0
  delta <- 0.9
  T <- NULL
  T.max <- 20
  game.par <- nlist(other.strategy, uCC, uCD, uDC, uDD, err.D.prob, err.C.prob, delta, T, T.max)
  return(game.par)
}

#' The actual strategy after model has been trained
#'
#' Does not work for itself - the "model" variable has to be specified beforehand.
#' @export
NN.strat = function(obs,i,t,history.see=NULL,...) {
  restore.point("NN.strat")
  arr <- vector("numeric",length=8)
  j = 3-i

  if(is.null(history.see)){
    history.see <- data.frame(me=rep(NA,200), other=rep(NA,200))
  }
  if(t>1){
    history.see[t-1,1] <- obs$a[i]
    history.see[t-1,2] <- obs$a[j]
  }

  if(obs$a[j] == "C" && t!=1){
    arr[1] <- TRUE
  }
  if(obs$a[j] == "D" && t!=1){
    arr[2] <- TRUE
  }
  if(obs$a[i] == "C" && t!=1){
    arr[3] <- TRUE
  }
  if(obs$a[i] == "D" && t!=1){
    arr[4] <- TRUE
  }
  arr[5] <- t/100
  if(t == 1){
    arr[6] <- TRUE
  }
  if(t > 1){
    arr[7] <- sum(history.see[1:(t-1),2]=="D")/t
  }
  if(t > 1){
    arr[8] <- sum(history.see[1:(t-1),1]=="D")/t
  }

  act.values <- predict(strat.model,t(arr))
  choice <- which.max(act.values)

  if(choice==1){
    a <- "C"
  } else {
    a <- "D"
  }

  return(list(a=a, history.see=history.see))
}



