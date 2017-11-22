#Play the Prisoners Dilemma of StratTourn

#' Standard Parameters of Repeated Prisoners Dilemma
#' Returns a list with parameters.
#' @export
Get.Game.Param.PD <- function(){
  other.strategies <- c(strat2)
  names(other.strategies) <- c("tit.for.tat")
  uCC <- 1
  uCD <- -1
  uDC <- 2
  uDD <- 0
  err.D.prob <- 0
  err.C.prob <- 0
  delta <- 0.9
  T <- 100
  T.max <- 100
  intermed <- 0
  game.par <- nlist(other.strategies, uCC, uCD, uDC, uDD, err.D.prob, err.C.prob, delta, T, T.max, intermed)
  return(game.par)
}

#' Defines model parameters for 'Prisoners Dilemma'
#'
#' Public Function which might be called by algorithm functions.
#' Output is a list of the following structure:
#' \itemize{
#' \item input.nodes - Length of array as presented by state.2.array
#' \item output.nodes - Number of possible actions
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
  restore.point("Generate.Start.State.PD")
  t <- 1 # first round
  me.last.see <- "Default"
  other.last.see <- "Default"
  game.finished <- FALSE

  #Define our strategy
  strat.no <- sample(1:length(game.object$game.pars$other.strategies),1)
  other.strategy <- game.object$game.pars$other.strategies[[strat.no]]

  if(!is.null(names(game.object$game.pars$other.strategies[strat.no])) && names(game.object$game.pars$other.strategies[strat.no]) == "self"){
    self.play = TRUE
  } else {
    self.play = FALSE
  }

  # Draw number of periods from a negative binominal distribution if not defined
  if (is.null(game.object$game.pars$T)) {
    ret = sample.T(delta=game.object$game.pars$delta, sample.delta=game.object$game.pars$delta)
    T = ret$T
    if (!is.null(game.object$game.pars$T.max))
      T = pmin(T, game.object$game.pars$T.max)
  } else {
    T = game.object$game.pars$T
  }

  history.see <- data.frame(me=rep(NA,T), other=rep(NA,T))
  history.real <- data.frame(me=rep(NA,T), other=rep(NA,T))

  #Determine starting variables of other strategy
  par.other.full <- formals(other.strategy)
  par.other <- par.other.full[!(names(par.other.full) %in% c("obs", "i", "t", "..."))]

  U = rep(0,2)

  res <- nlist(round=t, me.last.see, other.last.see, game.finished, history.see, history.real, other.strategy, par.other, T=T, self.play)
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
  if(is.null(game.object$encoding.state)){
    encoding <- "main"
  } else {
    encoding <- game.object$encoding.state
  }

  if(encoding == "main"){
    #[1] Bit - See C (other)
    #[2] Bit - See D (other)
    #[3] Bit - See C (me)
    #[4] Bit - See D (me)
    #[5] Bit - See C (other) [one round before]
    #[6] Bit - See D (other) [one round before]
    #[7] Bit - See C (me) [one round before]
    #[8] Bit - See D (other) [one round before]
    #[9] Int - Round/100
    #[10] Bit - is first round
    #[11] Int - number of D of other per round
    #[12] Int - number of D of me per round

    arr <- vector("numeric",length=12)

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
    if(game.state$round>2 && game.state$history.see[game.state$round-2,2] == "C"){
      arr[5] <- TRUE
    }
    if(game.state$round>2 && game.state$history.see[game.state$round-2,2] == "D"){
      arr[6] <- TRUE
    }
    if(game.state$round>2 && game.state$history.see[game.state$round-2,1] == "C"){
      arr[7] <- TRUE
    }
    if(game.state$round>2 && game.state$history.see[game.state$round-2,1] == "D"){
      arr[8] <- TRUE
    }
    arr[6] <- game.state$round/100
    if(game.state$round == 1){
      arr[7] <- TRUE
    }
    if(game.state$round > 1){
      arr[8] <- sum(game.state$history.see[1:(game.state$round-1),2]=="D")/game.state$round
    }
    if(game.state$round > 1){
      arr[9] <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")/game.state$round
    }
    return(arr)
  } else if (encoding=="full.zero"){
    me.C <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
    me.D <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
    other.C <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
    other.D <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
    arr <- c(me.C,me.D,other.C,other.D)
    return(arr)
  } else if (encoding=="full.compact"){
    me.C <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
    me.D <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
    other.C <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
    other.D <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
    me <- me.C-me.D
    other <- other.C-other.D
    arr <- c(me,other)
  } else if (encoding=="TenTen") { #Can easily be made more efficient
    me.C <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
    me.D <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
    other.C <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
    other.D <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))

    me <- me.C - me.D
    other <- other.C - other.D

    me.start <- me[1:10]
    other.start <- other[1:10]

    round <- game.state$round/game.object$game.pars$T.max
    if(game.state$round == 1){
      first.round <- TRUE
    } else {
      first.round <- FALSE
    }
    if(game.state$round > 1){
      av.other.def <- sum(game.state$history.see[1:(game.state$round-1),2]=="D")/game.state$round
    } else {
      av.other.def <- 0
    }
    if(game.state$round > 1){
      av.me.def <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")/game.state$round
    } else {
      av.me.def <- 0
    }

    me.fin <- me[(game.state$round-1):max((game.state$round-10),1)]
    if(length(me.fin)<=10){
      me.fin <- c(me.fin,rep(0,10-length(me.fin)))
    }

    other.fin <- other[(game.state$round-1):max((game.state$round-10),1)]
    if(length(other.fin)<=10){
      other.fin <- c(other.fin,rep(0,10-length(other.fin)))
    }

    if(game.state$round>=2){
      prev.val.as.seen <- sum(sapply((1:(game.state$round-1)),FUN=function(x){
        if(game.state$history.see[x,1]=="C" && game.state$history.see[x,2]=="C"){
          round.reward <- game.object$game.pars$uCC
        } else if (game.state$history.see[x,1]=="C"&&game.state$history.see[x,2]=="D"){
          round.reward <- game.object$game.pars$uCD
        } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="C"){
          round.reward <- game.object$game.pars$uDC
        } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="D"){
          round.reward <- game.object$game.pars$uDD
        } else {
          stop("something bad happened when calculating payoff")
        }
        return(round.reward)
      }))/(game.state$round-1)
    } else {
      prev.val.as.seen <- 1
    }

    arr <- c(me.start, other.start, other.fin, me.fin, round, av.other.def, av.me.def, prev.val.as.seen)
    return(arr)
  }  else if(encoding=="maximum.full.Ten") {
    restore.point("maxmimum.full.Ten.encoding")
    me.C <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
    me.D <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
    other.C <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
    other.D <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))

    if(game.object$game.pars$T.max>=game.state$round){
      round.cum <- c(rep(1,game.state$round),rep(0,game.object$game.pars$T.max-game.state$round))
      round.single <- rep(0,game.object$game.pars$T.max)
      round.single[game.state$round] <- 1
    } else {
      round.cum <- rep(1,game.object$game.pars$T.max)
      round.single <- rep(0,game.object$game.pars$T.max)
    }



    if(game.state$round > 1){
      av.other.def <- sum(game.state$history.see[1:(game.state$round-1),2]=="D")/game.state$round
    } else {
      av.other.def <- 0
    }
    if(game.state$round > 1){
      av.me.def <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")/game.state$round
    } else {
      av.me.def <- 0
    }

    me.C.fin <- me.C[(game.state$round-1):max((game.state$round-10),1)]
    if(length(me.C.fin)<=10){
      me.C.fin <- c(me.C.fin,rep(0,10-length(me.C.fin)))
    }

    me.D.fin <- me.C[(game.state$round-1):max((game.state$round-10),1)]
    if(length(me.D.fin)<=10){
      me.D.fin <- c(me.D.fin,rep(0,10-length(me.D.fin)))
    }

    other.C.fin <- other.C[(game.state$round-1):max((game.state$round-10),1)]
    if(length(other.C.fin)<=10){
      other.C.fin <- c(other.C.fin,rep(0,10-length(other.C.fin)))
    }

    other.D.fin <- other.D[(game.state$round-1):max((game.state$round-10),1)]
    if(length(other.D.fin)<=10){
      other.D.fin <- c(other.D.fin,rep(0,10-length(other.D.fin)))
    }

    if(game.state$round>=2){
      prev.val.as.seen <- sum(sapply((1:(game.state$round-1)),FUN=function(x){
        if(game.state$history.see[x,1]=="C" && game.state$history.see[x,2]=="C"){
          round.reward <- game.object$game.pars$uCC
        } else if (game.state$history.see[x,1]=="C"&&game.state$history.see[x,2]=="D"){
          round.reward <- game.object$game.pars$uCD
        } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="C"){
          round.reward <- game.object$game.pars$uDC
        } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="D"){
          round.reward <- game.object$game.pars$uDD
        } else {
          stop("something bad happened when calculating payoff")
        }
        return(round.reward)
      }))/(game.state$round-1)
    } else {
      prev.val.as.seen <- 1
    }

    prev.val.as.seen.abs <- prev.val.as.seen*(game.state$round-1)/game.object$game.pars$T.max

    arr <- c(me.C, me.D, other.C, other.D, me.C.fin, me.D.fin, other.C.fin, other.D.fin, round.cum, round.single, av.other.def, av.me.def, prev.val.as.seen,prev.val.as.seen.abs)
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

#' Action to Array for Prisoners Dilemma
#'
#' Transforms the output of a game strategy to the number of choice of the machine learning algorithm \cr \cr
#' Internal Function - There should be no need to call this function by an algorithm.
#' @param output Chosen action.
#' @param game.object as specified by Get.Game.Object
Action.2.Choice.PD <- function(output, game.object){
  if(is.null(game.object$encoding.action)){
    encoding <- "main"
  } else {
    encoding <- game.object$encoding.action
  }

  if(encoding == "main"){
    if(output=="C"){
      return(1)
    } else {
      return(2)
    }
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
  restore.point("State.Transition.PD")
  action.me <- Choice.2.Action.PD(action,game.object)
  reward <- 0
  game.finished <- FALSE

  if(game.state$round==1){
    obs.other <- list(a=c("C","C"))
  } else {
    obs.other <- list(a=game.state$history.see[game.state$round-1,])
  }

  args = c(list(obs = obs.other,i=2,t=game.state$round),game.state$par.other)
  strat.res <- do.call(game.state$other.strategy,args)
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

  #Intermediate returns for faster convergence
  if(game.object$game.pars$intermed>0){
    rel.round <- game.state$round-1
    if(game.state$history.see[rel.round,1]=="C" && game.state$history.see[rel.round,2]=="C"){
        reward <- game.object$game.pars$uCC * game.object$game.pars$intermed
      } else if (game.state$history.see[rel.round,1]=="C" && game.state$history.see[rel.round,2]=="D"){
        reward <- game.object$game.pars$uCD * game.object$game.pars$intermed
      } else if (game.state$history.see[rel.round,1]=="D" && game.state$history.see[rel.round,2]=="C"){
        reward <- game.object$game.pars$uDC * game.object$game.pars$intermed
      } else if (game.state$history.see[rel.round,1]=="D" && game.state$history.see[rel.round,2]=="D"){
        reward <- game.object$game.pars$uDD * game.object$game.pars$intermed
      } else {
        stop("something bad happened when calculating payoff")
      }
  }

  #Last round
  if(game.state$round>game.state$T){
    if(!game.state$self.play){
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
    } else {
      reward <- sum(sapply((1:game.state$T),FUN=function(x){
        if(game.state$history.real[x,1]=="C" && game.state$history.real[x,2]=="C"){
          round.reward <- game.object$game.pars$uCC
        } else if (game.state$history.real[x,1]=="C"&&game.state$history.real[x,2]=="D"){
          round.reward <- (game.object$game.pars$uCD + game.object$game.pars$uDC)/2
        } else if (game.state$history.real[x,1]=="D"&&game.state$history.real[x,2]=="C"){
          round.reward <- (game.object$game.pars$uCD + game.object$game.pars$uDC)/2
        } else if (game.state$history.real[x,1]=="D"&&game.state$history.real[x,2]=="D"){
          round.reward <- game.object$game.pars$uDD
        } else {
          stop("something bad happened when calculating payoff")
        }
        return(round.reward)
      }))/game.state$T
    }
    game.finished <- TRUE
    game.state$game.finished <- TRUE
  }

  return(nlist(next.state=game.state,reward,game.finished))
}

#' Generate Memory where strategies play against themselves
#'
#' Each strategy within the game.object plays against itself one time
#'
#' Outputs List of lists with the following elements:
#' \itemize{
#' \item state - Already encoded game state.
#' \item action - Which of the actions has been taken?
#' \item next.state - resulting next, encoded, state
#' \item reward - What did we get from transitioning to the next state?
#' \item done - Boolean; is the game over?
#' }
#' Public Function which might be called by algorithms.
#' @param game.object as specified by Get.Game.Object
#' @param algo.par as e.g. given by Get.Def.Par.QLearning
#' @export
Memory.Self.Play.PD <- function(game.object, algo.par){
  #generate Memory
  strats <- game.object$game.pars$other.strategies

  mem <- list()
  for(strat.i in 1:length(game.object$game.pars$other.strategies)){
    #ignore my own initialisation
    if(names(game.object$game.pars$other.strategies)[strat.i] == "self"){
      next
    }
    restore.point("inside.Memory.Self.Play.PD")
    strat <- game.object$game.pars$other.strategies[strat.i]
    strat.go <- game.object
    strat.go$game.pars$other.strategies <- strat
    state <- Generate.Start.State.PD(strat.go)
    obs = c("C","C")
    strat.pars <- NULL
    for(counter in 1:state$T){
      args <- c(list(obs=obs, i=1, t=state$round), strat.pars)
      strat.ret <- do.call(strat.go$game.pars$other.strategies[[1]],args)
      strat.action <- strat.ret$a
      strat.pars <- strat.ret[-c("a" %in% names(strat.ret))]
      strat.action <- Action.2.Choice.PD(output=strat.action, game.object)

      next.state.full <- State.Transition.PD(game.state = state, action = strat.action, game.object=game.object)

      next.state <- next.state.full$next.state
      reward <- next.state.full$reward
      done <- next.state.full$game.finished

      if(algo.par$mem.selection=="all" || (algo.par$mem.selection=="end.state" && done)){
        if(algo.par$mem.type=="game.encoded"){
          mem[[length(mem)+1]] <- list(state=t(State.2.Array.PD(game.state=state, game.object=game.object)), action=strat.action, next.state=t(State.2.Array.PD(game.state=next.state, game.object=game.object)), reward=reward, done=done)
        } else if (algo.par$mem.type=="game.state"){
          mem[[length(mem)+1]] <- list(state=state, action=strat.action, next.state=next.state, reward=reward, done=done)
        }
      }

      state <- next.state
      obs <- list(a=c(next.state$me.last.see, next.state$other.last.see))

    }
  }
  return(mem)
}

#' Generate Memory where strategies play against a random strategy
#'
#' Each strategy within the game.object plays against a random strategy of the given defection probability
#'
#' Outputs List of lists with the following elements:
#' \itemize{
#' \item state - Already encoded game state, if algo.par$mem.type=="game.encoded"
#' \item action - Which of the actions has been taken?
#' \item next.state - resulting next, encoded, state
#' \item reward - What did we get from transitioning to the next state?
#' \item done - Boolean; is the game over?
#' }
#' Public Function which might be called by algorithms.
#' @param game.object as specified by Get.Game.Object
#' @param algo.par as e.g. given by Get.Def.Par.QLearning
#' @export
Memory.Random.Play.PD <- function(game.object, algo.par){
  #Generate Random Strategy
  strat <- function(obs,i,t,...) {
      a = sample( c("C","D"), size=1,  prob=c(1-algo.par$def.prob,algo.par$def.prob))
      return(list(a=a))
  }

  mem <- list()
  #ignore my own initialisation
  restore.point("inside.Memory.Rand.Play.PD")
  strat.go <- game.object
  strat.go$game.pars$other.strategies <- c(strat)
  state <- Generate.Start.State.PD(strat.go)
  obs = c("C","C")
  strat.pars <- NULL
  for(counter in 1:state$T){
    args <- c(list(obs=obs, i=1, t=state$round), strat.pars)
    strat.ret <- do.call(strat.go$game.pars$other.strategies[[1]],args)
    strat.action <- strat.ret$a
    strat.pars <- strat.ret[-c("a" %in% names(strat.ret))]
    strat.action <- Action.2.Choice.PD(output=strat.action, game.object)

    next.state.full <- State.Transition.PD(game.state = state, action = strat.action, game.object=game.object)

    next.state <- next.state.full$next.state
    reward <- next.state.full$reward
    done <- next.state.full$game.finished

    if(algo.par$mem.selection=="all" || (algo.par$mem.selection=="end.state" && done)){
      if(algo.par$mem.type=="game.encoded"){
        mem[[length(mem)+1]] <- list(state=t(State.2.Array.PD(game.state=state, game.object=game.object)), action=strat.action, next.state=t(State.2.Array.PD(game.state=next.state, game.object=game.object)), reward=reward, done=done)
       } else if (algo.par$mem.type=="game.state"){
         mem[[length(mem)+1]] <- list(state=state, action=strat.action, next.state=next.state, reward=reward, done=done)
       }
     }

    state <- next.state
    obs <- list(a=c(next.state$me.last.see, next.state$other.last.see))

  }

  return(mem)
}

#' Get Game Object which fully defines Prisoners Dilemma.
#' @param encoding.state Which method should be used to encode the game? Currently supported:
#' \itemize{
#' \item main \itemize{
#' \item [1] Bit - See C (other)
#' \item [2] Bit - See D (other)
#' \item[3] Bit - See C (me)
#' \item[4] Bit - See D (other)
#' \item [5] Bit - See C (other) [one round before]
#' \item [6] Bit - See D (other) [one round before]
#' \item[7] Bit - See C (me) [one round before]
#' \item[8] Bit - See D (other) [one round before]
#' \item[9] Int - Round/100
#' \item[10] Bit - is first round
#' \item[11] Int - number of D of other per round
#' \item[12] Int - number of D of me per round
#' }
#' }
#' @param encoding.action Which method should be used to encode the action? Currently supported:
#' \itemize{
#' \item main - [C,D]
#' }
#' @export
Get.Game.Object.PD <- function(encoding.state=NULL, encoding.action=NULL){
  name <- "Prisoners Dilemma"
  supports <- c("memory.self.play", "memory.random.play")

  game.par <- Get.Par.PD
  state.transition <- State.Transition.PD
  start.state <- Generate.Start.State.PD
  state.2.array <- State.2.Array.PD
  memory.self.play <- Memory.Self.Play.PD
  memory.random.play <- Memory.Random.Play.PD

  game.pars <- Get.Game.Param.PD()

  game.object <- nlist(name, supports, game.pars, game.par, state.transition, start.state, state.2.array, encoding.state, encoding.action, memory.self.play, memory.random.play)
  return(game.object)
}

#' The actual strategy after model has been trained
#'
#' Does not work for itself - the "strat.model" variable has to be specified beforehand.
#' @export
NN.strat.main = function(obs,i,t,history.see=NULL,...) {
  restore.point("NN.strat")
  arr <- rep(0,12)
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
  if(t>2 && history.see[t-2,2] == "C"){
    arr[5] <- TRUE
  }
  if(t>2 && history.see[t-2,2] == "D"){
    arr[6] <- TRUE
  }
  if(t>2 && history.see[t-2,1] == "C"){
    arr[7] <- TRUE
  }
  if(t>2 && history.see[t-2,1] == "D"){
    arr[8] <- TRUE
  }
  arr[9] <- t/100
  if(t == 1){
    arr[10] <- TRUE
  }
  if(t > 1){
    arr[11] <- sum(history.see[1:(t-1),2]=="D")/t
  }
  if(t > 1){
    arr[12] <- sum(history.see[1:(t-1),1]=="D")/t
  }

  act.values <- predict(model,t(arr))
  choice <- which.max(act.values)

  if(choice==1){
    a <- "C"
  } else {
    a <- "D"
  }

  return(list(a=a, history.see=history.see))
}

NN.strat.full.zero = function(obs,i,t,history.see=NULL,...) {
  restore.point("NN.strat.zero")
  j = 3-i

  if(is.null(history.see)){
    history.see <- data.frame(me=rep(0,200), other=rep(0,200))
  }
  if(t>1){
    history.see[t-1,1] <- obs$a[i]
    history.see[t-1,2] <- obs$a[j]
  }
  me.C <- rep(0,200)
  me.C[history.see[,1]=="C"] <- 1
  me.D <- rep(0,200)
  me.D[history.see[,1]=="D"] <- 1
  other.C <- rep(0,200)
  other.C[history.see[,2]=="C"] <- 1
  other.D <- rep(0,200)
  other.D[history.see[,2]=="D"] <- 1

  arr <- c(me.C,me.D,other.C,other.D)

  act.values <- predict(model,t(arr))
  choice <- which.max(act.values)

  if(choice==1){
    a <- "C"
  } else {
    a <- "D"
  }

  return(list(a=a, history.see=history.see))
}



