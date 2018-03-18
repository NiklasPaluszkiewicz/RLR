#'Example srategy for the Hotelling game
#'
#'@export
traveling.salesman = function(obs,i,t,price=0.5,speed=0.05,...) {
  
  # Extract variables from obs 
  j = 3-i #Whats the number of the other player?
  obs.pj = obs$a[[j]]$p #What was the price of the other firm last round?
  obs.pi = obs$a[[i]]$p #What was the price of my firm last round?
  obs.lj = obs$a[[j]]$l #What was the location of the other firm last round?
  obs.li = obs$a[[i]]$l #What was the location of my firm last round?
  
  #start in the first round
  if(t==1){
    action = list(p=price,l=0)
    return(list(a=action, price=price, speed=speed))
  }
  #after the first round
  location = obs.li + speed
  if(location>1){ #we don't want to travel into the wasteland
    location = location - 1
  }
  action=list(p=price,l=location)
  return(list(a=action, price=price, speed=speed))
  
}

#'Example srategy for the Hotelling game
#'
#'@export
fix.price.loc = function(obs,i,t,...) {
  action = list(p=1,l=0.5)
  return(list(a=action))
}


#'@export
Get.Game.Param.Hotelling <- function(){
  
  other.strategies <- c(traveling.salesman)
  names(other.strategies) <- c("traveling.salesman")
  
  action.space <- "continous"
  
  lower.bound <- 0
  upper.bound <- 1
  s <- 1
  t.dist <- 1
  
  delta <- 0.985
  T <- NULL
  T.max <- 400
  intermed <- 0
  direct.rewards <- TRUE # Currently only direct rewards are supported.

  game.par <- nlist(other.strategies,action.space,lower.bound,upper.bound,s,t.dist,delta,T,T.max,intermed,direct.rewards)
  return(game.par)
  
}

#'@export
Get.Par.Hotelling <- function(game.object){
  
  input.nodes <- length(State.2.Array.Hotelling(game.object,Generate.Start.State.Hotelling(game.object)))
  output.nodes <- Action.Encoding.Info.Hotelling(game.object)$output.nodes
  game.param <- list(input.nodes=input.nodes, output.nodes=output.nodes)
  return(game.param)
  
  }

#'@export
Generate.Start.State.Hotelling <- function(game.object){
  
  t <- 1
  game.finished <- FALSE
  
  strat.no <- sample(1:length(game.object$game.pars$other.strategies),1)
  other.strategy <- game.object$game.pars$other.strategies[[strat.no]]
  
  if (is.null(game.object$game.pars$T)) {
    ret <-sample.T(delta=game.object$game.pars$delta, sample.delta=game.object$game.pars$delta)
    T <- ret$T
    if (!is.null(game.object$game.pars$T.max))
      T <- pmin(T, game.object$game.pars$T.max)
  } else {
    T <- game.object$game.pars$T
  }
  
  history.price <- data.frame(me = rep(NA,T), other = rep(NA,T))
  history.loc <- data.frame(me = rep(NA,T), other = rep(NA,T))
  
  par.other.full <- formals(other.strategy)
  par.other <- par.other.full[!(names(par.other.full) %in% c("obs", "i", "t", "..."))]
  
  res <- nlist(round=t,  game.finished, history.price, history.loc, other.strategy, par.other, T=T)
  
  return(res)
}

#'@export
State.Transition.Hotelling <- function(game.state, action, game.object){
  
     reward <- 0
     game.finished <- FALSE
     out.of.bounds <- FALSE
     
     s <- game.object$game.pars$s
     t <- game.object$game.pars$t.dist
     lower.bound <- game.object$game.pars$lower.bound
     upper.bound <- game.object$game.pars$upper.bound
  
    action.me = list(p = action[1], l = action[2])
    
    if(action.me$p<0||action.me$p>s||action.me$l<lower.bound||action.me$l>upper.bound){
      out.of.bounds <- TRUE
      action.me$p = max(0,min(action.me$p,game.object$game.pars$s))
      action.me$l = max(game.object$game.pars$lower.bound, min(action.me$l,game.object$game.pars$upper.bound))
    }
    
    if(game.state$round==1){
     obs.other<- list(a = list(list(p = NA, l = NA),
                            list(p=NA, l =NA))) # no initital game state
    }else {
     obs.other <- list(a= list(list(p = game.state$history.price[game.state$round-1,1], l = game.state$history.loc[game.state$round-1,1]),
                               list(p = game.state$history.price[game.state$round-1,2], l=  game.state$history.loc[game.state$round-1,2])))
    }
    
    args <- c(list(obs = obs.other,i=2,t=game.state$round),game.state$par.other)
    strat.res <- do.call(game.state$other.strategy, args)
    action.other <- strat.res$a
    par.other <- strat.res[-1]
    
    #Update state
    game.state$round = game.state$round + 1
    game.state$history.loc[game.state$round-1,] <- c(action.me$l, action.other$l)
    game.state$history.price[game.state$round-1,] <- c(action.me$p, action.other$p) 
    game.state$par.other <- par.other
    
    if(game.object$game.pars$direct.rewards){
      
      if(action.me$l<=action.other$l){
        l1 <- action.me$l
        p1 <- action.me$p
        l2 <- action.other$l
        p2 <- action.other$p
        is.lower <- 1
      } else {
        l1 <- action.other$l
        p1 <- action.other$p
        l2 <- action.me$l
        p2 <- action.me$p
        is.lower <- 2
      }
      
      x.lower1 <- l1 + (p1-s)/t
      x.lower2 <- l2 + (p2-s)/t
      x.upper1 <- l1 + (s-p1)/t
      x.upper2 <- l2 + (s-p2)/t
      x.indifferent <- (p2-p1)/(2*t) + 1/2 * (l1+l2)
      
      ## x.lower may never be left of lower.bound but should not be higher as upper.bound
      x.lower1 <- min(max(x.lower1,lower.bound), upper.bound)
      x.lower2 <- min(max(x.lower2,lower.bound), upper.bound)
      x.upper1 <- max(min(x.upper1, upper.bound), lower.bound)
      x.upper2 <- max(min(x.upper2, upper.bound), lower.bound)
      x.indifferent <- max(min(x.indifferent, upper.bound), lower.bound)
      
      #calculate part of interval which is relevant for profit
      if(l1<= x.indifferent && x.indifferent<=l2 && l1!=l2){ 
        #standard case -> x.indifferent lies between l1 & l2
        area1 <- min(x.indifferent,x.upper1)-x.lower1
        area2 <- x.upper2 - max(x.indifferent,x.lower2)
      } else if (l1 == x.indifferent && l2 == x.indifferent){
        #both are exactly identical (this may only happen if prices are equal too)
        area1 <- 1/2 * (x.upper1 - x.lower1)
        area2 <- area1
      } else if (x.indifferent < l1){ 
        #left case -> all customers go to l2 [if a company can't hold customers, which are exactly on them, then they do not get any]
        area1 <- 0
        area2 <- x.upper2 - x.lower2
      } else if (x.indifferent > l2){ 
        #right case -> all customers go to l1 [if a company can't hold customers, which are exactly on them, then they do not get any]
        area1 <- x.upper1 - x.lower1
        area2 <- 0
      } else {
        #this should never happen
        stop("bad case differentiation in hotelling.profits when calculating areas.\n")
      }
    
    pi1 <- area1 * p1
    pi2 <- area2 * p2
    
    #results with updated choices
    if(is.lower==1){
      profit.me <- pi1
      profit.other <- pi2
    } else {
      profit.me <- pi2
      profit.other <- pi1
    }
   } 
    
    if(out.of.bounds) profit.me <- 0
    
    if(game.state$round>game.state$T) game.finished <- TRUE
    game.state$game.finished <- game.finished
    
    return(nlist(next.state = game.state, reward = profit.me, game.finished))
  
}

#'@export
Action.Encoding.Info.Hotelling <- function(game.object){
  
  if(is.null(game.object$encoding.action)){
    encoding <- "main"
  } else {
    encoding <- game.object$encoding.action
  }
  
  if(encoding=="main"){
    res <- list(output.nodes=2) #one output neuron for every variable.  
    return(res)
  } else {
    stop("Wrong encoding specified.")
  }
  
}

#'@export
State.2.Array.Hotelling <- function(game.object, game.state){
  
  if(is.null(game.object$encoding.state)){
    encoding <- "main"
  } else {
    encoding <- game.object$encoding.state
  }
  
  if(encoding == "main"){
    
   arr <- vector(mode = "numeric", length = 14)
   
   if(game.state$round ==1){
     arr[1] <- TRUE
   } else {
     arr[2] <- game.state$history.price[game.state$round-1,1]
     arr[3] <- game.state$history.price[game.state$round-1,2]
     arr[4] <- game.state$history.loc[game.state$round-1,1]
     arr[5] <- game.state$history.loc[game.state$round-1,2]
     arr[6] <- game.state$history.price[game.state$round-1,1] - game.state$history.price[game.state$round-1,2] 
     arr[7] <- game.state$history.loc[game.state$round-1,1] - game.state$history.loc[game.state$round-1,2]
     arr[8] <- game.state$round/100
     arr[9] <- mean(game.state$history.price[1:game.state$round-1,2])
     arr[10] <- mean(game.state$history.loc[1:game.state$round-1,2])
   }
   
   if(game.state$round > 2){
     arr[11] <- game.state$history.price[game.state$round-2,1]
     arr[12] <- game.state$history.price[game.state$round-2,2]
     arr[13] <- game.state$history.loc[game.state$round-2,1]
     arr[14] <- game.state$history.loc[game.state$round-2,2]
   }
   
   return(arr)
  }   
}

#'@export
Get.Game.Object.Hotelling <- function(encoding.action = NULL, encoding.state = NULL){
  
  name <- "Hotelling"
  
  game.par <- Get.Par.Hotelling
  state.transition <- State.Transition.Hotelling
  start.state <- Generate.Start.State.Hotelling
  state.2.array <- State.2.Array.Hotelling
  
  game.pars <- Get.Game.Param.Hotelling()
  
  game.object <- nlist(name, game.par, state.transition, start.state, state.2.array, game.pars, encoding.action, encoding.state)
  return(game.object)
}


