#' Delivers some default Parameters of Q-Predictions
#'
#' @export
Get.Def.Par.QPredictions <- function(){

  #How to choose Actions & Learning
  action.policy  <- "epsilon.greedy" #"epsilon.greedy"
  weighting.policy <- "SARSA"
  weighting.factor <- 0.1 #a from Q learning
  end.start <- 0.5 #all after this percentage are classified as end points
  gamma <- NA #It is sensible (?) to set it to delta of game

  #Relevant parameters of epsilon.greedy
  epsilon.start <- 0.1
  epsilon.decay <- NA
  epsilon.min <- 0.001
  epsilon.decay.type <- "rate" #may be "rate" or "linear"

  # Incorporating new information and Updating Status
  mem.selection <- "all" #currently no other options are available - relevant for backwards compatibility
  mem.type <- "game.encoded" #game.encoded or game.encoded.rounds
  replay.every <- 100 # After How many rounds should the prediction model be updated?

  # Output
  show.current.status <- 50

  q.param <- nlist(action.policy,weighting.policy, weighting.factor, end.start, gamma, epsilon.start, epsilon.decay, epsilon.min, epsilon.decay.type,mem.selection, mem.type, show.current.status, replay.every)

  return(q.param)
}

#Q-Predictions
#' Q-Predictions is rather similar to Q-learning but we use a multilayer approach:\itemize{
#' \item We assume that even though the algorithm doesn't have complete information at runtime it may be used for training
#' \item A predictive neural net is used to calculate the next move of the opponent to use monte carlo studies.
#' \item Assumption: We have a single starting state.
#' }
#'
#' @param game.object Game Object as defined by \code{Get.Game.Object.<NAME>}.
#' @param model.par Model parameters. If \code{NULL}, the function \code{Get.Def.Par.QPredictions()} is called.
#' @export
Setup.QPredictions <- function(game.object, algo.par=NULL, model.par){
  restore.point("Setup.QPredictions")
  if(is.null(algo.par)){
    algo.par <- Get.Def.Par.QPredictions()
  }

  game.par <- game.object$game.par(game.object)

  if(is.null(model.par$name)){
    stop("name parameter of model.par missing!")
  }

  model <- model.par$setup(model.par, game.par)

  return(model)
}

#' Set changeable model variables
#'
#' Returns a list with the following items \itemize{
#' \item \strong{epsilon} Specifies how often the Algorithm tries a random move. Initialized with \code{epsilon.start} of \code{model.par}.
#' \item \strong{memory.net} A net based on the next state. Each Node holds the following information:\itemize{
#' \item \strong{encoding} Game state encoded for prediction algorithm.
#' \item \strong{depth} Current depth of tree. In the case of the Prisoners Dilemma equal to the round.
#' \item \strong{visited} How often have we visited this node?
#' \item \strong{Q.values} Q.values of this node.
#' \item \strong{precursors} All possibilites to get to this node
#' \item \strong{successors} All nodes to go to. Is a list itself with one entry for each action. Here a list with the following items exist: Number of Reached node. Number of visits.
#' }
#' }
#'
#' @param game.object A Game Object as defined by \code{Get.Game.Object.<Name>}. Necessary in the case of memory intitialisation.
#' @param model.par Parameters of QPredictions specification. Have to be specified and should be identical to the model.par as given to \code{Setup.QPredictions()}.
#' @param memory.init Which type of initialization should take place? It \code{NULL}, the option \code{none} is used. The following types are supported \itemize{
#' \item \strong{none} No initialization takes place. Memory is an empty list.
#' \item \strong{self.play} The other strategies play against themselves - to understand possible secret handshakes. The following \code{memory.param} are expected: \itemize{
#' \item \strong{no} How often should the other strategies play against themselves?
#' }
#' }
#' If combinations of different memories are needed, one can use the function \code{Extend.Memory.QPredictions()}
#' @param memory.param Parameters necessary for the chosen \code{memory.init}.
#'@export
Initialise.QPredictions <- function(game.object=NULL, algo.par, memory.init=NULL, memory.param = NULL){
  if(is.null(memory.init)){
    memory.init <- "none"
  }

  algo.var <- list()
  algo.var$epsilon <- algo.par$epsilon.start
  algo.var$memory.net <- list()

  if (memory.init != "none") {
    algo.var <- Extend.Memory.QPredictions(algo.var, algo.par=algo.par, game.object=game.object, memory.type=memory.init, memory.param=memory.param)
  }

  return(algo.var)
}

#' Extend Memory by specified experiences
#'
#' Returns modified algo.var, where memory has been extended as specified.
#'
#' @param algo.var A variable algorithm object, where to be modified variables are saved. Given by \code{Initialise.QPredictions()}
#' @param game.object A game object as defined by \code{Get.Game.Object.<Name>}.
#' @param memory.init Which type of extension should take place? The following types are supported \itemize{
#' \item \strong{self.play} The other strategies play against themselves - to understand possible secret handshakes. If I am myself part of the other strategies, the "self" strategy is ignored. The following \code{memory.param} are expected: \itemize{
#' \item \strong{no} How often should the other strategies play against themselves?
#' }
#' \item \strong{solid.foundation} Not only self.play, but also a random initialisation with increasing defect probabilities.The following \code{memory.param} are expected: \itemize{
#' \item \strong{self.no} How often should the other strategies play against themselves?
#' \item \strong{rep.no} How often should a random strategy be played? The defection probability is linearly increased.
#' }
#' }
#' If combinations of different memories are needed, one can use the function multiple times.
#' @param memory.param Parameters necessary for the chosen \code{memory.type}.
#'@export
Extend.Memory.QPredictions <- function(algo.var, algo.par=NULL, game.object, memory.type, memory.param=NULL){
  restore.point("Extend.Memory.QPredictions")
  if(memory.type == "self.play"){
    new.mem <- unlist(lapply(1:memory.param$no,FUN=function(x){
      if(!is.null(game.object$supports) && any(game.object$supports == "memory.self.play")){
        return(game.object$memory.self.play(game.object, algo.par))
      }
    }), recursive=FALSE)
  } else if (memory.type== "solid.foundation"){
    self.mem <- unlist(lapply(1:memory.param$self.no,FUN=function(x){
      if(!is.null(game.object$supports) && any(game.object$supports == "memory.self.play")){
        return(game.object$memory.self.play(game.object, algo.par))
      }
    }), recursive=FALSE)
    def.arr <- seq(0,1,length.out = memory.param$rep.no)
    rand.mem <- unlist(lapply(def.arr,FUN=function(x){
      if(!is.null(game.object$supports) && any(game.object$supports == "memory.random.play")){
        algo.par$def.prob <- x
        return(game.object$memory.random.play(game.object, algo.par))
      }
    }), recursive=FALSE)
    new.mem <- c(self.mem, rand.mem)
  } else {
    stop(paste0("memory.type ",memory.type," not supported."))
  }
  algo.var$memory.net <- Update.Net.QPredictions(algo.var$memory.net, new.mem, game.object, algo.par)
  return(algo.var)
}

#'Calculate Expected Value based on action
#'
#'Needed to calculate the next Q value. If converged, should result in the same value as the respective Q.value itself.
#'Also outputs edge value.
#'
#' @export
Calc.Reward.QPredictions.expectedQ <- function(net, pointer, action, option="ignore", mode){
  restore.point("Calc.Reward.QPredictions.expectedQ")

  if(option!="ignore") {
    stop("Not yet implemented. Use option ignore in Calc.Reward.QPredictions.expectedQ")
  }

  suc.vec <- net[[pointer]]$successors[[action]]
  if(option=="ignore" && length(suc.vec)==0){ #No successors known
    return(NA)
  }
  suc.weights <- net[[pointer]]$successors.visited[[action]]
  rel.weights <- suc.weights/sum(suc.weights)

  if(mode=="max"){
    Q.values <- sapply(suc.vec,FUN=function(x){
      if(all(is.na(net[[x]]$Q.values))){
        return(NA)
      } else {
        return(max(net[[x]]$Q.values, na.rm=TRUE))
      }
    })
    expQ <- sum((Q.values)*rel.weights)
    return(expQ)
  } else {
    stop("Not implemented yet.")
  }
}

#'Calculate Expected immediate Reward based on action
#'
#'Returns expected Reward based on historic data. Only used inuternally to update net as input for q values.
#'
#' @export
Calc.Reward.QPredictions.expectedReward <- function(net, pointer, action, mode="history"){
  restore.point("Calc.Reward.QPredictions.expectedReward")

  if(mode!="history") {
    stop("Not yet implemented. Use option history in Calc.Reward.QPredictions.expectedReward")
  }

  suc.vec <- net[[pointer]]$successors[[action]]
  if(length(suc.vec)==0){ #No successors known
    return(NA)
  }
  suc.weights <- net[[pointer]]$successors.visited[[action]]
  rel.weights <- suc.weights/sum(suc.weights)
  edge.rewards <- net[[pointer]]$edge.rewards[[action]]

  if(mode=="history"){
    expR <- sum(edge.rewards*rel.weights)
    return(expR)
  } else {
    stop("Not implemented yet.")
  }
}

#'Calc.Reward.QPredictions
#'
#' @export
Calc.Reward.QPredictions <- function(net, pointer, action, mode, mode.par, end.state=NULL, end.reward=NULL){
  restore.point("Calc.Reward.QPredictions")
  if(!is.na(net[[pointer]]$Q.values[action])){
    Q.current <- net[[pointer]]$Q.values[action]
  } else { #Here optimistic evaluation might take place, but we path through
    Q.current <- NA
  }

  if(!is.null(end.state)){
    #there are no relevant following Q.states
    Q.next <- end.state/(1-algo.par$gamma) #Expected value of repeating end.state infinitely often but with discontinuity probability of gamma
    reward <- end.reward
  } else {
    if(mode=="max"){
      Q.next <- Calc.Reward.QPredictions.expectedQ(net=net, pointer=pointer,action=action, mode=mode)
      if(length(Q.next)>1){
        stop("Q to big")
      }
      if(is.infinite(Q.next)){
        stop("is infinite")
      }
      reward <- Calc.Reward.QPredictions.expectedReward(net=net, pointer=pointer,action=action)
    } else {
      stop("weighting policy unknown")
    }
  }

  Q.update <- (reward + algo.par$gamma * Q.next)


  if(is.na(Q.current)){
    res <- Q.update
  } else {
    res <- (1-algo.par$weighting.factor) * Q.current + algo.par$weighting.factor * Q.update
  }

  return(res)
}

#' Calculates Endstate value
#'
#' Currently no option ist supported. Endstate value is calculated as average from end.pointer.pos to end of mem.path.
#'
#'@export
Calc.Endstate.Value.QPredictions <- function(net, end.pointer.pos.net, end.pointer.pos.path, mem.path, option=NULL){
  restore.point("Calc.Endstate.Value.QPredictions")

  if(is.null(option)){
    option <- "SARSA"
  }

  if(option=="SARSA"){
    res <- mean(unlist(lapply(mem.path[end.pointer.pos.path:length(mem.path)],FUN=function(x){x$reward})))
  } else {
    stop("Endstate-Value can't be calculated due to incorrect option.")
  }

  return(res)
}

#'Internal Function
#'
#'Expects one or several Paths.
#'
#'@export
Update.Net.QPredictions <- function(net, new.mem, game.object, algo.par){
  init.net.node <- function(state, precursors, round=1){
    visited <- 0
    successors <-  rep( list(list()), game.object$game.par(game.object)$output.nodes)
    successors.visited <- rep( list(list()), game.object$game.par(game.object)$output.nodes)
    edge.rewards <- rep( list(list()), game.object$game.par(game.object)$output.nodes)
    Q.values <- rep(NA, game.object$game.par(game.object)$output.nodes)
    round <- round
    return(nlist(state, visited, precursors, successors, successors.visited, edge.rewards, Q.values, round))
  }
  restore.point("Update.Net.QPredictions")
  if(length(net)==0){ #initialise root
    net[[1]] <- init.net.node(state=new.mem[[1]]$state, precursors=0)
  }

  #Identify start/end points
  starts <- which(sapply(new.mem,FUN=function(x){x$start}))
  ends <- which(sapply(new.mem,FUN=function(x){x$done}))
  no.paths <- length(starts)


  #Update net
  for(i in 1:no.paths){
    net.pointer.path <- c(1)
    end.pointer.pos <- ceiling(algo.par$end.start*(ends[i]-starts[i]))+1+starts[i]
    for(j in starts[i]:ends[i]){
      #restore.point("within.for.update")
      x <- new.mem[[j]]
      if(j==starts[i]){
        net.pointer <- 1
      }
      net[[net.pointer]]$visited <- net[[net.pointer]]$visited+1
      if(length(net[[net.pointer]]$successors[[x$action]])==0){ #first time visit
        if(game.object$full.encoding){ #Cycles are not possible
          no.next <- integer(0)
        } else {
          if(algo.par$mem.type=="game.encoded.round"){
            could.be.state <- which(sapply(1:length(net),FUN=function(y){
              net[[y]]$round==(x$round+1)
            }))
          } else {
            could.be.state <- 1:length(net)
          }
          if(length(could.be.state)==0){
            no.next <- integer(0)
          } else {
            no.next <- which(sapply(could.be.state,FUN=function(y){
              identical(x$next.state,net[[y]]$state)
            }))
          }
        }
        if(length(no.next)==0){
          no.next <- length(net)+1
          net[[no.next]] <- init.net.node(state=x$next.state, precursors=net.pointer, round=x$round+1)
        } else {
          ### Here we have to check whether the precursor is already identical to the net.pointer
          net[[no.next]]$precursors[length(net[[no.next]]$precursors)+1] <- net.pointer
        }
        net[[net.pointer]]$successors[[x$action]] <- no.next
        net[[net.pointer]]$successors.visited[[x$action]] <- 1
        net[[net.pointer]]$edge.rewards[[x$action]] <- x$reward
      } else { #There are already known states
        #Check whether it is one of the states we have seen.
        pos.no.next <- which(sapply(net[[net.pointer]]$successors[[x$action]],FUN=function(y){
          identical(x$next.state,net[[y]]$state)
        }))
        no.next <- net[[net.pointer]]$successors[[x$action]][pos.no.next]

        if(length(no.next)==0){ #first time visit but already others there
          if(game.object$full.encoding){ #Cycles are not possible
            no.next <- integer(0)
          } else {
            if(algo.par$mem.type=="game.encoded.round"){
              could.be.state <- which(sapply(1:length(net),FUN=function(y){
                net[[y]]$round==(x$round+1)
              }))
            } else {
              could.be.state <- 1:length(net)
            }
            if(length(could.be.state)==0){
              no.next <- integer(0)
            } else {
              no.next <- which(sapply(could.be.state,FUN=function(y){
                identical(x$next.state,net[[y]]$state)
              }))
            }
          }
          if(length(no.next)==0){
            no.next <- length(net)+1
            net[[no.next]] <- init.net.node(state=x$next.state, precursors=net.pointer, round=x$round+1)
          } else {
            net[[no.next]]$precursors[length(net[[no.next]]$precursors)+1] <- net.pointer
          }
          net[[net.pointer]]$successors[[x$action]] <- c(net[[net.pointer]]$successors[[x$action]], no.next)
          net[[net.pointer]]$successors.visited[[x$action]] <- c(net[[net.pointer]]$successors.visited[[x$action]],1)
          net[[net.pointer]]$edge.rewards[[x$action]] <- c(net[[net.pointer]]$edge.rewards[[x$action]],x$reward)
        } else { # we have already visited the next state
          net[[net.pointer]]$edge.rewards[[x$action]][pos.no.next] <- (net[[net.pointer]]$edge.rewards[[x$action]][pos.no.next]*net[[net.pointer]]$successors.visited[[x$action]][pos.no.next]+x$reward)/(net[[net.pointer]]$successors.visited[[x$action]][pos.no.next]+1)
          net[[net.pointer]]$successors.visited[[x$action]][pos.no.next] <- net[[net.pointer]]$successors.visited[[x$action]][pos.no.next]+1
        }
      }

      if(j==end.pointer.pos){
        net.pointer.end <- net.pointer
      }
      net.pointer <- no.next

      if(j==ends[i]){
        net[[net.pointer]]$visited <- net[[net.pointer]]$visited+1
      }
      net.pointer.path <- c(net.pointer.path,net.pointer)
    }

    restore.point("before.while.Update.Net")
    #net has now been filled with new nodes (if necessary)
    #We may now update the paths
    #If there are cycles this algo does not update perfectly but still halts

    already.updated <- rep(FALSE,length(net))
    #We go again through the path, this time from the end to the start
    #end states could be several with the later ones not as good


    #Determine Endstate value
    end.state.value <- Calc.Endstate.Value.QPredictions(net=net, end.pointer.pos.net=net.pointer.end, end.pointer.pos.path=end.pointer.pos-starts[i]+1, mem.path=new.mem[starts[i]:ends[i]])
    end.reward <- end.state.value
    end.pos <- net.pointer.path[length(net.pointer.path)]
    net[[end.pos]]$Q.values <- rep(Calc.Reward.QPredictions(net=net, pointer=net.pointer, action=new.mem[[j]]$action, mode=algo.par$weighting.policy, mode.par=list(weighting.factor=algo.par$weighting.factor, gamma=algo.par$gamma), end.state=end.state.value, end.reward=end.reward),length(net[[net.pointer]]$Q.values))
    end.state.value <- NULL
    already.updated[end.pos] <- TRUE

    for(j in ends[i]:starts[i]){
      net.pointer <- net.pointer.path[j-(starts[i])+1]
      if(!already.updated[net.pointer]){
        net[[net.pointer]]$Q.values[new.mem[[j]]$action] <- Calc.Reward.QPredictions(net=net, pointer=net.pointer, action=new.mem[[j]]$action, mode=algo.par$weighting.policy, mode.par=list(weighting.factor=algo.par$weighting.factor, gamma=algo.par$gamma), end.state=end.state.value, end.reward=end.reward)
        already.updated[net.pointer] <- TRUE
      }
    }

    backlog <- integer(0)
    net.pointer <- net.pointer.path[length(net.pointer.path)] #Start again from end.node
    while(TRUE){
      restore.point("inside.Updating.While")

      if(!already.updated[net.pointer]){
        a.length=length(net[[net.pointer]]$successors)
        net[[net.pointer]]$Q.values <- sapply(1:a.length,FUN=function(x){
          Calc.Reward.QPredictions(net=net, pointer=net.pointer, action=x, mode=algo.par$weighting.policy, mode.par=list(weighting.factor=algo.par$weighting.factor, gamma=algo.par$gamma), end.state=NULL)
        })

        if(all(is.na(net[[net.pointer]]$Q.values) | is.infinite(net[[net.pointer]]$Q.values))){
            stop("NA as Q Values in updating or infinite!")
        }
        already.updated[net.pointer] <- TRUE
      }

      if(length(backlog)==0){
        to.check <- which(sapply(net[[net.pointer]]$precursors,FUN=function(x){
            return(!(x %in% backlog) && !(already.updated[x]))
        }))
        to.check.no <- net[[net.pointer]]$precursors[to.check]
        if(length(to.check.no)==0){
          break
        }
        if(length(to.check)==1){
          net.pointer <- to.check.no
        } else {
          net.pointer <- to.check.no[1]
          backlog <- c(backlog, to.check.no[-1])
        }
      } else {
        to.check <- which(sapply(net[[net.pointer]]$precursors,FUN=function(x){
          return(!(x %in% backlog) && !(already.updated[x]))
        }))
        if(length(to.check)==0){
          #do nothing
        } else {
          to.check.no <- net[[net.pointer]]$precursors[to.check]
          backlog <- c(backlog,to.check.no)
        }
        net.pointer <- backlog[1]
        backlog <- backlog[-1]
      }
    }
  }
  return(net)
}

#' Train model of Q Pathing
#'
#' As QPredictions updates all new paths directly Replay only trains the prediction model.
#'
#' @export
Replay.QPredictions <- function(model, model.par, algo.par, algo.var, game.object){
  restore.point("Replay.QPredictions")

  no.actions <- game.object$game.par(game.object)$output.nodes

  x_train.raw <- lapply(1:length(algo.var$memory.net), FUN=function(x){
    restore.point("x.train.raw.replay.QPredictions")
    rel.a <- sapply(1:length(algo.var$memory.net[[x]]$Q.values), FUN=function(y){
      if(!is.na(algo.var$memory.net[[x]]$Q.values[y])){
        res <- rep(0,length(algo.var$memory.net[[x]]$Q.values)+1)
        res[y] <- 1
        res[length(algo.var$memory.net[[x]]$Q.values)+1] <- algo.var$memory.net[[x]]$Q.values[y]
        return(res)
      } else {
        return(NULL)
      }
    })
    rel.a.vec.tmp <- unlist(rel.a, recursive=FALSE)
    if(is.null(rel.a.vec.tmp)){
      return(NULL)
    } else {
      rel.a.vec <- t(rel.a.vec.tmp)
    }

    restore.point("inside.x.train.replay")
    n.row <- nrow(rel.a.vec)
    state.info <- matrix(rep(algo.var$memory.net[[x]]$state,n.row),nrow=n.row, byrow=TRUE)
    x.res <- cbind(state.info,rel.a.vec)
    return(x.res)
  })

  x_train <- do.call(rbind,x_train.raw)
  y_train <- as.matrix(x_train[,ncol(x_train)])
  x_train <- x_train[,-ncol(x_train)]

  if(is.null(model.par$single.dimensional) || !(model.par$single.dimensional)){
    stop("Model has to support single.dimensional and has it enabled.")
  }

  #Setup necessary precision
  if(!is.null(model.par$enforce.increasing.precision)&&model.par$enforce.increasing.precision==TRUE){
    prec.repeat <- TRUE
  } else {
    prec.repeat <- FALSE
  }

  #Main Part -> Training of model
  model.train <- model.par$train(model, model.par, x_train, y_train)
  model <- model.train$model
  fit.obj <- model.train$fit.obj
  restore.point("before.pre.training")
  #If model is not trained enough, repeat Training until ready
  if(prec.repeat && is.null(algo.var$cur.loss)){
    algo.var$cur.loss <- mean(fit.obj$metrics$loss)
  } else if (prec.repeat) {
    counter <- 0
    while(mean(fit.obj$metrics$loss)>algo.var$cur.loss){
      counter <- counter+1
      writeLines(paste0("Loss was only ",round(mean(fit.obj$metrics$loss),5), " but ",round(algo.var$cur.loss,5), " needed","\n",collapse=""))
      model.train <- model.par$train(model, model.par, x_train, y_train)
      model <- model.train$model
      fit.obj <- model.train$fit.obj
      if(counter>model.par$give.up.precision){
        break
      }
    }
  }
  if(prec.repeat){
    algo.var$cur.loss <- mean(fit.obj$metrics$loss)
  }

  return(nlist(model, algo.var))
}

#' Generates best guesses based on Experience
#'@export
Hybrid.Predict.Action.Values.QPredictions <- function(net, no.actions, net.pointer, model, model.par, state){
  restore.point("Hybrid.Predict.Action.Values.QPredictions")
  if(is.null(net.pointer)){ #we are flying blind
    rel.a <- unlist(sapply(1:no.actions, FUN=function(y){
      a.vec <- rep(0,no.actions)
      a.vec[y] <- 1
      return(a.vec)
    }))
    state.info <- matrix(rep(state,no.actions),nrow=no.actions, byrow=TRUE)
    x.res <- cbind(state.info,rel.a)
    return(as.vector(model.par$predict(model,model.par,x.res)))
  } else {
    act.vals <- net[[net.pointer]]$Q.values
    if(any(is.na(act.vals))){
      #Build info for unknown quantities
      unknown <- t(unlist(sapply((1:no.actions)[is.na(act.vals)], FUN=function(y){
        a.vec <- rep(0,no.actions)
        a.vec[y] <- 1
        return(a.vec)
      })))
      state.info <- matrix(rep(state,nrow(unknown)),nrow=nrow(unknown), byrow=TRUE)
      x.res <- cbind(state.info,unknown)
      pred.vals <- as.vector(model.par$predict(model,model.par,x.res))
      act.vals[is.na(act.vals)] <- pred.vals
    }
    return(act.vals)
  }
}

#' Determines which action to take
#'
#' @export
Act.QPredictions <- function(state, model, model.par, algo.var, game.object, eval.only=FALSE, net.pointer){
  restore.point("Act.QPredictions")
  if(eval.only){
    no.actions <- game.object$game.par(game.object)$output.nodes
    act.values <- Hybrid.Predict.Action.Values.QPredictions(net=algo.var$memory.net, no.actions=no.actions, net.pointer=net.pointer, model=model, model.par=model.par, state=state)
    return(which.is.max(act.values))
  }

  if(algo.par$action.policy=="epsilon.greedy"){
    if(runif(1) <= algo.var$epsilon){
      game.par <- game.object$game.par(game.object)
      return(sample(1:game.par$output.nodes,1))
    } else {
      no.actions <- game.object$game.par(game.object)$output.nodes
      act.values <- Hybrid.Predict.Action.Values.QPredictions(net=algo.var$memory.net, no.actions=no.actions, net.pointer=net.pointer, model=model, model.par=model.par, state=state)
      return(which.is.max(act.values))
    }
  }

  stop("Wrong action policy specified in algo.par.")

}

#' Train a model based on Q-Learning
#'
#' @export
Train.QPredictions <- function(model, model.par, algo.par, algo.var, game.object, episodes, eval.only=FALSE, start.w.training=TRUE){
  restore.point("Train.QPredictions")
  score.array <- NA
  if(is.null(algo.var$analysis)){
    algo.var$analysis <- list()
    algo.var$analysis$score <- NA
  }

  if(length(algo.var$memory.net)>0 && start.w.training && !eval.only){
    replay.res <- Replay.QPredictions(model, model.par, algo.par, algo.var, game.object)
    model <- replay.res$model
    algo.var <- replay.res$algo.var
  }

  for(i in 1:episodes){
    #restore.point("within.Train.QPredictions")
    state <- game.object$start.state(game.object)
    mem <- list()
    start<-TRUE
    net.pointer <- 1
    score <- 0
    while(TRUE){
      restore.point("within.Train.QPredictions.II")
      vis.state <- t(game.object$state.2.array(game.state=state, game.object=game.object)) # not a real state but what the algorithm sees. Could be a lot smaller than the real game state [but might depend on encoding]

      action <- Act.QPredictions(state=vis.state, model=model, model.par=model.par, algo.var=algo.var, game.object=game.object, eval.only=eval.only, net.pointer=net.pointer)

      next.state.full <- game.object$state.transition(game.state=state,action=action,game.object=game.object)

      next.state <- next.state.full$next.state
      reward <- next.state.full$reward
      score <- score+reward
      done <- next.state.full$game.finished
      vis.next.state <- t(game.object$state.2.array(game.state=next.state, game.object=game.object))
      round <- next.state.full$next.state$round-1
      #Update net.pointer
      if(is.null(net.pointer) || length(algo.var$memory.net[[net.pointer]]$successors[[action]])==0){
        net.pointer <- NULL
      } else {
        net.pointer <- algo.var$memory.net[[net.pointer]]$successors[[action]][which(sapply(algo.var$memory.net[[net.pointer]]$successors[[action]], FUN=function(x){
          identical(vis.next.state,algo.var$memory.net[[x]]$state)
        }))]
        if(length(net.pointer)==0){
          net.pointer <- NULL
        }
      }


      mem[[length(mem)+1]] <- list(state=vis.state, action=action, next.state=vis.next.state, reward=reward, done=done, start=start, round=round)


      if(done){
        break
      }
      state <- next.state
      start <- FALSE
    }

    #Update Memory
    if(!eval.only){
      algo.var$memory.net <- Update.Net.QPredictions(net=algo.var$memory.net, new.mem=mem, game.object=game.object, algo.par=algo.par)

      #Update Prediction Model
      if(i%%algo.par$replay.every==0){
        replay.res <- Replay.QPredictions(model, model.par, algo.par, algo.var, game.object)
        model <- replay.res$model
        algo.var <- replay.res$algo.var
      }
    }

    if(algo.par$action.policy=="epsilon.greedy" && algo.var$epsilon > algo.par$epsilon.min && i!=episodes){
      if(algo.par$epsilon.decay.type=="linear"){
        algo.var$epsilon <- seq(algo.par$epsilon.start,algo.par$epsilon.min,length.out=episodes)[i+1]
      } else if(algo.par$epsilon.decay.type=="rate"){
        decay.par <- (algo.par$epsilon.min/algo.par$epsilon.start)^(1/episodes)
        algo.var$epsilon <- decay.par*algo.var$epsilon
      } else {
        algo.var$epsilon <- algo.par$epsilon.decay*algo.var$epsilon
      }
    }

    score.array[i] <- score #Not used for learning, only for analysis
    if(is.na(algo.var$analysis$score[1])){
      algo.var$analysis$score[1] <- score.array[i]
    } else {
      algo.var$analysis$score[length(algo.var$analysis$score)+1] <- score.array[i]
    }

    if(i%%algo.par$show.current.status == 0){
      output.message <- paste0(c("episode: ",i," with avg score of ",round(mean(score.array[(i-algo.par$show.current.status+1):i]),2)),collapse="")
      print(output.message)
    }
    if(eval.only){
      eval.mess <- paste0(c("eval: ",i," with score of ",round(score.array[i],2)),collapse="")
      print(eval.mess)
    }
    out.save <<- list(model=model, algo.var=algo.var)
  }

  return(list(model=model, algo.var=algo.var))
}


