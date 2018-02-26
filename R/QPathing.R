#' Delivers some default Parameters of Q-learning
#'
#' @export
Get.Def.Par.QPathing <- function(){

  #How to choose Actions
  action.policy  <- "epsilon.greedy" #"epsilon.greedy"
  weighting.policy <- "mean.weighted"
  weighting.factor <- 10 #Overweight new information by factor

  #Relevant parameters of epsilon.greedy
  epsilon.start <- 0.1
  epsilon.decay <- NA
  epsilon.min <- 0.001
  epsilon.decay.type <- "rate" #may be "rate" or "linear"

  # Incorporating new information and Updating Status
  mem.selection <- "all" #currently no other options are available - relevant for backwards compatibility
  mem.type <- "game.encoded.rounds" #game.encoded or game.encoded.rounds
  replay.every <- 100 # After How many rounds should the prediction model be updated?

  # Output
  show.current.status <- 50

  q.param <- nlist(action.policy,weighting.policy, weighting.factor, epsilon.start, epsilon.decay, epsilon.min, epsilon.decay.type,mem.selection, mem.type, show.current.status, replay.every)

  return(q.param)
}

#Q-Pathing
#' Q-Pathing is rather similar to Q-learning but we have stronger assumptions:\itemize{
#' \item We only receive our reward at the end.
#' \item We update instantly.
#' \item We use our predictive model only in those cases, where we have not yet found information.
#' \item We have a single starting state.
#' }
#'
#' @param game.object Game Object as defined by \code{Get.Game.Object.<NAME>}.
#' @param model.par Model parameters. If \code{NULL}, the function \code{Get.Def.Par.QPathing()} is called.
#' @export
Setup.QPathing <- function(game.object, algo.par=NULL, model.par){
  restore.point("Setup.QPathing")
  if(is.null(algo.par)){
    algo.par <- Get.Def.Par.QPathing()
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
#' \item \strong{expected.reward} Expected value of this node.
#' \item \strong{precursors} All possibilites to get to this node
#' \item \strong{successors} All nodes to go to. Is a list itself with one entry for each action. Here a list with the following items exist: Number of Reached node. Number of visits.
#' }
#' }
#'
#' @param game.object A Game Object as defined by \code{Get.Game.Object.<Name>}. Necessary in the case of memory intitialisation.
#' @param model.par Parameters of QPathing specification. Have to be specified and should be identical to the model.par as given to \code{Setup.QPathing()}.
#' @param memory.init Which type of initialization should take place? It \code{NULL}, the option \code{none} is used. The following types are supported \itemize{
#' \item \strong{none} No initialization takes place. Memory is an empty list.
#' \item \strong{self.play} The other strategies play against themselves - to understand possible secret handshakes. The following \code{memory.param} are expected: \itemize{
#' \item \strong{no} How often should the other strategies play against themselves?
#' }
#' }
#' If combinations of different memories are needed, one can use the function \code{Extend.Memory.QPathing()}
#' @param memory.param Parameters necessary for the chosen \code{memory.init}.
#'@export
Initialise.QPathing <- function(game.object=NULL, algo.par, memory.init=NULL, memory.param = NULL){
  if(is.null(memory.init)){
    memory.init <- "none"
  }

  algo.var <- list()
  algo.var$epsilon <- algo.par$epsilon.start
  algo.var$memory.net <- list()

  if (memory.init != "none") {
    algo.var <- Extend.Memory.QPathing(algo.var, algo.par=algo.par, game.object=game.object, memory.type=memory.init, memory.param=memory.param)
  }

  return(algo.var)
}

#' Extend Memory by specified experiences
#'
#' Returns modified algo.var, where memory has been extended as specified.
#'
#' @param algo.var A variable algorithm object, where to be modified variables are saved. Given by \code{Initialise.QPathing()}
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
Extend.Memory.QPathing <- function(algo.var, algo.par=NULL, game.object, memory.type, memory.param=NULL){
  restore.point("Extend.Memory.QPathing")
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
  algo.var$memory.net <- Update.Net.QPathing(algo.var$memory.net, new.mem, game.object, algo.par)
  return(algo.var)
}

#'Calc.Reward.QPathing
#'
#' @export
Calc.Reward.QPathing <- function(net, pointer, action=NULL, mode, mode.par){
  restore.point("Calc.Reward.QPathing")
  default.val <- NA
  if(length(net[[pointer]]$successors)==0){
    return(default.val)
  }
  if(!is.null(action) && length(net[[pointer]]$successors[[action]])==0){
    return(default.val)
  }

  if(is.null(action)){
    pointers <- net[[pointer]]$successors
  }

  if(mode=="max"){
    res <- max(sapply(pointers,FUN=function(x){
      if(length(x)==0){
        return(default.val)
      }
      return(max(sapply(x,FUN=function(y){
        net[[y]]$expected.reward
      }), na.rm=TRUE))
    }), na.rm=TRUE)
  } else if (mode=="mean.weighted") {
    if(is.null(mode.par$weighting.factor)){
      stop("weighting factor not provided!")
    }
    if(!is.null(action)){
      if(length(net[[pointer]]$successors[[action]])==0){
          return(default.val)
      }
      not.na <- sapply(1:length(net[[pointer]]$successors[[action]]),FUN=function(y){return(!is.na(net[[net[[pointer]]$successors[[action]][y]]]$expected.reward))})
      if(!any(not.na)){
        return(NA)
      }
      res.nom <- sum(sapply((1:length(net[[pointer]]$successors[[action]]))[not.na],FUN=function(y){
          net[[net[[pointer]]$successors[[action]][y]]]$expected.reward*net[[pointer]]$successors.visited[[action]][[y]]
      }))
      res.denom <- sum(unlist(net[[pointer]]$successors.visited[[action]][not.na]))
      res <- res.nom/res.denom
      return(res)
    } else {
      action.vals <- sapply(1:length(pointers),FUN=function(x){ #going through actions
        if(length(net[[pointer]]$successors[[x]])==0){
          return(default.val)
        }
        not.na <- sapply(1:length(net[[pointer]]$successors[[x]]),FUN=function(y){return(!is.na(net[[net[[pointer]]$successors[[x]][y]]]$expected.reward))})
        if(!any(not.na)){
          return(NA)
        }
        res.nom <- sum(sapply((1:length(net[[pointer]]$successors[[x]]))[not.na],FUN=function(y){
          net[[net[[pointer]]$successors[[x]][y]]]$expected.reward*net[[pointer]]$successors.visited[[x]][[y]]
        }))
        res.denom <- sum(unlist(net[[pointer]]$successors.visited[[x]][not.na]))
        res <- res.nom/res.denom
        return(res)
      })

      res.tmp <- max(action.vals, na.rm=TRUE)

      if(!is.na(net[[pointer]]$expected.reward)){
        res <- (net[[pointer]]$visited*net[[pointer]]$expected.reward+res.tmp*mode.par$weighting.factor)/(net[[pointer]]$visited+mode.par$weighting.factor)
      } else {
        res <- res.tmp
      }
      return(res)
    }
  } else {
    stop("weighting policy unknown")
  }
}

#'Internal Function
#'
#'Expects one or several Paths.
#'
#'@export
Update.Net.QPathing <- function(net, new.mem, game.object, algo.par){
  init.net.node <- function(state, precursors, round=1){
    visited <- 0
    expected.reward <- NA
    successors <-  rep( list(list()), game.object$game.par(game.object)$output.nodes)
    successors.visited <- rep( list(list()), game.object$game.par(game.object)$output.nodes)
    round <- round
    return(nlist(state, visited, expected.reward, precursors, successors, successors.visited,round))
  }
  restore.point("Update.Net.QPathing")
  if(length(net)==0){ #initialise root
    net[[1]] <- init.net.node(state=new.mem[[1]]$state, precursors=0)
  }

  #Identify start/end points
  starts <- which(sapply(new.mem,FUN=function(x){x$start}))
  ends <- which(sapply(new.mem,FUN=function(x){x$done}))
  no.paths <- length(starts)

  #Update net
  for(i in 1:no.paths){
  for(j in starts[i]:ends[i]){
    restore.point("within.for.update")
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
        net[[no.next]]$precursors[length(net[[no.next]]$precursors)+1] <- net.pointer
      }
      net[[net.pointer]]$successors[[x$action]] <- no.next
      net[[net.pointer]]$successors.visited[[x$action]] <- 1
      net.pointer <- no.next
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
        net.pointer <- no.next
      } else { # we have already visited the next state
        net[[net.pointer]]$successors.visited[[x$action]][pos.no.next] <- net[[net.pointer]]$successors.visited[[x$action]][pos.no.next]+1
        net.pointer <- no.next
      }
    }

    if(j==ends[i]){
      net[[net.pointer]]$visited <- net[[net.pointer]]$visited+1
    }
  }
  restore.point("before.while.Update.Net")

  #net has now been filled with new nodes (if necessary)
  #We may now update the paths
  #If there are cycles this algo does not update perfectly but still halts
  already.updated <- rep(FALSE,length(net))
  end.state <- TRUE
  backlog <- integer(0)
  while(TRUE){
    #restore.point("inside.Updating.While")
    already.updated[net.pointer] <- TRUE

    if(end.state){
      if(!is.na(net[[net.pointer]]$expected.reward)){
        net[[net.pointer]]$expected.reward <- (new.mem[[j]]$reward + net[[net.pointer]]$expected.reward*(net[[net.pointer]]$visited-1))/net[[net.pointer]]$visited
      } else {
        net[[net.pointer]]$expected.reward <- new.mem[[j]]$reward
      }

      end.state <- FALSE
    } else {

      #Here the weighting policy takes place!
      net[[net.pointer]]$expected.reward <- Calc.Reward.QPathing(net=net, pointer=net.pointer, mode=algo.par$weighting.policy, mode.par=list(weighting.factor=algo.par$weighting.factor))
      if(is.na(net[[net.pointer]]$expected.reward) || is.infinite(net[[net.pointer]]$expected.reward)){
        stop("NA as expected reward in updating or infinite!")
      }
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
  #Check for Bugs
  na.found <- sapply(net,function(x){return(is.na(x$expected.reward))})
  if(any(na.found)){
    stop("There is an error in the Update function!")
  }
  }
  return(net)
}

#' Train model of Q Pathing
#'
#' As QPathing updates all new paths directly Replay only trains the prediction model.
#'
#' @export
Replay.QPathing <- function(model, model.par, algo.par, algo.var, game.object){
  restore.point("Replay.QPathing")

  no.actions <- game.object$game.par(game.object)$output.nodes

  x_train.raw <- lapply(1:length(algo.var$memory.net), FUN=function(x){
    restore.point("x.train.raw.replay.qpathing")
    rel.a <- sapply(1:length(algo.var$memory.net[[x]]$successors), FUN=function(y){
      if(length(algo.var$memory.net[[x]]$successors[[y]])>0){
        a.vec <- rep(0,no.actions+1)
        a.vec[y] <- 1
        a.vec[length(a.vec)] <- Calc.Reward.QPathing(net=algo.var$memory.net, pointer=x, action=y,mode=algo.par$weighting.policy, mode.par=list(weighting.factor=algo.par$weighting.factor))
        if(is.na(a.vec[length(a.vec)])){
          stop("NA as expected reward!")
        }
        return(a.vec)
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
Hybrid.Predict.Action.Values.QPathing <- function(net, no.actions, net.pointer, model, model.par, state){
  restore.point("Hybrid.Predict.Action.Values.QPathing")
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
    act.vals <- sapply(1:no.actions,FUN=function(y){
      restore.point("act.vals.inside")
      return(Calc.Reward.QPathing(net=net, pointer=net.pointer, action=y, mode=algo.par$weighting.policy, mode.par=list(weighting.factor=algo.par$weighting.factor)))
    })
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
Act.QPathing <- function(state, model, model.par, algo.var, game.object, eval.only=FALSE, net.pointer){
  restore.point("Act.QPathing")
  if(eval.only){
    no.actions <- game.object$game.par(game.object)$output.nodes
    act.values <- Hybrid.Predict.Action.Values.QPathing(net=algo.var$memory.net, no.actions=no.actions, net.pointer=net.pointer, model=model, model.par=model.par, state=state)
    return(which.is.max(act.values))
  }

  if(algo.par$action.policy=="epsilon.greedy"){
    if(runif(1) <= algo.var$epsilon){
      game.par <- game.object$game.par(game.object)
      return(sample(1:game.par$output.nodes,1))
    } else {
      no.actions <- game.object$game.par(game.object)$output.nodes
      act.values <- Hybrid.Predict.Action.Values.QPathing(net=algo.var$memory.net, no.actions=no.actions, net.pointer=net.pointer, model=model, model.par=model.par, state=state)
      return(which.is.max(act.values))
    }
  }

  stop("Wrong action policy specified in algo.par.")

}

#' Train a model based on Q-Learning
#'
#' @export
Train.QPathing <- function(model, model.par, algo.par, algo.var, game.object, episodes, eval.only=FALSE, start.w.training=TRUE){
  restore.point("Train.QPathing")
  score.array <- NA
  if(is.null(algo.var$analysis)){
    algo.var$analysis <- list()
    algo.var$analysis$score <- NA
  }

  if(length(algo.var$memory.net)>0 && start.w.training && !eval.only){
    replay.res <- Replay.QPathing(model, model.par, algo.par, algo.var, game.object)
    model <- replay.res$model
    algo.var <- replay.res$algo.var
  }

  for(i in 1:episodes){
    #restore.point("within.Train.QPathing")
    state <- game.object$start.state(game.object)
    mem <- list()
    start<-TRUE
    net.pointer <- 1
    while(TRUE){
      restore.point("within.Train.QPathing.II")
      vis.state <- t(game.object$state.2.array(game.state=state, game.object=game.object)) # not a real state but what the algorithm sees. Could be a lot smaller than the real game state [but might depend on encoding]

      action <- Act.QPathing(state=vis.state, model=model, model.par=model.par, algo.var=algo.var, game.object=game.object, eval.only=eval.only, net.pointer=net.pointer)

      next.state.full <- game.object$state.transition(game.state=state,action=action,game.object=game.object)

      next.state <- next.state.full$next.state
      reward <- next.state.full$reward
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
        algo.var$memory.net <- Update.Net.QPathing(net=algo.var$memory.net, new.mem=mem, game.object=game.object, algo.par=algo.par)

    #Update Prediction Model
      if(i%%algo.par$replay.every==0){
        replay.res <- Replay.QPathing(model, model.par, algo.par, algo.var, game.object)
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

    score.array[i] <- mem[[length(mem)]]$reward #Not used for learning, only for analysis
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


