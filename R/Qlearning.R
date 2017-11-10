#' Delivers some default Parameters of Q-learning
#'
#' @export
Get.Def.Par.QLearning <- function(){
  epsilon.start <- 1
  epsilon.decay <- 0.9
  epsilon.min <- 0.02
  batch.size <- 10000
  max.mem <- 100000
  remove.memory <- 0.1 #Fraction of Memory to randomly remove, if Memory is full
  gamma <- 1
  show.current.status <- 10
  replay.every <- 50
  replay.intensive <- 10
  q.param <- nlist(epsilon.start, epsilon.decay, epsilon.min, batch.size, max.mem, remove.memory, gamma, show.current.status, replay.every, replay.intensive)
  return(q.param)
}

#Q-Learning with Discrete Choices
#' Sets up a model based on model parameters
#'
#' @param game.object Game Object as defined by \code{Get.Game.Object.<NAME>}.
#' @param model.par Model parameters. If \code{NULL}, the function \code{Get.Def.Par.QLearning()} is called.
#' @export
Setup.QLearning <- function(game.object, algo.par=NULL, model.par){
  restore.point("Setup.QLearning")
  if(is.null(algo.par)){
    algo.par <- Get.Def.Par.QLearning()
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
#' \item \strong{memory} Memory of the Algorithm given past games. May be intitialised to give a starting point to learning.
#' }
#'
#' @param game.object A Game Object as defined by \code{Get.Game.Object.<Name>}. Necessary in the case of memory intitialisation.
#' @param model.par Parameters of QLearning specification. Have to be specified and should be identical to the model.par as given to \code{Setup.Qlearning()}.
#' @param memory.init Which type of initialization should take place? It \code{NULL}, the option \code{none} is used. The following types are supported \itemize{
#' \item \strong{none} No initialization takes place. Memory is an empty list.
#' \item \strong{self.play} The other strategies play against themselves - to understand possible secret handshakes. The following \code{memory.param} are expected: \itemize{
#' \item \stron{no} How often should the other strategies play against themselves?
#' }
#' }
#' If combinations of different memories are needed, one can use the function \code{Extend.Memory.Qlearning()}
#' @param memory.param Parameters necessary for the chosen \code{memory.init}.
#'@export
Initialise.Qlearning <- function(game.object=NULL, algo.par, memory.init=NULL, memory.param = NULL){
  if(is.null(memory.init)){
    memory.init <- "none"
  }

  algo.var <- list()
  algo.var$epsilon <- algo.par$epsilon.start
  algo.var$memory <- list() #items: state, action, reward
  if (memory.init != "none") {
    algo.var <- Extend.Memory.Qlearning(algo.var, game.object=game.object, memory.type=memory.init, memory.param=memory.param)
  }

  return(algo.var)
}

#' Extend Memory by specified experiences
#'
#' Returns modified algo.var, where memory has been extended as specified.
#'
#' @param algo.var A variable algorithm object, where to be modified variables are saved. Given by \code{Initialise.Qlearning()}
#' @param game.object A game object as defined by \code{Get.Game.Object.<Name>}.
#' @param memory.init Which type of extension should take place? The following types are supported \itemize{
#' \item \strong{self.play} The other strategies play against themselves - to understand possible secret handshakes. If I am myself part of the other strategies, the "self" strategy is ignored. The following \code{memory.param} are expected: \itemize{
#' \item \strong{no} How often should the other strategies play against themselves?
#' }
#' }
#' If combinations of different memories are needed, one can use the function multiple times.
#' @param memory.param Parameters necessary for the chosen \code{memory.type}.
#'@export
Extend.Memory.Qlearning <- function(algo.var, game.object, memory.type, memory.param=NULL){
  restore.point("Extend.Memory.Qlearning")
  if(memory.type == "self.play"){
    new.mem <- unlist(lapply(1:memory.param$no,FUN=function(x){
      if(!is.null(game.object$supports) && any(game.object$supports == "memory.self.play")){
        return(game.object$memory.self.play(game.object))
      }
    }), recursive=FALSE)
  } else {
    stop(paste0("memory.type ",memory.type," not supported."))
  }
  algo.var$memory <- c(algo.var$memory,new.mem)
  return(algo.var)
}

#' Train model of Q learning
#'
#' @export
Replay.Qlearning <- function(model, model.par, algo.par, algo.var){
  restore.point("Replay.Qlearning")
  if(length(algo.var$memory)<algo.par$batch.size){
    batch.size <- length(algo.var$memory)
  } else {
    batch.size <- algo.par$batch.size
  }
  minibatch <- sample(algo.var$memory, batch.size)
  #Transform dataset to trainable matrix
  x_train <- t(sapply(minibatch, FUN=function(x){
    return(x$state)
  }))

  x_next_state <- t(sapply(minibatch, FUN=function(x){
    return(x$next.state)
  }))

  reward <- sapply(minibatch, FUN=function(x){
    return(x$reward)
  })

  action <- sapply(minibatch, FUN=function(x){
    return(x$action)
  })

  done <- sapply(minibatch, FUN=function(x){
    return(x$done)
  })

  for(i in 1:algo.par$replay.intensive){
    print(paste0(i,". round of replay(",algo.par$replay.intensive,")"))
    y_train <- model.par$predict(model,x_next_state)
    target <- reward + algo.par$gamma*apply(y_train, 1, FUN=max)
    target[done] <- reward[done]
    y_train <- t(sapply(1:length(action),FUN=function(x){
      res <- y_train[x,]
      res[action[x]] <- target[x]
      return(res)
    }))
    model <- model.par$train(model, model.par, x_train, y_train)
  }

  # pred <- model.par$predict(model,x_train)
  #
  # traindata <- lapply(minibatch, FUN=function(x){
  #   restore.point("inside.reaplay.qlearning")
  #   target <- x$reward
  #   if(!x$done){
  #     pred <- model.par$predict(model,x$next.state)
  #     target <- x$reward + algo.par$gamma*max(pred)
  #   }
  #   target_f <- model.par$predict(model,x$next.state)
  #   target_f[x$action] <- target
  #   return(list(x_train=x$state, y_train=target_f))
  # })
  # x_train <- t(sapply(1:length(traindata),FUN=function(x){
  #   return(traindata[[x]]$x_train)
  # }))
  # y_train <- t(sapply(1:length(traindata),FUN=function(x){
  #   return(traindata[[x]]$y_train)
  # }))
  # model <- model.par$train(model, model.par, x_train, y_train)

  if(algo.var$epsilon > algo.par$epsilon.min){
    algo.var$epsilon <- algo.par$epsilon.decay*algo.var$epsilon
  }
  return(list(model=model, algo.var=algo.var))
}

#' Determines which action to take
#'
#' @export
Act.Qlearning <- function(state, model, model.par, algo.var, game.object){
  restore.point("Act.Qlearning")
  if(runif(1) <= algo.var$epsilon){
    game.par <- game.object$game.par(game.object)
    return(sample(1:game.par$output.nodes,1))
  }
  restore.point("within.Act.Qlearning")
  act.values <- model.par$predict(model,state)
  return(which.is.max(act.values))
}

#' Train a model based on Q-Learning
#'
#' @export
Train.QLearning <- function(model, model.par, algo.par, algo.var, game.object, episodes){
  score.array <- NA
  for(i in 1:episodes){
    restore.point("within.Train.Qlearning")
    state <- game.object$start.state(game.object)
    while(TRUE){
      restore.point("within.Train.Qlearning.II")
      vis.state <- t(game.object$state.2.array(game.state=state, game.object=game.object)) # not a real state but what the algorithm sees. Could be a lot smaller than the real game state [but might depend on encoding]

      action <- Act.Qlearning(state=vis.state, model=model, model.par=model.par, algo.var=algo.var, game.object=game.object)

      next.state.full <- game.object$state.transition(game.state=state,action=action,game.object=game.object)

      next.state <- next.state.full$next.state
      reward <- next.state.full$reward
      done <- next.state.full$game.finished

      algo.var$memory[[length(algo.var$memory)+1]] <- list(state=vis.state, action=action, next.state=t(game.object$state.2.array(game.state=next.state, game.object=game.object)), reward=reward, done=done)

      if(done){
        break
      }
      state <- next.state
    }
    score.array[i] <- algo.var$memory[[length(algo.var$memory)]]$reward #Not used for learning, only for analysis

    if(length(algo.var$memory)>algo.par$max.mem){
      algo.var$memory <- algo.var$memory[-c(sample(1:length(algo.var$memory),round(length(algo.var$memory)*algo.par$remove.memory)))]
    }

    if(i%%algo.par$replay.every == 0){
      replay.res <- Replay.Qlearning(model, model.par, algo.par, algo.var)
      model <- replay.res$model
      algo.var <- replay.res$algo.var
    }

    if(i%%algo.par$show.current.status == 0){
      output.message <- paste0(c("episode: ",i," with avg score of ",round(mean(score.array[(i-algo.par$show.current.status+1):i]),2)),collapse="")
      print(output.message)
    }
  }
  return(list(model=model, algo.var=algo.var))
}


