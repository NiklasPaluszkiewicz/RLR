#' Delivers some default Parameters of Q-learning
#'
#' @export
Get.Def.Par.QLearning <- function(){

  #How to choose Actions
  action.policy  <- "naive.info.treatment" #may be "epsilon.greedy" or "naive.info.treatment

  #Relevant parameters of epsilon.greedy
  epsilon.start <- 1
  epsilon.decay <- 0.95
  epsilon.min <- 0.05

  #Relevant parameters of naive.info.treatment
  exploration.affinity <- 1000 #A Higher Value means states which are not very well understood are highlighted
  lin.falling <- TRUE

  #Memory
  batch.size <- 100000
  max.mem <- 100000
  remove.memory <- 0.1 #Fraction of Memory to randomly remove, if Memory is full
  mem.type <- "game.encoded" #either "game.state" or "game.encoded"
  mem.selection <- "all" #either "all" or "end.state" [end.state is useful if no interal rewards are expected]

  # Incorporating new information and Updating Status
  gamma <- 1
  a <- 0.8
  replay.every <- 1
  replay.intensive <- 1

  # Output
  show.current.status <- 1

  q.param <- nlist(action.policy, epsilon.start, epsilon.decay, epsilon.min, exploration.affinity, lin.falling, batch.size, max.mem, remove.memory, mem.type, mem.selection, gamma, a, show.current.status, replay.every, replay.intensive)

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
#' \item \strong{epsilon} [only in case of epsilon.greedy] Specifies how often the Algorithm tries a random move. Initialized with \code{epsilon.start} of \code{model.par}.
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
  algo.var$exploration.affinity <- algo.par$exploration.affinity
  algo.var$memory <- list() #items: state, action, reward
  if (memory.init != "none") {
    algo.var <- Extend.Memory.Qlearning(algo.var, algo.par=algo.par, game.object=game.object, memory.type=memory.init, memory.param=memory.param)
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
#' \item \strong{solid.foundation} Not only self.play, but also a random initialisation with increasing defect probabilities.The following \code{memory.param} are expected: \itemize{
#' \item \strong{self.no} How often should the other strategies play against themselves?
#' \item \strong{rep.no} How often should a random strategy be played? The defection probability is linearly increased.
#' }
#' }
#' If combinations of different memories are needed, one can use the function multiple times.
#' @param memory.param Parameters necessary for the chosen \code{memory.type}.
#'@export
Extend.Memory.Qlearning <- function(algo.var, algo.par=NULL, game.object, memory.type, memory.param=NULL){
  restore.point("Extend.Memory.Qlearning")
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
  minibatch.no <- sample(1:length(algo.var$memory), batch.size)

  minibatch <- algo.var$memory[minibatch.no]
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
    Q_sa_old <- model.par$predict(model,x_train)
    Q_sa_next <- apply(model.par$predict(model,x_next_state), 1, FUN=max)
    #Go through all given that action was taken
    y_train <- t(sapply(1:length(action),FUN=function(x){
      restore.point("y_train.intern")
      res <- Q_sa_old[x,]
      if(done[x]){
        res[action[x]] <- reward[x]
      } else {
        res[action[x]] <- (1-algo.par$a)*Q_sa_old[x,action[x]] + algo.par$a * (reward[x] + algo.par$gamma*Q_sa_next[x]) #normal update "Q learning
      }
      return(res)
    }))

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
    algo.var$cur.loss <- mean(fit.obj$metrics$loss)

    if(algo.par$action.policy=="naive.info.treatment" && i==algo.par$replay.intensive){
      #In last round, update Loss per Data point
      predict.diff <- abs(model.par$predict(model,x_train) - y_train)
      print(paste0("mean predict.diff in round ",i,": ",mean(predict.diff)))
      algo.var$memory <- lapply(1:length(algo.var$memory), FUN=function(x){
        if(x %in% minibatch.no){ #update
          adress <- which(minibatch.no %in% x)
          minibatch.no[adress]
          res <- algo.var$memory[[x]]
          res$surprise <- predict.diff[adress,action[adress]]
          return(res)
        } else { #no update
          return(algo.var$memory[[x]])
        }
      })
    }
  }

  if(algo.par$action.policy=="epsilon.greedy" && algo.var$epsilon > algo.par$epsilon.min){
    algo.var$epsilon <- algo.par$epsilon.decay*algo.var$epsilon
  }

  if(algo.par$action.policy=="naive.info.treatment"){
    #update/build visited
    x_train <- t(sapply(algo.var$memory, FUN=function(x){
      return(x$state)
    }))
    action <- sapply(algo.var$memory, FUN=function(x){
      return(x$action)
    })
    #Not very efficient - find better method?
    comb <- cbind(x_train,action)
    visited <- aggregate(numdup ~., data=transform(comb,numdup=1), length)
    comb.id <- c(apply(comb,MARGIN=1,FUN=function(x){
      return(paste0(x,collapse=""))
    }))
    comb <- data.table(comb,id=comb.id)
    visited.id <- c(apply(visited,MARGIN=1,FUN=function(x){
      return(paste0(x[-length(x)],collapse=""))
    }))
    visited <- data.table(visited,id=visited.id)
    comb <- merge(comb,visited,by="id")

    algo.var$memory <- lapply(1:length(algo.var$memory), FUN=function(x){
      res <- algo.var$memory[[x]]
      res$visited <- comb$numdup[x]
      return(res)
    })
  }
  return(nlist(model, algo.var))
}

#' Determines which action to take
#'
#' @export
Act.Qlearning <- function(state, model, model.par, algo.var, game.object, eval.only=FALSE){
  restore.point("Act.Qlearning")

  if(eval.only){
    act.values <- model.par$predict(model,state)
    return(which.is.max(act.values))
  }

  if(algo.par$action.policy=="epsilon.greedy"){
    if(runif(1) <= algo.var$epsilon){
      game.par <- game.object$game.par(game.object)
      return(sample(1:game.par$output.nodes,1))
    } else {
      act.values <- model.par$predict(model,state)
      return(which.is.max(act.values))
    }
  } else if(algo.par$action.policy=="naive.info.treatment"){
    #Find Qa pair for all fitting for Q
    game.par <- game.object$game.par(game.object)
    found.in.mem <- unlist(sapply(1:game.par$output.nodes,FUN=function(x){
      for(i in 1:length(algo.var$memory)){
        if(identical(state,algo.var$memory[[i]]$state) && algo.var$memory[[i]]$action==x){
          return(i)
          break
        }
      }
      return(NA)
    }))
    if(any(is.na(found.in.mem))){ #if we have no experience of some Qa pairs, do the best you can out of those
      act.values <- model.par$predict(model,state)
      out.of <- is.na(found.in.mem)
      act.values[!out.of] <- -Inf
      return(which.is.max(act.values))
    }
    #Build naive criterion
    act.values <- model.par$predict(model,state)
    info.values <- sapply(1:length(act.values),FUN=function(x){
      if(!is.null(algo.var$memory[[found.in.mem[x]]]$surprise) && !is.null(algo.var$memory[[found.in.mem[x]]]$visited)){
        res <- act.values[x] + algo.var$exploration.affinity * algo.var$memory[[found.in.mem[x]]]$surprise / sqrt(algo.var$memory[[found.in.mem[x]]]$visited)
        return(res)
      } else { #it is new -> better not choose
        return(-Inf)
      }
    })
    return(which.is.max(info.values))
  }

  stop("Wrong action policy specified in algo.par.")
}

#' Train a model based on Q-Learning
#'
#' @export
Train.QLearning <- function(model, model.par, algo.par, algo.var, game.object, episodes, eval.only=FALSE, start.w.training=TRUE){
  score.array <- NA
  if(is.null(algo.var$analysis)){
    algo.var$analysis <- list()
    algo.var$analysis$score <- NA
  }

  if(length(algo.var$memory)>0 && start.w.training && !eval.only){
    replay.res <- Replay.Qlearning(model, model.par, algo.par, algo.var)
    model <- replay.res$model
    algo.var <- replay.res$algo.var
  }

  for(i in 1:episodes){
    restore.point("within.Train.Qlearning")
    state <- game.object$start.state(game.object)
    if(!is.null(algo.var$exploration.affinity) && algo.par$lin.falling){
      algo.var$exploration.affinity <- algo.par$exploration.affinity*(episodes-i)/episodes
    }
    while(TRUE){
      restore.point("within.Train.Qlearning.II")
      vis.state <- t(game.object$state.2.array(game.state=state, game.object=game.object)) # not a real state but what the algorithm sees. Could be a lot smaller than the real game state [but might depend on encoding]

      action <- Act.Qlearning(state=vis.state, model=model, model.par=model.par, algo.var=algo.var, game.object=game.object, eval.only=eval.only)

      next.state.full <- game.object$state.transition(game.state=state,action=action,game.object=game.object)

      next.state <- next.state.full$next.state
      reward <- next.state.full$reward
      done <- next.state.full$game.finished

      if(algo.par$mem.selection=="all" || (algo.par$mem.selection=="end.state" && done)){
        if(algo.par$mem.type=="game.encoded"){
          algo.var$memory[[length(algo.var$memory)+1]] <- list(state=vis.state, action=action, next.state=t(game.object$state.2.array(game.state=next.state, game.object=game.object)), reward=reward, done=done)
        } else if (algo.par$mem.type=="game.state"){
          algo.var$memory[[length(algo.var$memory)+1]] <- list(state=state, action=action, next.state=next.state, reward=reward, done=done)
        }
      }

      if(done){
        break
      }
      state <- next.state
    }
    score.array[i] <- algo.var$memory[[length(algo.var$memory)]]$reward #Not used for learning, only for analysis
    if(is.na(algo.var$analysis$score[1])){
      algo.var$analysis$score[1] <- score.array[i]
    } else {
      algo.var$analysis$score[length(algo.var$analysis$score)+1] <- score.array[i]
    }

    if(length(algo.var$memory)>algo.par$max.mem){
      algo.var$memory <- algo.var$memory[-c(sample(1:length(algo.var$memory),round(length(algo.var$memory)*algo.par$remove.memory)))]
    }

    if(i%%algo.par$replay.every == 0 && !eval.only){
      replay.res <- Replay.Qlearning(model, model.par, algo.par, algo.var)
      model <- replay.res$model
      algo.var <- replay.res$algo.var
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


