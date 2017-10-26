#Q-Learning with Discrete Choices
#' Sets up a model based on model parameters
#'
#' @export
Setup.QLearning <- function(game.object, model.par=NULL){
  restore.point("Setup.QLearning")
  if(is.null(model.par)){
    model.par <- Get.Def.Par.QLearning()
  }

  game.par <- game.object$game.par(game.object)

  model <- keras_model_sequential()

  for(i in 1:length(model.par$hidden.nodes)){
    if(i==1){
      model %>%
        layer_dense(units = model.par$hidden.nodes[i], input_shape = game.par$input.nodes) %>%
        layer_activation(activation = model.par$activation.hidden[i])
    } else {
      model %>%
        layer_dense(units = model.par$hidden.nodes[i], input_shape = model.par$hidden.nodes[i-1]) %>%
        layer_activation(activation = model.par$activation.hidden[i])
    }
  }

  model %>%
    layer_dense(units = game.par$output.nodes) %>%
    layer_activation(activation = model.par$activation.output)

  model %>% compile(
    loss = model.par$loss,
    optimizer = model.par$optimizer
  )

  return(model)
}

#' Set changeable model variables e.g. epsilon
#'
#' @param model.par Parameters of QLearning specification.
#'@export
Initialise.Qlearning <- function(model.par){
  model.var <- list()
  model.var$epsilon <- model.par$epsilon.start
  model.var$memory <- list() #items: state, action, reward
  return(model.var)
}

#' Train NN of Q learning
#'
#' @export
Replay.Qlearning <- function(model, model.var, model.par, batch.size){
  if(length(model.var$memory)<batch.size){
    batch.size <- length(model.var$memory)
  }
  minibatch <- sample(model.var$memory, batch.size)
  #Transform dataset to trainable matrix
  traindata <- lapply(minibatch, FUN=function(x){
    target <- x$reward
    if(!x$done){
      pred <- model %>% predict(x$next.state)
      target <- x$reward + model.par$gamma*max(pred)
    }
    target_f <- model %>% predict(x$state)
    target_f[x$action] <- target
    return(list(x_train=x$state, y_train=target_f))
  })
  x_train <- t(sapply(1:length(traindata),FUN=function(x){
    return(traindata[[x]]$x_train)
  }))
  y_train <- t(sapply(1:length(traindata),FUN=function(x){
    return(traindata[[x]]$y_train)
  }))

  fit(model,x_train, y_train, epochs = model.par$epochs, verbose=0)
  if(model.var$epsilon > model.par$epsilon.min){
    model.var$epsilon <- model.par$epsilon.decay*model.var$epsilon
  }
  return(list(model=model, model.var=model.var))
}

#' Determines which action to take
#'
#' @export
Act.Qlearning <- function(state, model, model.var, game.object){
  if(runif(1)<= model.var$epsilon){
    game.par <- game.object$game.par(game.object)
    return(sample(1:game.par$output.nodes,1))
  }
  restore.point("within.Act.Qlearning")
  act.values <- predict(model,state)
  return(which.max(act.values))
}

#' Train a model based on Q-Learning
#'
#' @export
Train.QLearning <- function(model, model.var, model.par, episodes, game.object){
  score.array <- NA
  for(i in 1:episodes){
    restore.point("within.Train.Qlearning")
    state <- game.object$start.state(game.object)
    while(TRUE){
      vis.state <- t(game.object$state.2.array(game.state=state, game.object=game.object)) # not a real state but what the algorithm sees. Could be a lot smaller than the real game state [but might depend on encoding]

      action <- Act.Qlearning(state=vis.state, model=model, model.var=model.var, game.object=game.object)
      next.state.full <- game.object$state.transition(game.state=state,action=action,game.object=game.object)
      next.state <- next.state.full$next.state
      reward <- next.state.full$reward
      done <- next.state.full$game.finished

      model.var$memory[[length(model.var$memory)+1]] <- list(state=vis.state, action=action, next.state=t(game.object$state.2.array(game.state=next.state, game.object=game.object)), reward=reward, done=done)
      if(done){
        break
      }
      state <- next.state
    }
    score.array[i] <- model.var$memory[[length(model.var$memory)]]$reward

    if(length(model.var$memory)>model.par$max.mem){
      model.var$memory <- model.var$memory[-c(sample(1:length(model.var$memory),round(length(model.var$memory)/10)))]
    }

    if(i%%model.par$replay.every == 0){
      replay.res <- Replay.Qlearning(model, model.var, model.par, model.par$batch.size)
      model <- replay.res$model
      model.var <- replay.res$model.var
    }

    if(i%%model.par$show.current.status == 0){
      output.message <- paste0(c("episode: ",i," with avg score of ",round(mean(score.array[(i-model.par$show.current.status+1):i]),2)),collapse="")
      print(output.message)
    }
  }
  return(list(model=model, model.var=model.var))
}

#' Delivers some default Parameters of Q-learning
#'
#' @export
Get.Def.Par.QLearning <- function(){
  hidden.nodes <- c(15,5)
  activation.hidden <- c("relu","relu")
  activation.output <- c("linear")
  loss <- "mse"
  optimizer <- optimizer_adam(lr=0.05)
  epsilon.start <- 1
  epsilon.decay <- 0.95
  epsilon.min <- 0.02
  batch.size <- 1000
  max.mem <- 1000000
  gamma <- 0.99
  show.current.status <- 100
  replay.every <- 50
  epochs <- 100
  q.param <- list(hidden.nodes=hidden.nodes,activation.hidden=activation.hidden,activation.output=activation.output,loss=loss,optimizer=optimizer,epsilon.start=epsilon.start,epsilon.decay=epsilon.decay,epsilon.min=epsilon.min,batch.size=batch.size,max.mem=max.mem, gamma=gamma, show.current.status=show.current.status, replay.every=replay.every, epochs=epochs)
  return(q.param)
}
