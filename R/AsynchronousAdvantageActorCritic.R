Get.Def.Par.A3C <- function(){

   Return.Type <- "N.Steps"   #Either "GAE" or "N.Steps" are supported. "GAE" := General Advantage Estimation
   N.Steps.Max <- 5
   gamma.return <- 0.9
   lambda.advantage <- 0.9

   N.Worker <- availableCores()-1

   show.current.status <- 20

   Full.Output<- TRUE
   Transfer.Output.every <- 1
   max.mem.per.worker <- 1000 # How much episodes should be stored per worker.

   use.egreedy.per.worker <- c(FALSE)   # defaults to False @ every worker.
   epsilon.start <- 0.5
   epsilon.decay <- 0.95
   epsilon.min <- 0

   save.Master.Network <- TRUE
   save.every <- 1 #Set this number equal to Max.Episodes to only store the weights of the last (fully trained) Master Network.
   save.path <- getwd()

   A3C.param <- nlist(Return.Type,N.Steps.Max, gamma.return, lambda.advantage,N.Worker,show.current.status,Full.Output,Transfer.Output.every,max.mem.per.worker,use.egreedy.per.worker,epsilon.start,epsilon.decay,epsilon.min,save.Master.Network, save.every, save.path)

   return(A3C.param)


}

Initialise.A3C <- function (algo.par){
  restore.point("Initialise.A3C")
  algo.var.Master <- replicate(algo.par$N.Worker,list(), simplify = "array")
  algo.var.Master <- lapply(1:algo.par$N.Worker, function(x) {algo.var.Master[[x]]$memory <- list()
                                                              algo.var.Master[[x]]$Graph.Output <- list()
                                                              return(algo.var.Master[[x]])})
  algo.var.Master$global.score <- vector("numeric")

  algo.var.Worker <- list()
  algo.var.Worker$memory <- list()
  algo.var.Worker$Graph.Output <- list()
  return(nlist(algo.var.Master,algo.var.Worker))

}

Train.A3c <- function(model.par, algo.par, algo.var, game.object, episodes.max){

    restore.point( "Train.A3C")

    print("Starting set-up of cluster...")

    threads <- vector(mode = "list" , length = algo.par$N.Worker)
    threads.index <- seq(1,algo.par$N.Worker)
    Threads.2.start = length(threads.index)
    cl <- makeClusterPSOCK(Threads.2.start)
    plan(cluster, workers = cl)

    algo.var.Master <- algo.var$algo.var.Master
    algo.var.Worker <- algo.var$algo.var.Worker

    if(length(algo.par$use.egreedy.per.worker)<algo.par$N.Worker) algo.par$use.egreedy.per.worker <- c(algo.par$use.egreedy.per.worker,rep(FALSE,times=(algo.par$N.Worker-length(algo.par$use.egreedy.per.worker))))

    clusterExport(cl = cl,varlist =   c("Setup.Neural.Network.A3C","Worker.A3C","Convert.2.train","Advantage.function","Act.A3C", "Define_Graph", "Initialise.A3C")) # line is not needed if functions are integrated in package.
    clusterExport(cl = cl, c("game.object","model.par", "algo.par","algo.var.Worker"), envir = environment())

    clusterEvalQ(cl = cl, {library(keras)               #Set up workers.
                           library(ReinforcementLearningwithR)
                           library(tensorflow)

                           set.storing(storing = FALSE)

                           sess <- tf$Session()
                           K <- backend()
                           K$set_session(session = sess)
                           K$manual_variable_initialization(TRUE)

                           model <- model.par$setup(model.par = model.par,game.par = game.object$game.par(game.object))

                           Graph <- Define_Graph(model = model,model.par = model.par, game.object = game.object)
                           s_ <- Graph$s_
                           a_ <- Graph$a_
                           r_ <- Graph$r_
                           adv_ <- Graph$adv_

                           sess$run(tf$global_variables_initializer())

                           Worker.Episode <- 0

    })

    print("Cluster set-up finished.")
    Master.sess <- tf$Session()
    K <- backend()
    K$set_session(session = Master.sess)
    K$manual_variable_initialization(TRUE)

    Master.Network <- model.par$setup(model.par = model.par, game.par = game.object$game.par(game.object))

    Gradient_Update <- Define_Graph_Gradient_Update(Master.Network)
    tuple.pl = tuple(Gradient_Update$vars.grads.pl) #Used to identify PL?s as key in dictionary.

    Master.sess$run(tf$global_variables_initializer())

    global.episode <- 1
    version <- 1
    threads <- lapply(1:Threads.2.start, function(i){
                                          future::cluster({Worker.A3C(worker.number=i,game.state = NULL, algo.par= algo.par, algo.var.worker = algo.var.Worker, model.par = model.par, game.object = game.object, weights = NULL, Worker.Episode=Worker.Episode)},
                                          persistent = TRUE, workers = cl[[threads.index[i]]], globals = c("i"))
    })

    while(TRUE){

      while(!any(resolved(threads))){

        Sys.sleep(0.1)

      }

      threads.index <- resolved(threads)
      threads.resolved <- threads[threads.index]
      threads.index <- which(threads.index)

      threads.value <- values(threads.resolved)
      threads.value.training <- lapply(threads.value, "[[", "training")

      if(algo.par$Full.Output) threads.value.output <- lapply(threads.value, "[[", "algo.var.worker")

      for(index in 1:length(threads.value.training)){

          Master.sess$run(Gradient_Update$gradients.update, feed_dict = dict(tuple.pl=threads.value.training[[index]]))
          Weights.Transfer.2.Worker <- get_weights(Master.Network)
          threads[[threads.index[index]]] <- future::cluster({Worker.A3C(worker.number=worker.number,game.state = next.state.global, algo.par= algo.par, algo.var.worker = algo.var.Worker ,game.object = game.object, model.par = model.par, weights = Weights.Transfer.2.Worker,Worker.Episode=Worker.Episode)},
                                                         persistent = TRUE, workers = cl[[threads.index[index]]],globals = c("Weights.Transfer.2.Worker"))

          if(algo.par$Full.Output&&!is.null(threads.value.output[[index]])){

            algo.var.Master[[threads.index[index]]]$memory <- c(algo.var.Master[[threads.index[index]]]$memory, threads.value.output[[index]]$memory)
            algo.var.Master[[threads.index[index]]]$Graph.Output <- c(algo.var.Master[[threads.index[index]]]$Graph.Output, threads.value.output[[index]]$Graph.Output)
          }

        if(threads.value[[index]]$finished){
          algo.var.Master$global.score[global.episode] <- threads.value[[index]]$reward

          if(global.episode%%algo.par$show.current.status==0){
            output.message <- paste0(c("episode: ",global.episode," with avg score of ",round(mean(algo.var.Master$global.score[(global.episode-algo.par$show.current.status+1):global.episode]),2)),collapse="")
            print(output.message)
          }

          if(algo.par$save.Master.Network&&global.episode%%algo.par$save.every==0){
            Master.Network$save_weights(filepath = paste0(algo.par$save.path,"/Network_weights_Version_",version,".h5")) # model is saved in a seperate file.
            version = version+1
          }

          global.episode <- global.episode +1
        }

        if(global.episode>episodes.max) break

      }

      if(global.episode>episodes.max) break

    }

    parallel::stopCluster(cl = cl)
    tf$Session$close(Master.sess)

    return(nlist(algo.var.Master))

}


Act.A3C <- function(state, model, model.par,algo.par, game.object,worker.number,epsilon){

  game.par <- game.object$game.par(game.object)

  if(runif(1)<epsilon&&algo.par$use.egreedy.per.worker[worker.number]){
    return(sample.int(n= game.par$output.nodes, size = 1))

  }else{

    p <- model.par$predict(model,state,"Prob")
    return(sample.int(n= game.par$output.nodes, size = 1, prob = p))
  }
}

Advantage.function <- function(Memory, model, model.par,algo.par, done, state, game.object){

  if(done == FALSE){
     R <- model.par$predict(model, t(game.object$state.2.array(game.state = state, game.object=game.object)), "Value")
     Memory$r[length(Memory$r)] <- Memory$r[length(Memory$r)] + algo.par$gamma.return*R
  }

  V_t <- model.par$predict(model,Memory$s,"Value")

  if(algo.par$Return.Type =="GAE"){
    TD.Error <- Memory$r[1:(length(Memory$r)-1)]+V_t[2:length(V_t)]*algo.par$gamma.return-V_t[1:(length(V_t)-1)]
    TD.Error[length(TD.Error)+1] <- Memory$r[length(Memory$r)]-V_t[length(V_t)]
    rev.TD.Error <- rev(TD.Error)
    rev.advantage <- as.vector(stats::filter(x = rev.TD.Error, filter = algo.par$gamma.return*algo.par$lambda.advantage , method = "recursive"))
    advantage = rev(rev.advantage)
  }

  rev.return <- rev(Memory$r)
  rev.n.return <- as.vector(stats::filter(x = rev.return, filter = algo.par$gamma.return, method = "recursive"))
  n.return <- rev(rev.n.return)

  if(algo.par$Return.Type =="N.Steps") advantage <- n.return - V_t

  Memory$adv <- as.matrix(advantage)
  Memory$target_v <- as.matrix(n.return)

  return(Memory)

}

Convert.2.train <- function(Memory){

  state.array <- t(sapply(Memory,"[[", "state"))
  reward.vector <- t(sapply(Memory,"[[", "reward"))
  action.array <- t(sapply(Memory,"[[", "action"))

  return(nlist(s = state.array, r = reward.vector, a = action.array))

}

Worker.A3C <- function(worker.number,game.state,model.par,algo.par,algo.var.worker,game.object, weights, Worker.Episode){

  Worker.Memory <- list()
  Training.queue <- list()

  if(is.null(game.state)){

    state <- game.object$start.state(game.object)
    assign("worker.number",worker.number, envir = .GlobalEnv)
    assign("epsilon",algo.par$epsilon.start, envir = .GlobalEnv)

    }else if(game.state$game.finished){

      state <- game.object$start.state(game.object)
      set_weights(model, weights)

    }else{

      state <- game.state$next.state
      set_weights(model, weights)

  }

   End.act.round <- state$round + algo.par$N.Steps.Max

   while(state$round<End.act.round){

     vis.state <- t(game.object$state.2.array(game.state = state, game.object = game.object))
     action <- Act.A3C(state = vis.state, model = model,model.par = model.par,algo.par=algo.par, game.object = game.object, worker.number = worker.number, epsilon=epsilon)

     next.state.full <- game.object$state.transition(game.state=state,action=action,game.object=game.object)

     next.state <- next.state.full$next.state
     reward <- next.state.full$reward
     done <- next.state.full$game.finished

     action.one.hot <- rep(0,times = game.object$game.par(game.object)$output.nodes)
     action.one.hot[action] <- 1

     Worker.Memory[[length(Worker.Memory)+1]] <- list(state = vis.state, action = action.one.hot , reward = reward, done = done)

     if(done) break

     state <- next.state

   }


   assign("next.state.global", next.state.full, envir = .GlobalEnv)

   if(done){
     assign("Worker.Episode", Worker.Episode+1, envir = .GlobalEnv)
     if(algo.par$use.egreedy.per.worker[worker.number]&&epsilon>algo.par$epsilon.min){
     assign("epsilon", epsilon*algo.par$epsilon.decay, envir = .GlobalEnv)
     }
   }

   Training.queue <- Convert.2.train(Memory = Worker.Memory)
   Training.queue  <- Advantage.function(Memory = Training.queue, model = model, model.par = model.par,algo.par = algo.par, done = done, state = state, game.object=game.object)

   graph.output <- sess$run(list(Graph$gradient_clipped,Graph$loss.policy,Graph$loss.value, Graph$loss), feed_dict = dict(s_ = Training.queue$s,r_ = Training.queue$target_v,a_ = Training.queue$a, adv_ = Training.queue$adv))
   names(graph.output) <- c("gradient","loss.policy","loss.value","loss")
   acc.gradients <- graph.output$gradient
   graph.output[[1]] <- NULL

   ##not supported atm
   # for(round in 1:length(Worker.Memory)){
   #
   #   gradients = sess$run(Graph$gradient, feed_dict = dict(s_ = Training.queue$s[round,,drop =FALSE],r_ = Training.queue$n.r[round,,drop = FALSE],a_ = Training.queue$a[round,,drop = FALSE]))
   #
   #   if(round==1){
   #
   #     acc.gradients = gradients
   #
   #   }else{
   #
   #   acc.gradients = lapply(seq_along(gradients),function(x){gradients[[x]]+acc.gradients[[x]]})
   #
   #   }
   # }

   if(algo.par$Full.Output&&Worker.Episode<algo.par$max.mem.per.worker){

      algo.var.worker$Graph.Output[[length(algo.var.worker$Graph.Output)+1]] <- graph.output
      algo.var.worker$memory = c(algo.var.worker$memory,Worker.Memory)

      if(Worker.Episode%%algo.par$Transfer.Output.every==0&&done){

        assign("algo.var.Worker", Initialise.A3C(algo.par = algo.par)$algo.var.Worker, envir = .GlobalEnv)

        return(list(training = acc.gradients, reward = Worker.Memory[[length(Worker.Memory)]]$reward, finished = done, algo.var.worker=algo.var.worker))

      }else{

        assign("algo.var.Worker", algo.var.worker, envir = .GlobalEnv)  #preserve state if Output is not pushed 2 Master.

       }

   }

   return(list(training = acc.gradients, reward = Worker.Memory[[length(Worker.Memory)]]$reward, finished = done, algo.var.worker = NULL))

}


Get.Def.Par.Neural.Network.A3C <- function(){

  name <- "A3C.Neural.Network"

  setup <- Setup.Neural.Network.A3C
  predict <- Predict.Neural.Network.A3C

  hidden.nodes <- c("10", "10", "10")
  activation.hidden <- c("relu", "relu", "relu")
  activation.output <- c("softmax", "linear")

  c.value <- 0.5
  c.entropy <- 0.02

  Optimizer <- tf$train$AdamOptimizer(learning_rate = 0.005)
  decay <- 0.99

  clip.norm = 40

  return(nlist(name,setup,predict,hidden.nodes,activation.hidden,activation.output,c.value,c.entropy, Optimizer, clip.norm))

}

Setup.Neural.Network.A3C <- function(model.par, game.par){

  model_seq <- keras_model_sequential()

  for(i in 1:length(model.par$hidden.nodes)){

    if(i==1){

      model_seq %>% layer_dense(units = model.par$hidden.nodes[i], activation = model.par$activation.hidden[i], input_shape =c(game.par$input.nodes))

    }else{

      model_seq %>% layer_dense(units = model.par$hidden.nodes[i], activation = model.par$activation.hidden[i])

    }

  }

  inputs <- layer_input(shape = c(game.par$input.nodes))
  input_seq <- inputs%>%model_seq

  P.out <- input_seq%>%layer_dense(units = game.par$output.nodes ,activation = model.par$activation.output[1])
  V.out <- input_seq%>%layer_dense(units = 1 ,activation = model.par$activation.output[2])

  model <- keras_model(inputs = inputs, outputs = c(P.out,V.out))

}

Predict.Neural.Network.A3C <- function(model,state,Indicator = NULL){

  p_v = model %>% predict(state)

  if(is.null(Indicator)){

    return(p_v)

   }else if(Indicator == "Value"){

     return(p_v[[2]])

    }else if(Indicator == "Prob"){

     return(p_v[[1]])
  }
}

Define_Graph <- function(model, model.par, game.object){

  s_ <- tf$placeholder(tf$float32, shape(NULL,game.object$game.par(game.object)$input.nodes))
  a_ <- tf$placeholder(tf$float32, shape(NULL, game.object$game.par(game.object)$output.nodes))
  r_ <- tf$placeholder(tf$float32, shape(NULL, 1))
  adv_ <- tf$placeholder(tf$float32, shape(NULL, 1))

  p_V <- model(s_)
  p <- p_V[[1]]
  V <- p_V[[2]]

  log.p <- log(tf$reduce_sum(p*a_, axis = 1L, keep_dims = TRUE))

  TD.error <- r_ - V

  loss.policy <- -tf$reduce_mean(log.p*adv_,reduction_indices = NULL)
  loss.value <- tf$reduce_mean((TD.error^2),reduction_indices = NULL)
  entropy <- tf$reduce_mean(tf$reduce_sum(p * log(p+1e-10), axis = 1L, keep_dims = TRUE),reduction_indices = NULL)

  loss <- loss.policy+model.par$c.value*loss.value+model.par$c.entropy*entropy
  Optimizer <- tf$train$AdamOptimizer(learning_rate = model.par$learn.rate)

  train_vars <- model$trainable_weights
  gradient_weights <- Optimizer$compute_gradients(loss, train_vars)
  gradient <- lapply(gradient_weights,"[[",1)
  gradient_clipped <- tf$clip_by_global_norm(gradient,model.par$clip.norm)[[1]]


  return(nlist(gradient_clipped,loss.policy,loss.value,loss,s_,a_,r_,adv_))

}


Define_Graph_Gradient_Update<- function(model){

  tvs <- model$trainable_weights

  vars.grads <- lapply(tvs,function(x){tf$Variable(tf$zeros_like(x$initialized_value()),trainable = FALSE)})
  vars.grads.pl <- lapply(vars.grads,function(x){tf$placeholder(dtype = tf$float32, shape = x$get_shape())})

  gradients.new <- mapply(function(x,y){tf$assign(x,y)},vars.grads,vars.grads.pl)
  gradients.weights.new  <-   mapply(function(x,y){list(list(x,y))},gradients.new,tvs)
  gradients.update <- model.par$Optimizer$apply_gradients(grads_and_vars = gradients.weights.new)

  return(nlist(gradients.update, vars.grads.pl))

}




