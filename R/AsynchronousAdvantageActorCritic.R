#' Get Default Parameters of A3C.
#'
#' Returns a list with the following items \itemize{
#'  \item \strong{Return Typ} Specifies how  returns are are modified for the gradient calculations. This should be either N.Steps or GAE.
#'  \item \strong{N.Steps.Max} How many rounds should be played before a weight update. Also specifies the number of actual returns before bootstrapping.
#'  \item \strong{gamma.return} Discount Factor used for discounting future returns.
#'  \item \strong{lambda.advantage} [only used in case of GAE] General Advantage Estimator.
#'  \item \strong{N.Worker} Sets the number of parallel agents.     
#'  \item \strong{show.current.status} How often should the mean score be calculated and displayed.
#'  \item \strong{Full.Output} If set to TRUE the memory of the worker is pushed to the master.
#'  \item \strong{Transfer.Output.every} For performance reasons this should not be to low and for memory limitations this should not be to high.
#'  \item \strong{max.mem.per.worker} How many Episodes should be stored for each worker.
#'  \item \strong{use.e.greedy.per.worker} Should be a boolean vector e.g. c(TRUE,TRUE) specifying the workers which use egreedy-exploration strategy.
#'  \item \strong{epsilon.start} Starting Value for epsilon. Higher Values encourage exploration.
#'  \item \strong{epsilon.decay} Specifies a linear decay for the value of epsilon.
#'  \item \strong{epsilon.min} Lower boundarie for the value of epsilon.
#'  \item \strong{save.Master.Network} If set to TRUE weights of the Master Network are saved at the location given by save.path.
#'  \item \strong{save.every} Specifies the number of episodes after which an additional parameter set should be stored.
#'  \item \strong{save.path} File Path to store and load weights of a Network. Defaults to the working directory.
#'  \item \strong{upload.file.name} Name of the file to upload weights. Whole file path consists \code{save.path} and \code{upload.file.name}.
#'  \item \strong{shiny.visualize} If TRUE the memory of the first worker is prepared to be visualized in shiny.
#'  \item \strong{shiny.append.file} If FALSE shiny data will be pushed to the shiny app at playing time. If FALSE a large file is created which can be uploaded to the shiny app.
#'  \item \strong{train} If FALSE  no training is taking place. Instead network weights are loaded from the location given by save.path.
#'  
#' }
#'@export

Get.Def.Par.A3C <- function(){
  
  Return.Type <- "GAE"   #Either "GAE" or "N.Steps" are supported. "GAE" := General Advantage Estimation.
  N.Steps.Max <- 10
  gamma.return <- 0.9
  lambda.advantage <- 0.9
  
  N.Worker <- availableCores()-1
  
  show.current.status <- 20
  Full.Output<- TRUE
  Transfer.Output.every <- 10
  max.mem.per.worker <- 1000 # How much episodes should be stored per worker.
  
  use.egreedy.per.worker <- c(FALSE)   #Defaults to False @ every worker. Makes A3C off-policy, but still provides some additional exploration.
  epsilon.start <- 0.5
  epsilon.decay <- 0.95
  epsilon.min <- 0
  
  save.Master.Network <- TRUE
  save.every <- 500 #Set this number equal to Max.Episodes to only store the weights of the last (fully trained) Master Network.
  save.path <- getwd()
  upload.file.name <- "Network_Weights"
  
  shiny.visualize <- FALSE # Works only in combination with the IPD game.
  shiny.append.file <- FALSE   
  
  train <- TRUE
    
  A3C.param <- nlist(Return.Type,N.Steps.Max, gamma.return, lambda.advantage,N.Worker,show.current.status,Full.Output,Transfer.Output.every,max.mem.per.worker,use.egreedy.per.worker,epsilon.start,epsilon.decay,epsilon.min,save.Master.Network, save.every, save.path, upload.file.name, shiny.visualize, shiny.append.file, train)
  
  return(A3C.param)
  
}

#' Set changeable A3C Parameters.
#'
#' Returns a structured list of lists to store the results of the learning process. \itemize{
#' \item \strong{algo.var.Master} Contains two lists for every worker. Memory stores the history of the worker and Graph.Output stores information of the gradient calulations. Additionally a global score is stored.
#' \item \strong{algo.var.worker} Contains two lists with the memory and the graph output for the given worker.
#' }
#'
#' @param algo.par A list as given by \code{Get.Def.Par.A3C}.
#'@export

Initialise.A3C <- function (algo.par){
  
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

#' Use the A3C algorithm to train a model 
#'
#' Returns a list as given by \code{Initialise.A3C} with stored experiences for each worker.
#'
#' @param model.par A list as given by \code{Get.Def.Par.Neural.Network.A3C} or \code{Get.Def.Par.Neural.Network.A3C.LSTM}.
#' @param algo.par A list as given by \code{Get.Def.Par.A3C}.
#' @param algo.var A list as given by \code{Initialise.A3C}.
#' @param game.object A Game Object (list) as defined by \code{Get.Game.Object.<NAME>}.
#' @param episodes.max Specifies how many episodes should be played in total.  
#'@export

Train.A3c <- function(model.par, algo.par, algo.var, game.object, episodes.max){
  
  restore.point( "Train.A3C")
  
  print("Starting setup of cluster...")
  
  threads <- vector(mode = "list" , length = algo.par$N.Worker)
  threads.index <- seq(1,algo.par$N.Worker)
  Threads.2.start = length(threads.index)
  cl <- makeClusterPSOCK(Threads.2.start)
  plan(cluster, workers = cl)
  
  if(!is.null(game.object$name)&&game.object$name=="Prisoners Dilemma"){
    choices <- c("DD","DC","CD","CC")
    actions <- c("C","D")
  }
  
  algo.var.Master <- algo.var$algo.var.Master
  algo.var.Worker <- algo.var$algo.var.Worker
  
  if(length(algo.par$use.egreedy.per.worker)<algo.par$N.Worker) algo.par$use.egreedy.per.worker <- c(algo.par$use.egreedy.per.worker,rep(FALSE,times=(algo.par$N.Worker-length(algo.par$use.egreedy.per.worker))))
  
  #clusterExport(cl = cl,varlist =   c("Setup.Neural.Network.A3C","Setup.Neural.Network.A3C.LSTM","redim.state","Worker.A3C","Convert.2.train","Advantage.function","Act.A3C", "Define_Graph", "Initialise.A3C", "Get.Game.Param.Hotelling", "Get.Par.Hotelling", "Generate.Start.State.Hotelling", "State.Transition.Hotelling", "Action.Encoding.Info.Hotelling", "State.2.Array.Hotelling", "Get.Game.Object.Hotelling")) # line is not needed if functions are integrated in package. 
  clusterExport(cl = cl, c("game.object","model.par", "algo.par","algo.var.Worker"), envir = environment())
  
  clusterEvalQ(cl = cl, { #Set up workers.
    
    library(keras)              
    library(ReinforcementLearningwithR)
    library(tensorflow)
    
    set.storing(storing = FALSE)   
    
    sess <- tf$Session()
    K <- backend()
    K$set_session(session = sess)
    K$manual_variable_initialization(TRUE)
    
    model <- model.par$setup(model.par = model.par,game.object = game.object)
    
    if(!algo.par$train){
      
      model$load_weights(paste0(algo.par$save.path,"/",algo.par$upload.file.name,".h5"))
      
    }else{
       
      sess$run(tf$global_variables_initializer())
      if(model.par$name == "A3C.Neural.Network.LSTM") model%>%predict(redim.state(t(c(rep(1,game.object$game.par(game.object)$input.nodes)))))
    
      Graph <- Define_Graph(model = model,model.par = model.par, game.object = game.object)
      s_ <- Graph$s_
      a_ <- Graph$a_
      r_ <- Graph$r_
      adv_ <- Graph$adv_
    
      sess$run(tf$global_variables_initializer())
    
    }
    
    Worker.Episode <- 1
    
  })
    
  print("Cluster setup finished.")
  
  Master.sess <- tf$Session()
  K <- backend()
  K$set_session(session = Master.sess)
  K$manual_variable_initialization(TRUE)
  
  Master.Network <- model.par$setup(model.par = model.par, game.object = game.object)
  
  if(!algo.par$train){
    Master.Network$load_weights(paste0(algo.par$save.path,"/Network_weights_Version_1.h5"))
  }else{
    Gradient_Update <- Define_Graph_Gradient_Update(Master.Network, model.par = model.par)
    tuple.pl = tuple(Gradient_Update$vars.grads.pl) #Used to identify PL´s as key in dictionary.
    Master.sess$run(tf$global_variables_initializer())
  }
  
  global.episode <- 1
  threads <- lapply(1:Threads.2.start, function(i){
    future::cluster({Worker.A3C(worker.number=i,game.state = NULL, algo.par= algo.par, algo.var.worker = algo.var.Worker, model.par = model.par, game.object = game.object, weights = NULL, Worker.Episode=Worker.Episode, model = model)},
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
      
      if(algo.par$train){
        Master.sess$run(Gradient_Update$gradients.update, feed_dict = dict(tuple.pl=threads.value.training[[index]]))
      }
      Weights.Transfer.2.Worker <- get_weights(Master.Network)
      threads[[threads.index[index]]] <- future::cluster({Worker.A3C(worker.number=worker.number,game.state = next.state.global, algo.par= algo.par, algo.var.worker = algo.var.Worker ,game.object = game.object, model.par = model.par, weights = Weights.Transfer.2.Worker,Worker.Episode=Worker.Episode, model = model)},
                                                         persistent = TRUE, workers = cl[[threads.index[index]]],globals = c("Weights.Transfer.2.Worker"))
      
      if(algo.par$Full.Output&&!is.null(threads.value.output[[index]])){
        
        algo.var.Master[[threads.index[index]]]$memory <- c(algo.var.Master[[threads.index[index]]]$memory, threads.value.output[[index]]$memory)
        algo.var.Master[[threads.index[index]]]$Graph.Output <- c(algo.var.Master[[threads.index[index]]]$Graph.Output, threads.value.output[[index]]$Graph.Output) 
        
        if(algo.par$shiny.visualize==TRUE&&threads.index[index]==1&&!is.null(game.object$name)&&game.object$name=="Prisoners Dilemma"){
          #Prepare data as matrix for fast read/write acces.
          shiny.data = prep.data.4.shiny(threads.value.output[[threads.index[index]]], choices, actions)
          
          if(algo.par$shiny.append.file){
            
            if(!exists("temp1")) temp1 <- matrix(ncol = ncol(shiny.data$memory.mat),nrow = 0)
            if(!exists("temp2")) temp2 <- matrix(ncol = ncol(shiny.data$graph.mat),nrow = 0)
            if(!exists("temp3")) temp3 <- matrix(ncol = ncol(shiny.data$help.data), nrow = 0)
            
            temp1 <- rbind(temp1, shiny.data$memory.mat)
            temp2 <- rbind(temp2, shiny.data$graph.mat)
            temp3 <- rbind(temp3, shiny.data$help.data)
          
           }else{
          
            dump("shiny.data", file = paste0(algo.par$save.path, "/shiny_data.R"), append = FALSE)
            
           }
        }
      }
      
      if(threads.value[[index]]$finished){
                                                                                                                                                                                                                                                                                                              
        algo.var.Master$global.score[global.episode] <- threads.value[[index]]$reward
        
        if(global.episode%%algo.par$show.current.status==0){
          output.message <- paste0(c("episode: ",global.episode," with avg score of ",round(mean(algo.var.Master$global.score[(global.episode-algo.par$show.current.status+1):global.episode]),2)),collapse="")
          print(output.message)
        }
        
        if(algo.par$save.Master.Network&&global.episode%%algo.par$save.every==0){
          Master.Network$save_weights(filepath = paste0(algo.par$save.path,"/Network_weights_Episode_",global.episode,".h5")) # model is saved in a seperate file. 
        }
        
        global.episode <- global.episode +1
      }
      
      if(global.episode>episodes.max) break
      
    }
    
    if(global.episode>episodes.max) break
    
  }
  
  if(algo.par$shiny.visualize&&algo.par$shiny.append.file&&!is.null(game.object$name)&&game.object$name=="Prisoners Dilemma"){
    
    temp4 <- list()
    temp4$game.pars <- game.object$game.pars
    temp4$algo.par <- algo.par
    
    shiny.data <- list(memory.mat = temp1, graph.mat = temp2, help.data = temp3, meta.data =  temp4)    
    dump("shiny.data", file = paste0(algo.par$save.path, "/shiny_data.R"), append = FALSE)
    
  }
  
  parallel::stopCluster(cl = cl)
  tf$Session$close(Master.sess)
  
  return(nlist(algo.var.Master))
  
}

#' Determines which action the algorithm takes
#'
#' Internal function which is called from \code{Worker.A3C}.
#'
#' @param state Encoded game state as given by \code{state.2.array}
#' @param model A model as given by \code{Setup.Neural.Network.A3C} or \code{Setup.Neural.Network.A3C.LSTM}
#' @param model.par A list as given by \code{Get.Def.Par.Neural.Network.A3C} or \code{Get.Def.Par.Neural.Network.A3C.LSTM}
#' @param game.object A Game Object (list) as defined by \code{Get.Game.Object.<NAME>}.
#' @param worker.number A number to identify the worker.   
#' @param epsilon Current Value of epsilon.
#'@export

Act.A3C <- function(state, model, model.par,algo.par, game.object,worker.number,epsilon){
  
  game.par <- game.object$game.par(game.object)
  game.pars <- game.object$game.pars
  
  if(!is.null(game.pars$action.space)&&game.pars$action.space == "continous"){
    
    mean_sd <- as.vector(model.par$predict(model,model.par,state,"Prob"))
    mean <- mean_sd[1:(length(mean_sd)-1)] 
    sd <- mean_sd[length(mean_sd)]
    return(nlist(action = round(rnorm(n = game.par$output.nodes, mean = mean, sd = sd),2)))
  }
  
  if(runif(1)<epsilon&&algo.par$use.egreedy.per.worker[worker.number]){
    return(nlist(action = sample.int(n= game.par$output.nodes, size = 1), prob = p))
    
  }else{
    
    p <- as.vector(model.par$predict(model,model.par,state,"Prob"))
    return(nlist(action = sample.int(n= game.par$output.nodes, size = 1, prob = p), prob = p))
  }
}

#' Calculates N-Step Returns or weighted Temporal Difference Errors
#'
#' Internal function called from \code{Worker.A3C}
#'
#' @param state Actual game state as returned by \code{State.Transition.<NAME>}.
#' @param model A model as given by \code{Setup.Neural.Network.A3C} or \code{Setup.Neural.Network.A3C.LSTM}.
#' @param model.par A list as given by \code{Get.Def.Par.Neural.Network.A3C} or \code{Get.Def.Par.Neural.Network.A3C.LSTM}.
#' @param algo.par  A list as given by \code{Get.Def.Par.A3C}.
#' @param game.object  A Game Object (list) as defined by \code{Get.Game.Object.<NAME>}.
#' @param queue Game history of the last n rounds given by \code{Convert.2.train}.
#' @param done Boolean specifiying wether actual episode is ended. Determines if a Bootstrap Value has to be calculated.
#'@export

Advantage.function <- function(state, model, model.par, algo.par, game.object, queue,done){
  
  if(done == FALSE){
    
    R <- as.vector(model.par$predict(model, model.par, t(game.object$state.2.array(game.state = state, game.object=game.object)), "Value"))
    queue$r[length(queue$r)] <- queue$r[length(queue$r)] + algo.par$gamma.return*R
  }
  
  V_t <- as.vector(model.par$predict(model,model.par,queue$s,"Value"))
  
  if(algo.par$Return.Type =="GAE"&&length(queue$r)>1){
    TD.Error <- queue$r[1:(length(queue$r)-1)]+V_t[2:length(V_t)]*algo.par$gamma.return-V_t[1:(length(V_t)-1)]
    TD.Error[length(TD.Error)+1] <- queue$r[length(queue$r)]-V_t[length(V_t)]
    rev.TD.Error <- rev(TD.Error)
    rev.advantage <- as.vector(stats::filter(x = rev.TD.Error, filter = algo.par$gamma.return*algo.par$lambda.advantage , method = "recursive"))
    advantage = rev(rev.advantage)
  }
  
  rev.return <- rev(queue$r)
  rev.n.return <- as.vector(stats::filter(x = rev.return, filter = algo.par$gamma.return, method = "recursive"))
  n.return <- rev(rev.n.return)
  
  if(algo.par$Return.Type =="N.Steps"||length(queue$r)==1) advantage <- n.return - V_t
  
  queue$adv <- as.matrix(advantage)
  queue$target_v <- as.matrix(n.return)
  
  return(queue)
  
}

#'Converts stored Memory into arrays.
#'
#' Internal function which is called from \code{Worker.A3C}.
#' 
#' @param Memory Stored History of Worker from last n rounds.
#'@export   

Convert.2.train <- function(Memory){
  
  state.array <- t(sapply(Memory,"[[", "state"))
  reward.vector <- t(sapply(Memory,"[[", "reward"))
  action.array <- t(sapply(Memory,"[[", "action"))
  
  return(nlist(s = state.array, r = reward.vector, a = action.array))
  
}

#'Defines an Agent based on the A3C-Algorithm
#'
#' Internal function which is called from \code{Train.A3C}.
#'
#' Returns a list with the following items \itemize{
#' \item \strong{training} A list containing calculated gradients.
#' \item \strong{reward} Last obtained reward during the current n-steps.
#' \item \strong{finished} Boolean specifiying wether episode is finished or not.
#' \item \strong{algo.var.Worker} List containing the stored experience of the given worker.
#'}  
#'
#' @param worker.number Number to identify the given worker.
#' @param game.state Game State from the last round.
#' @param model.par A list as given by \code{Get.Def.Par.Neural.Network.A3C} or \code{Get.Def.Par.Neural.Network.A3C.LSTM}.
#' @param algo.par A list as given by \code{Get.Def.Par.A3C}.
#' @param algo.var.worker A list as given by \code{Initialise.A3C}.
#' @param game.object A Game Object (list) as defined by \code{Get.Game.Object.<NAME>}.
#' @param weights Network weights from the Master Network.
#' @param Worker.Episode The actual Episode of the Worker.
#' @param model A Model as given by \code{Setup.Neural.Network.A3C} or \code{Setup.Neural.Network.A3C.LSTM}
#'@export 

Worker.A3C <- function(worker.number,game.state,model.par,algo.par,algo.var.worker,game.object, weights, Worker.Episode, model){
  
  Worker.Memory <- list()
  
  if(is.null(game.state)){
    
    state <- game.object$start.state(game.object)
    assign("worker.number",worker.number, envir = .GlobalEnv)
    assign("epsilon",algo.par$epsilon.start, envir = .GlobalEnv)
    assign("reward.vec.global", vector("numeric"), envir = .GlobalEnv)
    if(!algo.par$train&&model.par$name == "A3C.Neural.Network.LSTM") model$layers[[3]]$reset_states()
    
  }else if(game.state$game.finished){
    
    state <- game.object$start.state(game.object)
    set_weights(model, weights)
    if(model.par$name == "A3C.Neural.Network.LSTM") model$layers[[3]]$reset_states()
    
  }else{ 
    
    if(model.par$name == "A3C.Neural.Network.LSTM"&&model.par$reset.cell.state.every == "N.Steps") model$layers[[3]]$reset_states()
    state <- game.state$next.state
    set_weights(model, weights)
    
  }
  
  End.act.round <- state$round + algo.par$N.Steps.Max
  
  if(model.par$name == "A3C.Neural.Network.LSTM") lstm.cell.states <- sess$run(model$layers[[3]]$states)
  
  while(state$round<End.act.round){
    
    vis.state <- t(game.object$state.2.array(game.state = state, game.object = game.object))
    action_prob <- Act.A3C(state = vis.state, model = model,model.par = model.par,algo.par=algo.par, game.object = game.object, worker.number = worker.number, epsilon=epsilon) # LSTM-Cell is stateful and therefore remembers the state from the previous prediction. 
    
    next.state.full <- game.object$state.transition(game.state=state,action=action_prob$action,game.object=game.object)
    
    next.state <- next.state.full$next.state
    reward <- next.state.full$reward
    done <- next.state.full$game.finished
    
    if(!is.null(game.object$game.pars$action.space)&&game.object$game.pars$action.space == "continous"){
      Worker.Memory[[length(Worker.Memory)+1]] <- list(state = vis.state, action = action_prob$action, worker.episode = Worker.Episode, round = state$round,reward = reward, done = done)
    }else{
     action.one.hot <- rep(0,times = game.object$game.par(game.object)$output.nodes)
     action.one.hot[action_prob$action] <- 1
    
     Worker.Memory[[length(Worker.Memory)+1]] <- list(state = vis.state, action = action.one.hot, prob = action_prob$prob, worker.episode = Worker.Episode, round = state$round,reward = reward, done = done, chosen.action = next.state$history.real[state$round,])
    }
    
    if(done) break
    
    state <- next.state
    
  }
  
  queue <- Convert.2.train(Memory = Worker.Memory)
  Training.queue  <- Advantage.function(queue = queue, model = model, model.par = model.par,algo.par = algo.par, done = done, state = state, game.object=game.object)
  
  assign("next.state.global", next.state.full, envir = .GlobalEnv)
  assign("reward.vec.global", c(reward.vec.global, queue$r), envir = .GlobalEnv)
  avg.reward.episode <- NULL
  
  if(done){
    assign("Worker.Episode", Worker.Episode+1, envir = .GlobalEnv)
    if(!is.null(game.object$game.pars$direct.rewards)&&game.object$game.pars$direct.rewards){
      avg.reward.episode <- mean(reward.vec.global)
      assign("reward.vec.global", vector("numeric"), envir = .GlobalEnv)
    }else{
      avg.reward.episode <- Worker.Memory[[length(Worker.Memory)]]$reward
    }
    if(algo.par$use.egreedy.per.worker[worker.number]&&epsilon>algo.par$epsilon.min){
      assign("epsilon", epsilon*algo.par$epsilon.decay, envir = .GlobalEnv)
    }
  }
  
  if(model.par$name == "A3C.Neural.Network.LSTM"){ 
    model$layers[[3]]$reset_states(states = lstm.cell.states)
    Training.queue$s <- redim.state(Training.queue$s)
  }
  
  if(algo.par$train){
    graph.output <- sess$run(list(Graph$gradient_clipped,Graph$loss.policy,Graph$loss.value, Graph$loss), feed_dict = dict(s_ = Training.queue$s,r_ = Training.queue$target_v,a_ = Training.queue$a, adv_ = Training.queue$adv))
    names(graph.output) <- c("gradient","loss.policy","loss.value","loss")
    acc.gradients <- graph.output$gradient
    graph.output[[1]] <- NULL
  }else{
    graph.output <- NULL
    acc.gradients <- NULL
  }

  if(algo.par$Full.Output&&Worker.Episode<algo.par$max.mem.per.worker){
    
    algo.var.worker$Graph.Output[[length(algo.var.worker$Graph.Output)+1]] <- graph.output
    algo.var.worker$memory = c(algo.var.worker$memory,Worker.Memory)
    
    if(Worker.Episode%%algo.par$Transfer.Output.every==0&&done){
      
      assign("algo.var.Worker", Initialise.A3C(algo.par = algo.par)$algo.var.Worker, envir = .GlobalEnv)
      
      return(list(training = acc.gradients, reward = avg.reward.episode, finished = done, algo.var.worker=algo.var.worker))
      
    }else{
      
      assign("algo.var.Worker", algo.var.worker, envir = .GlobalEnv)  #preserve state if Output is not pushed 2 Master.
      
    }
    
  }
  
  return(list(training = acc.gradients, reward = avg.reward.episode, finished = done, algo.var.worker = NULL))
  
}

#' Get Default Parameters of the Feed-Forward Neural Network for the A3C implementation.
#'
#' Returns a list with default Parameters which could be used as the \code{model.par} argument: \itemize{
#' \item \strong{name} Name of the network. Used to identify the type of network.
#' \item \strong{setup} Function to create the neural network. Default case: \code{Setup.Neural.Network.A3C}.
#' \item \strong{predict} Function predicts values based on the neural network. Default case: \code{Predict.Neural.Network.A3C}
#' \item \strong{hidden.nodes} Vector containing the number of neurons of each network layer. 
#' \item \strong{activation.hidden} Specifies the activation function of the neurons in each hidden layer.
#' \item \strong{activation.output} Specifies the activation functions in the output layer.
#' \item \strong{c.value} Importance of Value loss in the total loss of the network. By Default 0.5.
#' \item \strong{c.entropy} Importance of Entropy loss in the total loss of the network. By Default 0.02.
#' \item \strong{Optimizer} Specifies the used Optimizer and the learning rate. Default Case: Adam Optimizer with a learning rate of 0.005.
#' \item \strong{decay} 
#' \item \strong{clip.norm} The global norm of the gradients at which they are clipped. Prevents really large weight updates.
#'}
#'@export

Get.Def.Par.Neural.Network.A3C <- function(){
  
  name <- "A3C.Neural.Network"
  
  setup <- Setup.Neural.Network.A3C
  predict <- Predict.Neural.Network.A3C
  
  hidden.nodes <- c("10", "10", "10")
  activation.hidden <- c("relu", "relu", "relu")
  activation.output <- c("softmax", "linear")
  
  c.value <- 0.5
  c.entropy <- 0.02
  
  clip.norm = 40
  
  return(nlist(name,setup,predict,hidden.nodes,activation.hidden,activation.output,c.value, c.entropy ,clip.norm))
  
}

#' Get Default Parameters of the LSTM Neural Network for the A3C implementation.
#' 
#' Returns a list which can be used as the \code{model.par} argument: \itemize{
#' \item \strong{name} Name of the network. Used to identify the type of network.
#' \item \strong{setup} Function to create the neural network. Default case: \code{Setup.Neural.Network.A3C.LSTM}.
#' \item \strong{predict} Function predicts values based on the neural network. Default case: \code{Predict.Neural.Network.A3C.LSTM
#' \item \strong{reset.cell.state.every} Should be either "N.Steps" or "Episode". Specifies when the cell-state is reseted to 0.
#' \item \strong{c.value} Importance of Value loss in the total loss of the network. By Default 0.5.
#' \item \strong{c.entropy} Importance of Entropy loss in the total loss of the network. By Default 0.02.
#' \item \strong{Optimizer} Specifies the used Optimizer and the learning rate. Default Case: Adam Optimizer with a learning rate of 0.005.
#' \item \strong{decay} 
#' \item \strong{clip.norm} The global norm of the gradients at which they are clipped. Prevents really large weight updates.
#' }
#'@export

Get.Def.Par.Neural.Network.A3C.LSTM <- function(){
  
  name <- "A3C.Neural.Network.LSTM"
  
  setup <- Setup.Neural.Network.A3C.LSTM
  predict <- Predict.Neural.Network.A3C
  
  # hidden.nodes <- c("10", "10", "10")
  # activation.hidden <- c("relu", "relu", "relu")
  # activation.output <- c("softmax", "linear")
  
  reset.cell.state.every <- "N.Steps" # after how many rounds should the state of the lstm-cell be reseted. If not "N.Steps" cell-states are reseted after the whole episode.
  
  c.value <- 0.5
  c.entropy <- 0.0001
  
  # Optimizer <- tf$train$AdamOptimizer()
  # decay <- 0.99
  
  clip.norm = 40
  
  return(nlist(name,setup,predict,reset.cell.state.every,c.value,c.entropy, clip.norm))
  
}

#' Setup a Feed-Forward Neural Network for the A3C-implementation.
#' 
#' Returns a Feed-Forward Neural Network based on keras.
#' Important: At the moment this does only support discrete action spaces. For the continous case use \code{Setup.Neural.Network.A3C.LSTM}
#' @param model.par A list with parameters to set up the Network e.g. as given by \code{Get.Def.Par.Neural.Network.A3C}.
#' @param game.par A list with parameters of the game. 
#' 
#'@export 

Setup.Neural.Network.A3C <- function(model.par, game.object){
  
  game.par <- game.object$game.par(game.object)
  
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

#' Setup a  Neural Network with an LSTM-Layer for the A3C-implementation.
#' 
#' Returns a Recurrent Neural Network based on keras.
#' @param model.par A list with parameters to set up the Network e.g. as given by \code{Get.Def.Par.Neural.Network.A3C.LSTM}.
#' @param game.par A list with parameters of the game. 
#' 
#'@export 

Setup.Neural.Network.A3C.LSTM <- function(model.par, game.object){
  
  game.par <- game.object$game.par(game.object)
  game.pars <- game.object$game.pars
  inputs <- layer_input(batch_shape = list(1,NULL,game.par$input.nodes))
  
  hidden_layers <-inputs%>%
    time_distributed(layer = layer_dense(units = 10, batch_input_shape = list(1,NULL,game.par$input.nodes), activation = "relu"))%>%
    layer_lstm(units = 7,return_sequences =TRUE, return_state= FALSE, stateful = TRUE)%>%
    time_distributed(layer =layer_dense(units = 10, activation = "relu"))
  
  if(!is.null(game.pars$action.space)&&game.pars$action.space== "continous"){
    Mean.out <- hidden_layers%>%time_distributed(layer= layer_dense(units = game.par$output.nodes, activation = "linear"))
    Var.out <-  hidden_layers%>%time_distributed(layer= layer_dense(units = 1, activation = "softplus"))
    
    P.out <- layer_concatenate(list(Mean.out,Var.out))
  }else{
    P.out <- hidden_layers%>%time_distributed(layer= layer_dense(units = game.par$output.nodes, activation = "softmax"))
  }
  
  V.out <- hidden_layers%>%time_distributed(layer_dense(units = 1, activation = "linear"))
  
  model <- keras_model(inputs = inputs, outputs = c(P.out,V.out))
  
}

#' Predict Neural Network
#'
#' Predict action probabilities and state values based on a neural network.
#' @param model A Neural Network e.g. as given by \code{Setup.Neural.Network.A3c} or \code{Setup.Neural.Network.A3c.LSTM}.
#' @param model.par A list with parameters to set up the Network e.g. as given by \code{Get.Def.Par.Neural.Network.A3C} or \code{Get.Def.Par.Neural.Network.A3C.LSTM}.
#' @param state An encoded game state. 
#' @param Indicator Indicates which value should be returned. Could be either: \itemize{
#' \item \strong{"Value"} Only the predicted value of the state is returned.
#' \item \strong{"Prob"} Only the action probabilites of the given state are returned.
#' \item \strong{NULL} Both are retured.
#' }
#'@export 

Predict.Neural.Network.A3C <- function(model,model.par,state,Indicator = NULL){
  
  if(model.par$name=="A3C.Neural.Network.LSTM") state <-redim.state(state)
  
  p_v <- model %>% predict(state) 
  
  if(is.null(Indicator)){
    
    return(p_v)
    
  }else if(Indicator == "Value"){
    
    return(p_v[[2]])
    
  }else if(Indicator == "Prob"){
    
    return(p_v[[1]])
  }
}

#' Graph for Network Loss according to A3C.
#' 
#' Defines a tensorflow graph specifiying the loss calculation according to the A3C algorithm.
#' 
#' Returns a list of tensors with the following items: \itemize{
#' \item \strong{gradient.clipped} Calculated gradients of the network weights.
#' \item \strong{loss.policy} Calculated loss of the policy.
#' \item \strong{loss.value} Calculated value loss.
#' \item \strong{loss} Calculated total loss of the network.
#' \item \strong{s_} Placeholder for states.
#' \item \strong{a_} Placeholder for actions.
#' \item \strong{r_} Placeholder for rewards.
#' \item \strong{adv_} Placeholder for calculated advantages.
#' }
#' 
#' @param model A Neural Network e.g. as given by \code{Setup.Neural.Network.A3c} or \code{Setup.Neural.Network.A3c.LSTM}.
#' @param model.par A list with parameters to set up the Network e.g. as given by \code{Get.Def.Par.Neural.Network.A3C} or \code{Get.Def.Par.Neural.Network.A3C.LSTM}.
#' @param game.object A Game Object (list) as defined by \code{Get.Game.Object.<NAME>}.
#'@export

Define_Graph <- function(model, model.par, game.object){
  
  if(model.par$name == "A3C.Neural.Network"){
    
    s_ <- tf$placeholder(tf$float32, shape(NULL,game.object$game.par(game.object)$input.nodes))
    
  }else if(model.par$name == "A3C.Neural.Network.LSTM"){
    
    s_ <- tf$placeholder(tf$float32, shape(1,NULL,game.object$game.par(game.object)$input.nodes))
    
  }

  
  a_ <- tf$placeholder(tf$float32, shape(NULL, game.object$game.par(game.object)$output.nodes))
  r_ <- tf$placeholder(tf$float32, shape(NULL, 1))
  adv_ <- tf$placeholder(tf$float32, shape(NULL, 1))
  
  p_V <- model(s_)
  p <- p_V[[1]]
  V <- p_V[[2]]
  
  if(model.par$name == "A3C.Neural.Network.LSTM"){
    if(!is.null(game.object$game.pars$action.space)&&game.object$game.pars$action.space == "continous"){
      p = tf$reshape(p, shape = c(-1L,as.integer(game.object$game.par(game.object)$output.nodes+1)))
     }else{p = tf$reshape(p, shape = c(-1L,as.integer(game.object$game.par(game.object)$output.nodes)))
    }
    V = tf$reshape(V, shape = c(-1L,1L))
  }
  
  TD.error <- r_ - V
  
  if(!is.null(game.object$game.pars$action.space)&&game.object$game.pars$action.space == "continous"){
    d <- tf$contrib$distributions$MultivariateNormalDiag(loc =p[,1:game.object$game.par(game.object)$output.nodes], scale_identity_multiplier = p[,game.object$game.par(game.object)$output.nodes+1])
    log.p <- d$log_prob(a_)
    entropy <- tf$reduce_sum(d$entropy())
    
  }else{
    log.p <- log(tf$reduce_sum(p*a_, axis = 1L, keep_dims = TRUE))
    entropy <- tf$reduce_sum(tf$reduce_sum(p * log(p+1e-10), axis = 1L, keep_dims = TRUE),reduction_indices = NULL)
  }
  
  loss.policy <- -tf$reduce_sum(log.p*adv_,reduction_indices = NULL)
  loss.value <- tf$reduce_sum((TD.error^2),reduction_indices = NULL)
  
  loss <- loss.policy+model.par$c.value*loss.value+model.par$c.entropy*entropy
  
  train_vars <- model$trainable_weights
  gradient_weights <- tf$train$AdamOptimizer(learning_rate = 0.001)$compute_gradients(loss, train_vars)
  gradient <- lapply(gradient_weights,"[[",1)
  gradient_clipped <- tf$clip_by_global_norm(gradient,model.par$clip.norm)[[1]]
  
  return(nlist(gradient_clipped,loss.policy,loss.value,loss,s_,a_,r_,adv_))
  
}

#' Graph to update Network weights
#' 
#' Defines a tensorflow graph to update Network parameters based on the gradients w.r.t to the loss function.
#' 
#' Returns a list of tensors and placeholders \itemize{
#' \item \strong{gradients.update} Calculate updated weights based on provided gradients.
#' \item \strong{vars.grads.pl} Placeholders for the gradients.
#' }
#' 
#' @param model A Neural Network e.g. as given by \code{Setup.Neural.Network.A3c} or \code{Setup.Neural.Network.A3c.LSTM}.
#' @param model.par A list with parameters to set up the Network e.g. as given by \code{Get.Def.Par.Neural.Network.A3C} or \code{Get.Def.Par.Neural.Network.A3C.LSTM}.
#'@export 

Define_Graph_Gradient_Update<- function(model, model.par){
  
  tvs <- model$trainable_weights
  
  vars.grads <- lapply(tvs,function(x){tf$Variable(tf$zeros_like(x$initialized_value()),trainable = FALSE)})
  vars.grads.pl <- lapply(vars.grads,function(x){tf$placeholder(dtype = tf$float32, shape = x$get_shape())})
  
  gradients.new <- mapply(function(x,y){tf$assign(x,y)},vars.grads,vars.grads.pl)
  gradients.weights.new  <-   mapply(function(x,y){list(list(x,y))},gradients.new,tvs)
  gradients.update <- tf$train$AdamOptimizer(learning_rate = 0.001)$apply_gradients(grads_and_vars = gradients.weights.new)
  
  return(nlist(gradients.update, vars.grads.pl))
  
}

#' Change dimensionality of the state array.
#'
#' Helper funciton to add a batch dimensionalty needed for the LSTM-Neural Network.
#' @param state An ecoded array of game states
#'@export  

redim.state <- function(state){
  dim(state) <- c(1,dim(state))
  return(state)
}

#' Prepare Worker Memory to visualize with shiny
#' 
#' Memory of one worker is transformed so it can be as input to the shiny app.
#' 
#' Returns a list with following items \itemize{
#' 
#' \item \strong{memory.mat} The experience of a worker stored in a numeric matrix.
#' \item \strong{graph.mat} Network loss data stored in a numeric matrix.
#' \item \strong{help.data} Additional data used in the shiny app.
#' } 
#' 
#' @param threads.data Memory of experience of one worker.
#' @param choices Possible resulting choices as a string vector. In the IPD game this would be c("CC", "CD","DC","DD").     
#' @param actions Actions space of the specific game [Currently only IPD is supported.] as a string vector. 
#'@export

prep.data.4.shiny <- function(threads.data,choices,actions){
  
  threads.data$memory <-  lapply(seq_along(threads.data$memory), function(x){
    
    choices.df <-  unlist(threads.data$memory[[x]]$chosen.action)
    choices.str <- paste0(choices.df[1],choices.df[2])
    threads.data$memory[[x]]$choices.ind <- which(choices == choices.str)
    threads.data$memory[[x]]$action.me <- which(actions == choices.df[1])
    threads.data$memory[[x]]$action.other <- which(actions == choices.df[2])
    threads.data$memory[[x]]$state <- NULL
    threads.data$memory[[x]]$chosen.action <- NULL
    return(threads.data$memory[[x]])
    
  })
  
  memory.mat <- t(sapply(threads.data$memory, unlist))
  memory.mat <- memory.mat[,setdiff(colnames(memory.mat),c("prob2","action1", "action2"))]
  avg.reward <- memory.mat[memory.mat[,"done"]==1,"reward"]
  choice.ind.start <- memory.mat[memory.mat[,"round"]==1,"choices.ind"]
  
  help.data <- cbind(avg.reward,choice.ind.start)
  graph.mat <- t(sapply(threads.data$Graph.Output, unlist))
  
  return(nlist(memory.mat,graph.mat,help.data))
  
}


