#Functions related to NeuralNetworks

#' Define default Parameters of the Neural Network Function
#'
#' Returns a List which may be used as \code{model.par} of e.g. the function \code{Setup.QLearning()} with the following parameters:\itemize{
#' \item \strong{name} - Identifier of Model. Per Default \"Neural.Network.Basic\".
#' \item \strong{setup} - Function which should be used to setup the Neural Network. Per Default \code{Setup.Neural.Network}
#' \item \strong{predict} - Function which should be used to predict the Neural Network. Per Default \code{Predict.Neural.Network}
#' \item \strong{train} - Function which should be used to train/calibrate the Neural Network. Per Default \code{Train.Neural.Network}
#' \item \strong{hidden.nodes} - A Vector consisting of the number of Neurons in each hidden layer - e.g. c(25,10) to have two hidden layers with the first layer having 25 Neurons.
#' \item \strong{activation.hidden} - A Vector defining the activation functions of the hidden layers, e.g. c(\"relu\",\"relu\"). Has to have the same number of items as \code{hidden.nodes}. Supported are e.g. relu, tanh, sigmoid and linear
#' \item \strong{activation.output}. Activiation function of the output layer. Supported are e.g. relu, tanh, sigmoid and linear.
#' \item \strong{loss}. Specifies the loss function, e.g. \'mse\'
#' \item \strong{optimizer}. Specifies the used optimizer. By Default Adam Optimization is used with a Learning rate of 0.001.
#' \item \strong{epochs}. How many epochs should the Neural Network be trained?
#' \item \strong{batch.size}. Batch Size of Neural Network.
#' \item \strong{verbose}. Should the Neural Network give an output? 0 for no output, 1 for output for each epoch, 2 for aggregate output every other epoch.
#' }
#' @export
Get.Def.Par.Neural.Network <- function(){
  #Identifier
  name <- "Neural.Network.Basic"

  #Functions
  setup <- Setup.Neural.Network
  predict <- Predict.Neural.Network
  train <- Train.Neural.Network

  #Struktural Parameters
  hidden.nodes <- c(10,5)
  activation.hidden <- c("relu","relu")
  activation.output <- c("linear")
  loss <- "mse"
  optimizer <- optimizer_adam(lr=0.001)
  single.dimensional <- TRUE #Only one output neuron. Actions are part of Statespace

  #Training parameters
  epochs <- 50
  batch.size.train <- 32
  verbose <- 0
  enforce.increasing.precision <- TRUE
  give.up.precision <- 10

  model.def.par <- nlist(name,setup,predict,train,hidden.nodes,activation.hidden,activation.output,loss,optimizer,epochs, batch.size.train, verbose, enforce.increasing.precision, give.up.precision, single.dimensional)

  return(model.def.par)
}

#' Setup a Neural Network
#'
#' Setup the Neural Network in keras to work with it. Returns a keras stile Neural Network
#' @param model.par Parameters of Neural Network e.g. given by \code{Get.Def.Par.Neural.Network}
#' @param game.par Parameters of Game. Used are \itemize{
#' \item input.nodes - Number of Input Nodes
#' \item output.nodes - Number of Actions
#' }
#' @export
Setup.Neural.Network <- function(model.par, game.par){
  model <- keras_model_sequential()
  if(model.par$single.dimensional){
    input.nodes <- game.par$input.nodes + game.par$output.nodes
    output.nodes <- 1
  } else {
    input.nodes <- game.par$input.nodes
    output.nodes <- game.par$output.nodes
  }

  for(i in 1:length(model.par$hidden.nodes)){
    if(i==1){
      model %>%
        layer_dense(units = model.par$hidden.nodes[i], input_shape = input.nodes) %>%
        layer_activation(activation = model.par$activation.hidden[i])
    } else {
      model %>%
        layer_dense(units = model.par$hidden.nodes[i], input_shape = model.par$hidden.nodes[i-1]) %>%
        layer_activation(activation = model.par$activation.hidden[i])
    }
  }

  model %>%
    layer_dense(units = output.nodes) %>%
    layer_activation(activation = model.par$activation.output)

  model %>% compile(
    loss = model.par$loss,
    optimizer = model.par$optimizer
  )
  return(model)
}

#' Evaluate Neural Network
#'
#' Evaluate a model based on a game.state
#' @param model A trained Neural Network e.g. given by \code{Setup.Neural.Network}.
#' @param state A game.state after being encoded by the game.object.
#' @export
Predict.Neural.Network <- function(model, model.par, state){
  restore.point("Predict.Neural.Network")
  return(model %>% predict(state))
}

#' Train Neural Network
#'
#' Trains a neural network and prints some helpful statistics. Returns a trained model.
#' @param model A Neural Network e.g. given by \code{Setup.Neural.Network}
#' @param model.par Parameters of Neural Network, e.g. given by \code{Get.Default.Neural.Network}
#' @param x_train A Matrix with as much columns as input parameters in the encoding.
#' @param y_train A Matrix with as much columns as output parameters (e.g. action parameters). For each state (x_train)/action combination(column of y_train) this value determines the target.
#' @export
Train.Neural.Network <- function(model, model.par, x_train, y_train){
  restore.point("Train.Neural.Network")
  fit.obj <- fit(model,x_train, y_train, epochs = model.par$epochs, verbose=model.par$verbose, batch_size = model.par$batch.size.train)
  print(fit.obj)
  print("")
  return(nlist(model,fit.obj))
}
