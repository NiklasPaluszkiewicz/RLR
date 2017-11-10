#Function related to NeuralNetworks

#' @export
Get.Def.Par.Neural.Network <- function(){
  #Identifier
  name <- "Neural.Network.Basic"

  #Functions
  setup <- Setup.Neural.Network
  predict <- Predict.Neural.Network
  train <- Train.Neural.Network

  #Parameters
  hidden.nodes <- c(100)
  activation.hidden <- c("tanh")
  activation.output <- c("linear")
  loss <- "mse"
  optimizer <- optimizer_adam(lr=0.001)
  epochs <- 500
  batch.size.train <- 4

  model.def.par <- nlist(name,setup,predict,train,hidden.nodes,activation.hidden,activation.output,loss,optimizer,epochs, batch.size.train)

  return(model.def.par)
}

#' @export
Setup.Neural.Network <- function(model.par, game.par){
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

#' @export
Predict.Neural.Network <- function(model, state){
  restore.point("Predict.Neural.Network")
  return(model %>% predict(state))
}

#' @export
Train.Neural.Network <- function(model, model.par, x_train, y_train){
  restore.point("Train.Neural.Network")
  fit.obj <- fit(model,x_train, y_train, epochs = model.par$epochs, verbose=0, batch_size = model.par$batch.size.train)
  print(fit.obj)
  print("")
  return(model)
}
