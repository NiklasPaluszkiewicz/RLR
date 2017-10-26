#A simple Game, which delivers A, B or "A and B" - The Goal is to correctly identify which setting is displayed and choosing the solutions 1, 2 or 3. This is repeated 3 times with each round giving 1 Point if correct. If all are scored correct, 5 extra points are awarded.
#' Defines model parameters for 'Simple Game'
#'
#' Public Function which might be called by algorithm functions.
#' Output is a list of the following structure:
#' \itemize{
#' \item input.nodes - Length of array as presented by state.2.array
#' }
#' @param game.object A game object as defined by Get.Game.Object.Simple.Game
#' @export
Get.Par.Simple.Game <- function(game.object){
  restore.point("Get.Par.Simple.Game")
  input.nodes <- length(State.2.Array.Simple.Game(Generate.Start.State.Simple.Game(),game.object=game.object))
  output.nodes <- Action.Encoding.Info.Simple.Game(game.object$encoding.action)$output.nodes
  game.param <- list(input.nodes=input.nodes, output.nodes=output.nodes)
  return(game.param)
}

#' Generates Start State for Simple Game
#'
#' A state consisting of a human-readable start state. Here all information which are actually state dependend should be saved. All information which does not change from game to game of the same type of game should be saved in game.object.\cr \cr
#' Note that states have to be structurally identical to each other, even if not all information is needed at the beginning.\cr \cr
#' Public function, which might be called by an algorithm.
#' @export
Generate.Start.State.Simple.Game <- function(game.object){
  round <- 1 # first round
  A <- sample(c(TRUE,FALSE),1)
  B <- sample(c(TRUE,FALSE),1)
  if(!A&&!B){
    force.which <- sample(c("A","B"),1)
    if(force.which=="A"){
      A <- TRUE
    } else {
      B <- TRUE
    }
  }
  game.finished <- FALSE
  correct.rounds <- 0
  res <- list(A=A,B=B,round=round, correct.rounds=correct.rounds, game.finished=game.finished)
  return(res)
}

#' State to Array for Simple Game
#'
#' Transforms Game State to readable array. Here a lot of optimization might take place, as different algorithms might like different encodings.
#' @param game.state Game State in human readable form.
#' @param game.object as specified by Get.Game.Object
#' Public Function which might be called by algorithms.
#' @export
State.2.Array.Simple.Game <- function(game.state,game.object){
  if(is.null(game.object$encoding.state)){
    encoding <- "main"
  } else {
    encoding <- game.object$encoding.state
  }

  if(encoding == "main"){
    arr <- vector("numeric",length=3)
    arr[1] <- game.state$A
    arr[2] <- game.state$B
    arr[3] <- game.state$round/4
    return(arr)
  } else {
    stop("Wrong encoding specified.")
  }
}

#' Array to Action for Simple Game
#'
#' Transforms the output of an machine learning algorithm to a readable game state \cr \cr
#' Internal Function - There should be no need to call this function by an algorithm.
#' @param output.choice Number of chosen action.
#' @param game.object as specified by Get.Game.Object
Choice.2.Action.Simple.Game <- function(output.choice, game.object){
  if(is.null(game.object$encoding.action)){
    encoding <- "main"
  } else {
    encoding <- game.object$encoding.action
  }

  if(encoding == "main"){
    if(output.choice==1){
      chosen <- "A"
    } else if (output.choice==2){
      chosen <- "B"
    } else {
      chosen <- "A and B"
    }
    res <- list(chosen=chosen)
    return(res)
  } else {
    stop("Wrong encoding specified.")
  }
}

#' Get Info of Action Encoding
#'
#' Internal Function delivering info of action encoding, as e.g. number of nodes.
Action.Encoding.Info.Simple.Game <- function(game.object){
  if(is.null(game.object$encoding.action)){
    encoding <- "main"
  } else {
    encoding <- game.object$encoding.action
  }

  if(encoding=="main"){
    res <- list(output.nodes=3)
    return(res)
  } else {
    stop("Wrong encoding specified.")
  }
}

#' Get next State of Simple Game
#'
#' Outputs List with three elements:
#' \itemize{
#' \item next.state - next game state (in Complete Form)
#' \item reward - What did we get from transitioning to the next state?
#' \item game.finished - Boolean; is the game over?
#' }
#' Public Function which might be called by algorithms.
#' @param game.state A complete game state, non encoded
#' @param action The choice of action.
#' @param game.object as specified by Get.Game.Object
#' @export
State.Transition.Simple.Game <- function(game.state, action,game.object){
  action <- Choice.2.Action.Simple.Game(action,game.object)

  #Calculate next state I
  #It might be sensible to write a completely new function to do this in general, but this is much easier
  next.state <- Generate.Start.State.Simple.Game()
  next.state$round <- game.state$round+1
  next.state$correct.rounds <- game.state$correct.rounds #!!! gets updated at a later point
  if(next.state$round==4){
    next.state$game.finished <- TRUE
  }
  game.finished <- next.state$game.finished

  #Calculate Reward and update correct.rounds in next.state
  if(action$chosen=="A" && game.state$A && !game.state$B){
    reward <- 1
    next.state$correct.rounds <- next.state$correct.rounds+1
  } else if (action$chosen=="B" && game.state$B && !game.state$A){
    reward <- 1
    next.state$correct.rounds <- next.state$correct.rounds+1
  } else if (action$chosen=="A and B" && game.state$A && game.state$B){
    reward <- 1
    next.state$correct.rounds <- next.state$correct.rounds+1
  } else {
    reward <- 0
  }

  #Last Round effect
  if(next.state$game.finished && next.state$correct.rounds == 3){
    reward <- reward + 5
  }

  return(list(next.state=next.state,reward=reward,game.finished=game.finished))
}

#' Get Game Object which fully defines simple game.
#'
#' A simple Game, which delivers A, B or "A and B" - The Goal is to correctly identify which setting is displayed and choosing the solutions 1, 2 or 3. This is repeated 3 times with each round giving 1 Point if correct. If all are scored correct, 5 extra points are awarded.
#' @param encoding.state Which method should be used to encode the game? Currently supported:
#' \itemize{
#' \item main - [A,B,rounds]
#' }
#' @param encoding.action Which method should be used to encode the action? Currently supported:
#' \itemize{
#' \item main - [A,B,A and B]
#' }
#' @export
Get.Game.Object.Simple.Game <- function(encoding.state=NULL, encoding.action=NULL){

  game.par <- Get.Par.Simple.Game
  state.transition <- State.Transition.Simple.Game
  state.2.array <- State.2.Array.Simple.Game
  start.state <- Generate.Start.State.Simple.Game

  game.object <- nlist(game.par, encoding.state, encoding.action,state.transition,state.2.array, start.state)
  return(game.object)
}
