# RLR
Reinforcement Learning with R

This R package has the goal to bring some known - and newly developed - Machine Learning algorithms to R. 

Currently developement focuses on examining and analysing different learning algorithms, such as Q-Learning or A3C, in the context of finding best answers to the repeated prisoners dilemma (see the package skranz/StratTourn). Syntax may change at any time as this repository is in ongoing development - update with precaution!

Defining the interface between games and learning algorithms:

Every Game has to provide the following function:

Get.Game.Object.<GameName>(<potential parameters>) which returns a list (called "game.object") which should have the following list elements:

  * game.par - A function which recieves a game.object and returns a list with two elements
    -> input.nodes - Number of input informations after encoding the game state - in case of a Neural Network the Number of Perceptor Neurons
    -> output.nodes - Number of possible actions in this game.
  * state.transition - A function which receives the game.state, the action of the "player" and the game.object and returns a new game.state
  * start.state - A function which receives a game.object and returns the first game.state. The game.states cane have any structure as no algorithm should operate on game.states.
  * state.2.array - A function which receives a game.state and a game.object and returns a vector specified by an internal encoding function with all information necessary for the learning algorithm.
  
Other list elements may be used by the game functions itself. The following items are recommended:

  * game.pars - a list with parameters which are necessary e.g. to determine the course of the game or the start state of the game
  * encoding.state - a string or function specifing how the game state should be transformed to an input vector of the learning algorithm.
  * encoding.action - a string or function specifing how the chosen action of the learning algorithm matches with the actual game action
  
 Note that structurally a game.object should never have information regarding any game.states.
<<<<<<< HEAD
=======

>>>>>>> 13486534882d82249cbf47841b07dc4e88352a78
