library(UnSupervisedLearningwithR)

game.object <- Get.Game.Object.PD()
model <- Setup.QLearning(game.object, model.par=NULL)
model.par <- Get.Def.Par.QLearning()
model.var <- Initialise.Qlearning(model.par)

set.storing(TRUE)
res <- Train.QLearning(model=model,model.var = model.var, model.par=model.par, game.object = game.object, episodes=2000)

#Analyze Game
model <- res$model
test.state1 <- Generate.Start.State.Simple.Game()

prediction <- predict(model,t(game.object$state.2.array(game.state=test.state1, game.object)))
action <- which.max(prediction)

test.state2 <- game.object$state.transition(game.state=test.state1,action=action,game.object=game.object)$next.state

prediction <- predict(model,t(game.object$state.2.array(game.state=test.state2,game.object)))
action <- which.max(prediction)

test.state3 <- game.object$state.transition(game.state=test.state2,action=action,game.object=game.object)$next.state

prediction <- predict(model,t(game.object$state.2.array(game.state=test.state3,game.object)))
action <- which.max(prediction)

test.state.final <- game.object$state.transition(game.state=test.state3,action=action,game.object=game.object)$next.state

prediction <- predict(model,t(game.object$state.2.array(game.state=test.state.final,game.object)))
