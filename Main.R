library(ReinforcementLearningwithR)

game.object <- Get.Game.Object.PD(encoding.state = "maximum.full.Ten")
algo.par <- Get.Def.Par.QLearning()
model.par <- Get.Def.Par.Neural.Network()

model <- Setup.QLearning(game.object, algo.par=algo.par, model.par=model.par)

algo.var <- Initialise.Qlearning(game.object, algo.par, memory.init="solid.foundation", memory.par=list(self.no=0,rep.no=5))

set.storing(FALSE)

res <- Train.QLearning(model=model, model.par=model.par, algo.par=algo.par, algo.var=algo.var, game.object = game.object, episodes=50, eval.only=FALSE, start.w.training = TRUE)

#Save Memory & model
model <- res$model
algo.var$memory <- res$algo.var$memory
algo.var$analysis <- res$algo.var$analysis

#Graphical Analysis
plot(algo.var$analysis$score)

#Analyze
model.par$predict(model,algo.var$memory[[length(algo.var$memory)-99]]$state)
algo.var$memory[[length(algo.var$memory)-4]]$action
model.par$predict(model,algo.var$memory[[length(algo.var$memory)-18]]$state)
algo.var$memory[[length(algo.var$memory)-1]]$action
model.par$predict(model,algo.var$memory[[length(algo.var$memory)]]$state)
algo.var$memory[[length(algo.var$memory)]]$action



model.par$predict(model,algo.var$memory[[1]]$state)

#Analyze Game
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
