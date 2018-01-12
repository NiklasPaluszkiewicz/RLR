library(ReinforcementLearningwithR)

game.object <- Get.Game.Object.PD(encoding.state = "TenTen")

game.object$game.pars$T <- 50
game.object$game.pars$T.max <- 50
game.object$game.pars$other.strategies <- c(tit.for.tat)
names(game.object$game.pars$other.strategies) <- c("tit.for.tat")

algo.par <- Get.Def.Par.QPathing()
model.par <- Get.Def.Par.Neural.Network()

model <- Setup.QPathing(game.object, algo.par=algo.par, model.par=model.par)

algo.var <- Initialise.QPathing(game.object, algo.par, memory.init="solid.foundation", memory.par=list(self.no=1,rep.no=5))

set.storing(FALSE)

algo.var$epsilon <- algo.par$epsilon.start
res <- Train.QPathing(model=model, model.par=model.par, algo.par=algo.par, algo.var=algo.var, game.object = game.object, episodes=500, eval.only=FALSE, start.w.training = TRUE)

#Save Memory & model
model <- res$model
algo.var$memory.net <- res$algo.var$memory.net
algo.var$analysis <- res$algo.var$analysis

#Graphical Analysis
prec <- 10
mov.av <- sapply(seq(floor((length(algo.var$analysis$score)/prec)),length(algo.var$analysis$score),by=floor((length(algo.var$analysis$score)/prec))), FUN=function(x){
  sum(algo.var$analysis$score[(x-floor((length(algo.var$analysis$score)/prec))):x])/(floor((length(algo.var$analysis$score)/prec)))
})
plot(algo.var$analysis$score)
lines(x=seq(floor((length(algo.var$analysis$score)/prec)),length(algo.var$analysis$score),by=floor((length(algo.var$analysis$score)/prec))), y=mov.av, lty="solid", col="red", type="l", lwd="3")

res.analysis <- Train.QPathing(model=model, model.par=model.par, algo.par=algo.par, algo.var=algo.var, game.object = game.object, episodes=10, eval.only=TRUE, start.w.training = TRUE)

