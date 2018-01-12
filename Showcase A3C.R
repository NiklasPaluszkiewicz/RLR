game =   Get.Game.Object.PD(encoding.state = "full.zero")

game$game.pars$T <- 30
#game$game.pars["T"] <- list(NULL)
game$game.pars$T.max <- 30
game$game.pars$delta <- 0.9
game$game.pars$intermed <- 1 # use full intermediate returns.

model.par <- Get.Def.Par.Neural.Network.A3C()
algo.par <- Get.Def.Par.A3C()
algo.var <- Initialise.A3C(algo.par=algo.par)

set.storing(storing = FALSE)

Train.Output <- Train.A3c(model.par = model.par, algo.par = algo.par, algo.var = algo.var, game.object = game, episodes.max = 1000)

Reward <- lapply(1:algo.par$N.Worker,function(x){sapply(Train.Output$algo.var.Master[[x]]$memory, "[[", "reward")})
Done <- lapply(1:algo.par$N.Worker,function(x){sapply(Train.Output$algo.var.Master[[x]]$memory, "[[", "done")})
State <- lapply(1:algo.par$N.Worker,function(x){t(sapply(Train.Output$algo.var.Master[[x]]$memory, "[[", "state"))})
Rewards.Finished <- lapply(1:algo.par$N.Worker,function(x){Reward[[x]][Done[[x]]]})

loss.value<- lapply(1:algo.par$N.Worker,function(x){sapply(Train.Output$algo.var.Master[[x]]$Graph.Output,function(y){y$loss.value})})
loss.policy<- lapply(1:algo.par$N.Worker,function(x){sapply(Train.Output$algo.var.Master[[x]]$Graph.Output,function(y){y$loss.policy})})
loss<- lapply(1:algo.par$N.Worker,function(x){sapply(Train.Output$algo.var.Master[[x]]$Graph.Output,function(y){y$loss})})

colors = c("red","blue","yellow")

#Plot reward per worker and episode
plot(stats::filter(Rewards.Finished[[1]],filter=rep(0.1,10), method  = "convolution", sides = "1"),xlab= "Episode", ylab = "Reward MA(10)")
invisible(lapply(2:algo.par$N.Worker, function(x){lines(stats::filter(Rewards.Finished[[x]],filter=rep(0.1,10), method  = "convolution", sides = "1"), col =colors[x-1])}))

#Plot policy loss of network per worker and n-steps
plot(stats::filter(loss.policy[[1]],filter=rep(0.1,10), method  = "convolution", sides = "1"), type = "l", xlab = "#N-Step", ylab= "Loss MA(10)")
invisible(lapply(2:algo.par$N.Worker, function(x){lines(stats::filter(loss.policy[[x]],filter=rep(0.1,10), method  = "convolution", sides = "1"), col =colors[x-1])}))
