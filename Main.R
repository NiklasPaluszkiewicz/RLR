library(ReinforcementLearningwithR)

game.object <- Get.Game.Object.PD(encoding.state = "Slim.TenTen")

game.object$game.pars$T <- 60
game.object$game.pars$T.max <- 60
game.object$game.pars$delta <- 0.95
game.object$game.pars$other.strategies <- c(ta.daaa)
names(game.object$game.pars$other.strategies) <- c("ta.daaa")
game.object$game.pars$err.D.prob <- 0.15
game.object$game.pars$err.C.prob <- 0.0

algo.par <- Get.Def.Par.QPredictions()
algo.par$mem.type <- "game.encoded"
algo.par$weighting.factor <- 0.1
algo.par$show.current.status <- 10
algo.par$replay.every <- 25
episodes <- 500
algo.par$epsilon.start <- 1
algo.par$epsilon.min <- 0.02
algo.par$gamma <- game.object$game.pars$delta
algo.par$weighting.policy <- "max"


model.par <- Get.Def.Par.Neural.Network()
model.par$hidden.nodes <- c(10,5)
model.par$activation.hidden <- c("relu","relu")
model.par$epochs <- 10

model <- Setup.QPredictions(game.object, algo.par=algo.par, model.par=model.par)

algo.var <- Initialise.QPredictions(game.object, algo.par, memory.init="solid.foundation", memory.par=list(self.no=100,rep.no=100))
#algo.var <- Initialise.QPathing(game.object, algo.par, memory.init=NULL)


set.storing(FALSE)

#algo.var$epsilon <- algo.par$epsilon.min
#st <- Sys.time()
res <- Train.QPredictions(model=model, model.par=model.par, algo.par=algo.par, algo.var=algo.var, game.object = game.object, episodes=episodes, eval.only=FALSE, start.w.training = TRUE)
#et <- Sys.time()
#ttm <- et - st
#tt <- as.numeric(difftime(et, st, tz,
#                          units = c("secs")))
#tt

#Save Memory & model
model <- res$model
algo.var$memory.net <- res$algo.var$memory.net
algo.var$analysis <- res$algo.var$analysis

#Graphical Analysis
prec <- 20
mov.av <- sapply(seq(floor((length(algo.var$analysis$score)/prec)),length(algo.var$analysis$score),by=floor((length(algo.var$analysis$score)/prec))), FUN=function(x){
  sum(algo.var$analysis$score[(x-floor((length(algo.var$analysis$score)/prec))):x])/(floor((length(algo.var$analysis$score)/prec)))
})
plot(algo.var$analysis$score)
lines(x=seq(floor((length(algo.var$analysis$score)/prec)),length(algo.var$analysis$score),by=floor((length(algo.var$analysis$score)/prec))), y=mov.av, lty="solid", col="red", type="l", lwd="3")

#anal.rounds <- 1000
#res.analysis <- Train.QPredictions(model=model, model.par=model.par, algo.par=algo.par, algo.var=algo.var, game.object = game.object, episodes=anal.rounds, eval.only=TRUE, start.w.training = TRUE)
#mean(res.analysis$algo.var$analysis$score[(length(res.analysis$algo.var$analysis$score)-anal.rounds):length(res.analysis$algo.var$analysis$score)])
#1.099
#1.113
#1.116
#Calc.Reward.QPathing(net=res$algo.var$memory.net, pointer=477,mode=algo.par$weighting.policy, action=2, mode.par=list(weighting.factor=algo.par$weighting.factor))
#res$algo.var$memory.net[[477]]

#====== Examining Answers ========

# Init game
game = make.pd.game(err.D.prob=0.15, delta=0.95)
#original: err.D.prob = 0.15, delta=0.95

strat = nlist( rainbow.unicorn.antistrat2,seda.strat2)


rep.res <- c()
u.weights <- c()
for(i in 1:1000){
  res <- run.rep.game(game=game, strat=strat, T.max=60)
  rep.res <- c(rep.res,res$res[1,"u"])
  u.weights <- c(u.weights,res$res[1,"u.weight"])
  print(i)
}
sum((unlist(rep.res)*unlist(u.weights)))/sum(unlist(u.weights))
# 0.6213851

# Init and run a tournament of several strategies against each other
strat = nlist(NN.strat.Slim.TenTen,ta.daaa, a.tadaaa.1)
tourn = init.tournament(game=game, strat=strat)

#set.storing(FALSE)  # uncomment to make code run faster
tourn = run.tournament(tourn=tourn, R = 1000, T.max=60)
show.tournament(tourn)

