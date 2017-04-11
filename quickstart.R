library(devtools)
# replace this soon
load_all("/home/probst/Paper/Exploration_of_Hyperparameters/OMLbots")

# This has to be replaced by the database extraction (Daniel) ----------------------------------------------
library(devtools)
load_all()

tbl.results = getRunTable(run.tag = "botV1")
print(head(tbl.results))

tbl.hypPars = getHyperparTable("botV1", excl.run.ids = NULL, numRuns = 300000, n = 10000)
save(tbl.hypPars, file = "hypPars.RData")
tbl.hypPars3 = tbl.hypPars
for(i in 1:20) {
tbl.hypPars2 = getHyperparTable("botV1", excl.run.ids = unique(tbl.hypPars$run.id), numRuns = 300000, n = 10000)
print(head(tbl.hypPars2))
tbl.hypPars3 = rbind(tbl.hypPars3, tbl.hypPars2)
save(tbl.hypPars3, file = paste0("hypPars",i,".RData"))
}

tbl.hypPars = tbl.hypPars[which(tbl.hypPars$hyperpar.name != "verbose"),]

tbl.results = tbl.results[tbl.results$run.id %in% unique(tbl.hypPars$run.id), ]
task.ids = unique(tbl.results$task.id)

load("hypPars.RData")
# -----------------------------------------------------------------------------------------------------------

surrogate.mlr.lrns = list(
    makeLearner("regr.rpart"),
    makeLearner("regr.randomForest", par.vals = list(ntree = 2000)),
    makeLearner("regr.xgboost", par.vals = list(nrounds = 300, eta = 0.03, max_depth = 2, nthread = 1)),
    makeLearner("regr.svm"),
    #makeLearner("regr.bartMachine"),
    makeLearner("regr.cubist"),
    makeLearner("regr.glmnet"), 
    makeLearner("regr.kknn"), 
    makeLearner("regr.brnn"),
    makeLearner("regr.lm"),
    #makeLearner("regr.km")
  )

library("parallelMap")
parallelStartSocket(9)
benchmark = compareSurrogateModels(measure.name = "area.under.roc.curve", learner.name = "mlr.classif.xgboost", 
  task.ids = NULL, tbl.results, tbl.hypPars, tbl.metaFeatures = NULL, lrn.par.set, surrogate.mlr.lrns)
parallelStop()

surrogate.mlr.lrn = makeLearner("regr.randomForest", par.vals = list(ntree = 2000))
surrogates = makeSurrogateModels(measure.name = "area.under.roc.curve", learner.name = "mlr.classif.xgboost", 
  task.ids = NULL, tbl.results, tbl.hypPars, tbl.metaFeatures = NULL, lrn.par.set, surrogate.mlr.lrn)

#surrogates = getSurrogateModels(measure.name, learner.name, task.ids)

default = calculateDefault(surrogates)
optimum = calculateDatasetOptimum(surrogate, hyperpar = "all")

# Tunability overall
overallTunability = calculateTunability(default, optimum)
mean(overallTunability)

# Tunability hyperparameter specific
optimumHyperpar = calculateDatasetOptimum(surrogate, default, hyperpar = "one", n.points = 10000)
tunability = calculateTunability(default, optimumHyperpar)
colMeans(tunability)

# Interactions
optimumTwoHyperpar = calculateDatasetOptimum(surrogate, hyperpar = "two")
tunability = calculateTunability(defaults, optimumHyperpar, optimumTwoHyperpar)

# Tuning Space
tuningSpace = calculateTuningSpace(surrogate, quantile)








