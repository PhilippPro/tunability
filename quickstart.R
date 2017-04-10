library(devtools)
# replace this soon
load_all("/home/probst/Paper/Exploration_of_Hyperparameters/OMLbots")

# This has to be replaced by the database extraction (Daniel) ----------------------------------------------
library(devtools)
load_all()

tbl.results = getRunTable(run.tag = "botV1")
print(head(tbl.results))

tbl.hypPars = getHyperparTable("botV1", excl.run.ids = NULL, numRuns = 300000, n = 200000)
print(head(tbl.hypPars))

tbl.results = tbl.results[tbl.results$run.id %in% unique(tbl.hypPars$run.id), ]
task.ids = unique(tbl.results$task.id)
# -----------------------------------------------------------------------------------------------------------

surrogates = makeSurrogateModels(measure.name = "area.under.roc.curve", learner.name = "mlr.classif.xgboost", 
  task.ids = NULL, tbl.results, tbl.hypPars, param.set = lrn.par.set[[6]]$param.set)

#surrogates = getSurrogateModels(measure.name, learner.name, task.ids)

default = calculateDefault(surrogates, param.set = lrn.par.set[[6]]$param.set)
optimum = calculateDatasetOptimum(surrogate, hyperpar = "all", param.set = lrn.par.set[[6]]$param.set)

# Tunability overall
overallTunability = calculateTunability(default, optimum)
overallTunability

# Tunability hyperparameter specific
optimumHyperpar = calculateDatasetOptimum(surrogate, default, hyperpar = "one", param.set = lrn.par.set[[6]]$param.set, n.points = 10000)
tunability = calculateTunability(default, optimumHyperpar)
colMeans(tunability)

# Interactions
optimumTwoHyperpar = calculateDatasetOptimum(surrogate, hyperpar = "two")
tunability = calculateTunability(defaults, optimumHyperpar, optimumTwoHyperpar)

# Tuning Space
tuningSpace = calculateTuningSpace(surrogate, quantile)








