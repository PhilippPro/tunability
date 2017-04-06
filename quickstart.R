library(devtools)
# replace this soon
load_all("/home/probst/Paper/Exploration_of_Hyperparameters/OMLbots")

# This has to be replaced by the database extraction (Daniel) ----------------------------------------------
library(devtools)
load_all()

tbl.results = getRunTable("botV1")
print(head(tbl.results))

tbl.hypPars = getHyperparTable("botV1")
print(head(tbl.hypPars))

tbl.results = tbl.results[tbl.results$run.id %in% unique(tbl.hypPars$run.id),]
task.ids = unique(tbl.results$task.id)

surrogates = makeSurrogateModels(measure.name = "area.under.roc.curve", learner.name = "mlr.classif.rpart", 
  task.ids = task.ids, tbl.results, tbl.hypPars, param.set = lrn.par.set[[2]]$param.set)

#surrogates = getSurrogateModels(measure.name, learner.name, task.ids)

defaults = calculateDefault(surrogates, param.set = lrn.par.set[[2]]$param.set)
optimum = calculateDatasetOptimum(surrogate, hyperpar = "all")

# Tunability overall
overallTunability = calculateTunability(defaults, optimum)
overallTunability

# Tunability hyperparameter specific
optimumHyperpar = calculateDatasetOptimum(surrogate, hyperpar = "one")
tunability = calculateTunability(defaults, optimumHyperpar)

# Interactions
optimumTwoHyperpar = calculateDatasetOptimum(surrogate, hyperpar = "two")
tunability = calculateTunability(defaults, optimumHyperpar, optimumTwoHyperpar)

# Tuning Space
tuningSpace = calculateTuningSpace(surrogate, quantile)








