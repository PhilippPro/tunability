library(devtools)
# replace this soon
load_all("/home/probst/Paper/Exploration_of_Hyperparameters/OMLbots")

# This has to be replaced by the database extraction (Daniel) ----------------------------------------------
library(devtools)
load_all()

tbl.results = getRunTable(run.tag = "botV1", numRuns = 200000)
print(head(tbl.results))

tbl.hypPars = getHyperparTable("botV1", excl.run.ids = NULL, numRuns = 200000, n = 1000)
#save(tbl.hypPars, file = "hypPars.RData")
tbl.hypPars3 = tbl.hypPars
for(i in 3:50) {
  tbl.hypPars2 = getHyperparTable("botV1", excl.run.ids = unique(tbl.hypPars3$run.id), numRuns = 200000, n = 10000)
  print(head(tbl.hypPars2))
  tbl.hypPars3 = rbind(tbl.hypPars3, tbl.hypPars2)
  save(tbl.hypPars3, file = paste0("hypPars",i,".RData"))
}

print(table(tbl.hypPars3$hyperpar.name))

tbl.hypPars = tbl.hypPars[which(tbl.hypPars$hyperpar.name != "verbose"),]

tbl.results = tbl.results[tbl.results$run.id %in% unique(tbl.hypPars$run.id), ]
task.ids = unique(tbl.results$task.id)

load("hypPars.RData")
# -----------------------------------------------------------------------------------------------------------

library(stringi)
learner.names = paste0("mlr.", names(lrn.par.set))
learner.names = stri_sub(learner.names, 1, -5)

# Compare different surrogate models
surrogate.mlr.lrns = list(
  makeLearner("regr.rpart"),
  makeLearner("regr.ranger", par.vals = list(num.trees = 2000)),
  #makeLearner("regr.xgboost", par.vals = list(nrounds = 300, eta = 0.03, max_depth = 2, nthread = 1)),
  makeLearner("regr.svm"),
  #makeLearner("regr.bartMachine"),
  makeLearner("regr.cubist"),
  #makeLearner("regr.glmnet"), 
  makeLearner("regr.kknn") ,
  #makeLearner("regr.brnn"), # too many errors
  makeLearner("regr.lm")
  #makeLearner("regr.km")
)

bmr = list()

library("parallelMap")
parallelStartSocket(9)
for (i in seq_along(learner.names)) {
  bmr[[i]] = compareSurrogateModels(measure.name = "area.under.roc.curve", learner.name = learner.names[i], 
    task.ids = NULL, tbl.results, tbl.hypPars, tbl.metaFeatures = NULL, lrn.par.set, surrogate.mlr.lrns)
}
names(bmr) = learner.names
parallelStop()

for(i in seq_along(bmr)) {
rmat = convertBMRToRankMatrix(bmr[[i]])
print(rmat)
print(plotBMRSummary(bmr[[i]]))
print(plotBMRBoxplots(bmr[[i]], style = "violin"))
print(plotBMRRanksAsBarChart(bmr[[i]], pos = "stack"))
}
bmr_surrogate = bmr

# Save results
save(bmr_surrogate, file = "results.RData")

# Best model in general: 

# Calculate tunability measures
surrogate.mlr.lrn = makeLearner("regr.ranger", par.vals = list(num.trees = 2000))

results = list()

for(i in seq_along(learner.names)) {
  print(i)

  surrogates = makeSurrogateModels(measure.name = "area.under.roc.curve", learner.name = learner.names[i], 
    task.ids = NULL, tbl.results, tbl.hypPars, tbl.metaFeatures = NULL, lrn.par.set, surrogate.mlr.lrn)
  
  #surrogates = getSurrogateModels(measure.name, learner.name, task.ids)
  
  default = calculateDefault(surrogates)

  # Tunability overall
  optimum = calculateDatasetOptimum(surrogates, hyperpar = "all")
  overallTunability = calculateTunability(default, optimum)
  overallTunability = mean(overallTunability)
  
  # Tunability hyperparameter specific
  optimumHyperpar = calculateDatasetOptimum(surrogates, default, hyperpar = "one", n.points = 10000)
  tunability = calculateTunability(default, optimumHyperpar)
  tunability = colMeans(tunability)
  
  # Tuning space
  tuningSpace = calculateTuningSpace(optimum, quant = 0.1)
  
  results[[i]] = list(surrogates = surrogates, default = default, overallTunability = overallTunability, 
    tunability = tunability, tuningSpace = tuningSpace)
}
names(results) = learner.names



save(bmr_surrogate, results, file = "results.RData")

# Annex

# Interactions
optimumTwoHyperpar = calculateDatasetOptimum(surrogate, hyperpar = "two")
tunability = calculateTunability(defaults, optimumHyperpar, optimumTwoHyperpar)

# Tuning Space









