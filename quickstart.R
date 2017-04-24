library(devtools)
# replace this soon
load_all("/home/probst/Paper/Exploration_of_Hyperparameters/OMLbots")

# This has to be replaced by the database extraction (Daniel) ----------------------------------------------
library(devtools)
load_all()

tbl.results = getRunTable(run.tag = "botV1", numRuns = 200000)
print(head(tbl.results))

tbl.hypPars = getHyperparTable("botV1", excl.run.ids = NULL, numRuns = 200000, n = 1000)
#save(tbl.results, tbl.hypPars, file = "hypPars.RData")
tbl.hypPars3 = tbl.hypPars
for(i in 3:50) {
  tbl.hypPars2 = getHyperparTable("botV1", excl.run.ids = unique(tbl.hypPars3$run.id), numRuns = 200000, n = 10000)
  print(head(tbl.hypPars2))
  tbl.hypPars3 = rbind(tbl.hypPars3, tbl.hypPars2)
  save(tbl.hypPars3, file = paste0("hypPars",i,".RData"))
}

print(table(tbl.hypPars3$hyperpar.name))

tbl.hypPars = tbl.hypPars[which(tbl.hypPars$hyperpar.name != "verbose"), ]

tbl.results = tbl.results[tbl.results$run.id %in% unique(tbl.hypPars$run.id), ]
task.ids = unique(tbl.results$task.id)

tbl.metaFeatures = getMetaFeaturesTable(local.db = NULL)


convertMtry = function(tbl.results, tbl.hypPars) {
  features1 = listOMLTasks(limit = 50000)
  results1 = tbl.results[, c("run.id", "task.id")]
  features1 = features[, c("task.id", "number.of.features")]
  results1 = results1 %>%
    left_join(., features1, by = "task.id") %>%
    select(., -task.id)
  hypPars1 = tbl.hypPars[tbl.hypPars$hyperpar.name == "mtry", ] 
  hypPars1 = left_join(hypPars1, results1, by = "run.id")
  hypPars1 = unique(hypPars1)
  tbl.hypPars[tbl.hypPars$hyperpar.name == "mtry", ]$hyperpar.value = as.numeric(hypPars1$hyperpar.value) / hypPars1$number.of.features
  tbl.hypPars
}

tbl.hypPars = convertMtry(tbl.results, tbl.hypPars)

save(tbl.results, tbl.hypPars, file = "hypPars.RData")

load("hypPars.RData")
# -----------------------------------------------------------------------------------------------------------



library(stringi)
learner.names = paste0("mlr.", names(lrn.par.set))
learner.names = stri_sub(learner.names, 1, -5)

# Compare different surrogate models
surrogate.mlr.lrns = list(
  makeLearner("regr.rpart"),
  makeLearner("regr.ranger", par.vals = list(num.trees = 2000, respect.unordered.factors = TRUE)),
  #makeLearner("regr.xgboost", par.vals = list(nrounds = 300, eta = 0.03, max_depth = 2, nthread = 1)),
  #makeLearner("regr.svm"),
  #makeLearner("regr.bartMachine"),
  makeLearner("regr.cubist"),
  #makeLearner("regr.glmnet"), 
  makeLearner("regr.kknn") ,
  #makeLearner("regr.brnn"), # too many errors
  makeLearner("regr.lm")
  #makeLearner("regr.km")
)

bmr = list()

set.seed(123)
library("parallelMap")
parallelStartSocket(9)
for (i in seq_along(learner.names)) {
  print(i)
  bmr[[i]] = compareSurrogateModels(measure.name = "area.under.roc.curve", learner.name = learner.names[i], 
    task.ids = NULL, tbl.results, tbl.hypPars, tbl.metaFeatures = NULL, lrn.par.set, surrogate.mlr.lrns, min.experiments = 100)
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

# Best model in general: ranger, cubist

# Calculate tunability measures
surrogate.mlr.lrn = makeLearner("regr.ranger", par.vals = list(num.trees = 2000, respect.unordered.factors = TRUE))

results = surrogates_all = list()

set.seed(123)
for(i in seq_along(learner.names)) {
  print(i)

  # Surrogate model calculation
  surrogates = makeSurrogateModels(measure.name = "area.under.roc.curve", learner.name = learner.names[i], 
    task.ids = NULL, tbl.results, tbl.hypPars, tbl.metaFeatures = NULL, lrn.par.set, surrogate.mlr.lrn)
  
  #surrogates = getSurrogateModels(measure.name, learner.name, task.ids)
  
  # Default calculation
  default = calculateDefault(surrogates)
  # Tunability overall
  optimum = calculateDatasetOptimum(surrogates, hyperpar = "all", n.points = 10000)
  # Tunability hyperparameter specific
  optimumHyperpar = calculateDatasetOptimum(surrogates, default, hyperpar = "one", n.points = 10000)
  # Tunability for two hyperparameters
  optimumTwoHyperpar = calculateDatasetOptimum(surrogates, default, hyperpar = "two", n.points = 10000)
  # Tuning space
  tuningSpace = calculateTuningSpace(optimum, quant = 0.1)
    
  surrogates_all[[i]] = surrogates
  results[[i]] = list(default = default,  optimum = optimum, optimumHyperpar = optimumHyperpar, 
    optimumTwoHyperpar = optimumTwoHyperpar, tuningSpace = tuningSpace)
}
names(results) = learner.names
names(surrogates_all) = learner.names

save(bmr_surrogate, results, file = "results.RData")
save(surrogates_all, file = "surrogates.RData")

# Calculations
overallTunability = calculateTunability(default, optimum)
mean(overallTunability)
tunability = calculateTunability(default, optimumHyperpar)
data.frame(t(colMeans(tunability)))

colMeans(optimumTwoHyperpar[[4]]$optimum, dims = 1, na.rm = TRUE)
colMeans(optimum, dims = 1, na.rm = TRUE)
tunability = colMeans(calculateTunability(results$mlr.classif.rpart$default, results$mlr.classif.rpart$optimumHyperpar))

# Bare values
tab = colMeans(results$mlr.classif.rpart$optimumTwoHyperpar$optimum, dims = 1, na.rm = TRUE) - 
  mean(results$mlr.classif.rpart$default$result)

colnames(tab) = rownames(tab) = names(tunability)
# Interaction
colMeans(results$mlr.classif.glmnet$optimumTwoHyperpar$optimum, dims = 1, na.rm = TRUE) - 
  mean(results$mlr.classif.glmnet$default$result) - 
  outer(tunability, tunability, '+')
# Performance gain
colMeans(results$mlr.classif.glmnet$optimumTwoHyperpar$optimum, dims = 1, na.rm = TRUE) - 
  mean(results$mlr.classif.glmnet$default$result) - 
  outer(tunability, tunability, pmax)

# Annex




