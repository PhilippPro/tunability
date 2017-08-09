library(devtools)
# replace this by database from web?
load_all("/home/probst/Paper/Exploration_of_Hyperparameters/OMLbots")
#load_all("C:/Promotion/Hyperparameters/OMLbots")
load_all()
lrn.par.set = getMultipleLearners()
# Database extraction
path = paste0("/home/probst/Paper/Exploration_of_Hyperparameters/OMLbots", "/mlrRandomBotDatabaseSnapshot.db")
local.db = src_sqlite(path, create = FALSE)

tbl.results = collect(tbl(local.db, sql("SELECT * FROM [tbl.results]")), n = Inf)
tbl.metaFeatures = collect(tbl(local.db, sql("SELECT * FROM [tbl.metaFeatures]")), n = Inf)
tbl.hypPars = collect(tbl(local.db, sql("SELECT * FROM [tbl.hypPars]")), n = Inf)

library(stringi)
learner.names = paste0("mlr.", names(lrn.par.set))
learner.names = stri_sub(learner.names, 1, -5)

################################ Compare different surrogate models
surrogate.mlr.lrns = list(
  makeLearner("regr.lm"),
  makeLearner("regr.rpart"),
  makeLearner("regr.kknn"),
  makeLearner("regr.ranger", par.vals = list(num.trees = 2000, respect.unordered.factors = TRUE)),
  makeLearner("regr.cubist")
  #makeLearner("regr.xgboost", par.vals = list(nrounds = 300, eta = 0.03, max_depth = 2, nthread = 1)),
  #makeLearner("regr.svm"),
  #makeLearner("regr.bartMachine"),
  #makeLearner("regr.glmnet"), 
  #makeLearner("regr.brnn"), # too many errors
  #makeLearner("regr.km")
)

bmr = list()

library("parallelMap")
parallelStartSocket(8)
for (i in seq_along(learner.names)) {
  print(i)
  set.seed(123 + i)
  bmr[[i]] = compareSurrogateModels(measure.name = "area.under.roc.curve", learner.name = learner.names[i], 
    task.ids = NULL, tbl.results, tbl.metaFeatures,  tbl.hypPars, lrn.par.set, surrogate.mlr.lrns, min.experiments = 100)
  gc()
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


################################# Calculate tunability measures
surrogate.mlr.lrn = makeLearner("regr.ranger", par.vals = list(num.trees = 2000, respect.unordered.factors = TRUE))
results = surrogates_all = list()

for(i in seq_along(learner.names)) {
  print(i)
  set.seed(124 + i)
  # Surrogate model calculation
  surrogates = makeSurrogateModels(measure.name = "area.under.roc.curve", learner.name = learner.names[i], 
    task.ids = NULL, tbl.results, tbl.hypPars, tbl.metaFeatures = NULL, lrn.par.set, surrogate.mlr.lrn)
  
  
  surrogates = makeSurrogateModels(measure.name = "area.under.roc.curve", learner.name = learner.names[i], 
    task.ids = NULL, tbl.results, tbl.metaFeatures,  tbl.hypPars, lrn.par.set, surrogate.mlr.lrn, min.experiments = 100)
  #surrogates = getSurrogateModels(measure.name, learner.name, task.ids)

  # Default calculation
  default = calculateDefault(surrogates)
  # Tunability overall
  optimum = calculateDatasetOptimum(surrogates, default, hyperpar = "all", n.points = 100000)
  # Tunability hyperparameter specific
  optimumHyperpar = calculateDatasetOptimum(surrogates, default, hyperpar = "one", n.points = 100000)
  # Tunability for two hyperparameters
  optimumTwoHyperpar = calculateDatasetOptimum(surrogates, default, hyperpar = "two", n.points = 10000)
  # Tuning space
  tuningSpace = calculateTuningSpace(optimum, quant = 0.1)
    
  surrogates_all[[i]] = surrogates
  results[[i]] = list(default = default,  optimum = optimum, optimumHyperpar = optimumHyperpar, 
    optimumTwoHyperpar = optimumTwoHyperpar, tuningSpace = tuningSpace)
  gc()
}
names(results) = learner.names
names(surrogates_all) = learner.names

save(bmr_surrogate, results, file = "results.RData")
save(surrogates_all, file = "surrogates.RData")

# Calculations
default = results$mlr.classif.ranger$default
optimum = results$mlr.classif.ranger$optimum
optimumHyperpar = results$mlr.classif.ranger$optimumHyperpar
overallTunability = calculateTunability(default, optimum)
mean(overallTunability)
tunability = calculateTunability(default, optimumHyperpar)
# scaled
data.frame(t(colMeans(tunability/overallTunability, na.rm = T)))
data.frame(t(colMeans(tunability)))

# Interaction
# Bare values
tab = colMeans(results$mlr.classif.ranger$optimumTwoHyperpar$optimum, dims = 1, na.rm = TRUE) - 
  mean(results$mlr.classif.ranger$default$result)
diag(tab) = colMeans(tunability)
colnames(tab) = rownames(tab) = names(tunability)
tab
# Interaction
colMeans(results$mlr.classif.ranger$optimumTwoHyperpar$optimum, dims = 1, na.rm = TRUE) - 
  mean(results$mlr.classif.ranger$default$result) - 
  outer(colMeans(tunability), colMeans(tunability), '+')
# Performance gain
colMeans(results$mlr.classif.ranger$optimumTwoHyperpar$optimum, dims = 1, na.rm = TRUE) - 
  mean(results$mlr.classif.ranger$default$result) - 
  outer(colMeans(tunability), colMeans(tunability), pmax)

# Package defaults
package.defaults = list(
  glmnet = data.frame(alpha = 1, lambda = 0), # no regularization
  rpart = data.frame(cp = 0.01, maxdepth = 30, minbucket = 7, minsplit = 20),
  kknn = data.frame(k = 7),
  svm = data.frame(kernel = "radial", cost = 1, gamma = 1, degree = 3), 
  ranger = data.frame(num.trees = 500, replace = TRUE, sample.fraction = 1, mtry  = 0.1, respect.unordered.factors = FALSE),
  xgboost = data.frame(nrounds = 500, eta = 0.3, subsample = 1, booster = "gbtree", max_depth = 6, min_child_weight = 1,
    colsample_bytree = 1, colsample_bylevel = 1, lambda = 1, alpha = 1)
)

# Parameters dependent on data characteristics: svm: gamma, ranger: mtry. 
# Not Specified: glmnet: alpha, xgboost: nrounds
tbl.metaFeatures = listOMLTasks(limit = 50000)
resultsPackageDefaults = list()

set.seed(123)
for(i in seq_along(learner.names)) {
  print(i)
  surrogates = surrogates_all[[i]]
  def = package.defaults[[i]]
  default = calculatePackageDefaultPerformance(surrogates, def, tbl.metaFeatures)
  optimumHyperpar = calculateDatasetOptimumPackageDefault(surrogates, default, hyperpar = "one", n.points = 100000, tbl.metaFeatures)
  optimumTwoHyperpar = calculateDatasetOptimumPackageDefault(surrogates, default, hyperpar = "two", n.points = 10000, tbl.metaFeatures)
    resultsPackageDefaults[[i]] = list(default = default,  optimumHyperpar = optimumHyperpar, optimumTwoHyperpar = optimumTwoHyperpar)
}
names(resultsPackageDefaults) = learner.names

resultsPackageDefaults$mlr.classif.svm$default$default$gamma = "1/p"
resultsPackageDefaults$mlr.classif.ranger$default$default$mtry = "sqrt(p)"

save(bmr_surrogate, results, resultsPackageDefaults, file = "results.RData")

# Calculations
default = resultsPackageDefaults$mlr.classif.ranger$default
optimum = results$mlr.classif.ranger$optimum
optimumHyperpar = resultsPackageDefaults$mlr.classif.ranger$optimumHyperpar
overallTunability = calculateTunability(default, optimum)
mean(overallTunability)

tunability = calculateTunability(default, optimumHyperpar)

data.frame(t(colMeans(tunability)))
# scaled
data.frame(t(colMeans(tunability/overallTunability, na.rm = T)))




# Annex