library(devtools)
# replace this by database from web?
OMLbots_path = "/home/probst/Paper/Exploration_of_Hyperparameters/OMLbots"
# OMLbots_path = "C:/Promotion/Hyperparameters/OMLbots"
load_all(OMLbots_path)
load_all()
lrn.par.set = getMultipleLearners()
# Database extraction
path = paste0(OMLbots_path, "/mlrRandomBotDatabaseSnapshot.db")
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
surrogate.mlr.lrn = makeLearner("regr.ranger", par.vals = list(num.trees = 2000, respect.unordered.factors = TRUE, num.threads = 4))
results = list()

task.ids = calculateTaskIds(tbl.results, tbl.hypPars, min.experiments = 200)

for(i in 1:6) {
  print(i)
    set.seed(199 + i)
  # Surrogate model calculation
  surrogates = makeSurrogateModels(measure.name = "area.under.roc.curve", learner.name = learner.names[i], 
    task.ids = task.ids, tbl.results, tbl.metaFeatures, tbl.hypPars, lrn.par.set, surrogate.mlr.lrn)
  save(surrogates, file = paste0("surrogates_",i, ".RData"))
}

for(i in 1:6) {
  print(i)
  set.seed(199 + i)
  load(paste0("surrogates_",i, ".RData"))
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
    
  results[[i]] = list(default = default,  optimum = optimum, optimumHyperpar = optimumHyperpar, 
    optimumTwoHyperpar = optimumTwoHyperpar, tuningSpace = tuningSpace)
  gc()
  save(results, file = "results.RData")
}
names(results) = learner.names

# Calculations
default = results$mlr.classif.glmnet$default
optimum = results$mlr.classif.glmnet$optimum
optimumHyperpar = results$mlr.classif.glmnet$optimumHyperpar
overallTunability = calculateTunability(default, optimum)
mean(overallTunability)
tunability = calculateTunability(default, optimumHyperpar)
data.frame(t(colMeans(tunability)))
# scaled
data.frame(t(colMeans(tunability/overallTunability, na.rm = T)))

# Interaction
# Bare values
tab = colMeans(results$mlr.classif.rpart$optimumTwoHyperpar$optimum, dims = 1, na.rm = TRUE) - 
  mean(results$mlr.classif.rpart$default$result)
diag(tab) = colMeans(tunability)
colnames(tab) = rownames(tab) = names(tunability)
tab
# Interaction
colMeans(results$mlr.classif.rpart$optimumTwoHyperpar$optimum, dims = 1, na.rm = TRUE) - 
  mean(results$mlr.classif.rpart$default$result) - 
  outer(colMeans(tunability), colMeans(tunability), '+')
# Performance gain
colMeans(results$mlr.classif.rpart$optimumTwoHyperpar$optimum, dims = 1, na.rm = TRUE) - 
  mean(results$mlr.classif.rpart$default$result) - 
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
resultsPackageDefaults = list()

for(i in seq_along(learner.names)) {
  print(i)
  set.seed(199 + i)
  load(paste0("surrogates_",i, ".RData"))
  
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
default = resultsPackageDefaults$mlr.classif.rpart$default
optimum = results$mlr.classif.rpart$optimum
optimumHyperpar = resultsPackageDefaults$mlr.classif.rpart$optimumHyperpar
overallTunability = calculateTunability(default, optimum)
mean(overallTunability)

tunability = calculateTunability(default, optimumHyperpar)

data.frame(t(colMeans(tunability)))
# scaled
data.frame(t(colMeans(tunability/overallTunability, na.rm = T)))




# Annex