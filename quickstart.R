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
# only models which do not have to be tuned!
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
task.ids = calculateTaskIds(tbl.results, tbl.hypPars, min.experiments = 200)

whole.table = inner_join(tbl.results, tbl.hypPars, by = "setup") %>% select(., task_id, fullName)
cross.table = table(whole.table$task_id, whole.table$fullName)
bigger = rowSums(cross.table > min.experiments)
task.ids = names(bigger)[bigger == 6] 

rownames(cross.table)[cross.table[,2] < 200]
c("272", "282", "3896", "3917", "7295", "9889", "9910", "9911", "9957", "9976", "34539", "145834", "145836", "145847", "145848",
 "145853", "145854", "145857", "145862", "145872", "145878", "145972", "145976", "145979", "146012", "146064", "146066", "146082", "146085")

measures = c("auc", "accuracy", "brier")

for(k in seq_along(measures)) {

configureMlr(show.info = TRUE, on.learner.error = "warn", on.learner.warning = "warn", on.error.dump = TRUE)
library("parallelMap")
parallelStartSocket(4)
for (i in seq_along(learner.names)) {
  print(i)
  set.seed(521 + i)
  if(i == 4) { # task.id 146085, 14966 does not work for svm
    bmr[[i]] = compareSurrogateModels(measure.name = measures[k], learner.name = learner.names[i], 
      task.ids = task.ids[-c(38,60)], tbl.results, tbl.metaFeatures,  tbl.hypPars, lrn.par.set, surrogate.mlr.lrns)
  } else {
  bmr[[i]] = compareSurrogateModels(measure.name = measures[k], learner.name = learner.names[i], 
    task.ids = task.ids, tbl.results, tbl.metaFeatures,  tbl.hypPars, lrn.par.set, surrogate.mlr.lrns)
  }
  gc()
  save(bmr, file = paste0("results_", measures[k], ".RData"))
}
parallelStop()
names(bmr) = learner.names


for(i in seq_along(bmr)) {
  print(i)
  rmat = convertBMRToRankMatrix(bmr[[i]])
  print(rmat)
  print(plotBMRSummary(bmr[[i]]))
  print(plotBMRBoxplots(bmr[[i]], style = "violin"))
  print(plotBMRRanksAsBarChart(bmr[[i]], pos = "stack"))
}
bmr_surrogate = bmr

# Save results
save(bmr_surrogate, file = paste0("results_", measures[k], ".RData"))

# Best model in general: ranger, cubist

################################# Calculate tunability measures
surrogate.mlr.lrn = makeLearner("regr.ranger", par.vals = list(num.trees = 2000, respect.unordered.factors = TRUE, num.threads = 4))
results = list()

task.ids = calculateTaskIds(tbl.results, tbl.hypPars, min.experiments = 200)

for(i in seq_along(learner.names)) {
  print(i)
    set.seed(199 + i)
  # Surrogate model calculation
  surrogates = makeSurrogateModels(measure.name = measures[k], learner.name = learner.names[i], 
    task.ids = task.ids, tbl.results, tbl.metaFeatures, tbl.hypPars, lrn.par.set, surrogate.mlr.lrn)
  save(surrogates, file = paste0("surrogates_", measures[k], "_", i, ".RData"))
}

for(i in seq_along(learner.names)) {
  print(i)
  set.seed(199 + i)
  load(paste0("surrogates_", measures[k], "_", i, ".RData"))
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
  save(bmr_surrogate, results, file = paste0("results_", measures[k], ".RData"))
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
tab = colMeans(results$mlr.classif.glmnet$optimumTwoHyperpar$optimum, dims = 1, na.rm = TRUE) - 
  mean(results$mlr.classif.glmnet$default$result)
diag(tab) = colMeans(tunability)
colnames(tab) = rownames(tab) = names(tunability)
tab
# Interaction
colMeans(results$mlr.classif.glmnet$optimumTwoHyperpar$optimum, dims = 1, na.rm = TRUE) - 
  mean(results$mlr.classif.glmnet$default$result) - 
  outer(colMeans(tunability), colMeans(tunability), '+')
# Performance gain
colMeans(results$mlr.classif.glmnet$optimumTwoHyperpar$optimum, dims = 1, na.rm = TRUE) - 
  mean(results$mlr.classif.glmnet$default$result) - 
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
  load(paste0("surrogates_", measures[k], "_", i, ".RData"))
  
  def = package.defaults[[i]]
  default = calculatePackageDefaultPerformance(surrogates, def, tbl.metaFeatures, tbl.results)
  optimumHyperpar = calculateDatasetOptimumPackageDefault(surrogates, default, hyperpar = "one", n.points = 100000, tbl.metaFeatures, tbl.results)
  optimumTwoHyperpar = calculateDatasetOptimumPackageDefault(surrogates, default, hyperpar = "two", n.points = 10000, tbl.metaFeatures, tbl.results)
    resultsPackageDefaults[[i]] = list(default = default,  optimumHyperpar = optimumHyperpar, optimumTwoHyperpar = optimumTwoHyperpar)
  save(bmr_surrogate, results, resultsPackageDefaults, file = paste0("results_", measures[k], ".RData"))
}
names(resultsPackageDefaults) = learner.names

resultsPackageDefaults$mlr.classif.svm$default$default$gamma = "1/p"
resultsPackageDefaults$mlr.classif.ranger$default$default$mtry = "sqrt(p)"

save(bmr_surrogate, results, resultsPackageDefaults, file = paste0("results_", measures[k], ".RData"))

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

# KI for tunability
y = overallTunability
hist(y)
qqnorm(y)
qqline(y)

t_value = qt(0.975, length(y) - 1)
mean(y) + c(-t_value, t_value) * sd(y) / sqrt(length(y))

# Tunability of the "algorithm"; overfitting problem!
the_order = order(results[[5]]$default$result)
plot(results$mlr.classif.glmnet$default$result[the_order], type = "l", ylab = "AUC")
avg_results = numeric(6)
best_results = best_results_default = numeric(length(results$mlr.classif.glmnet$default$result))

for(i in seq_along(learner.names)) {
  lines(results[[i]]$default$result[the_order], col = i)
  avg_results[i] = mean(results[[i]]$default$result)
  for(j in 1:length(results[[i]]$default$result)) {
    best_results_default[j] = ifelse(results[[i]]$default$result[j] > best_results[j], results[[i]]$default$result[j], best_results[j])
    best_results[j] = ifelse(results[[i]]$optimum$optimum[j] > best_results[j], results[[i]]$optimum$optimum[j], best_results[j])
  }
}
legend("topleft", legend = substr(learner.names, 13, 100), col = 1:6, lty = 1)

round(best_results - (results[[5]]$default$result), 3)
mean(best_results_default - (results[[5]]$default$result))
mean(best_results - (results[[5]]$default$result))

mean((results[[5]]$default$result) - (results[[6]]$default$result))
# maybe overfitting! 

# Make Crossvalidation to test if there is overfitting
results_cv = list()
for(i in 1:6) {
  print(i)
  set.seed(3000 + i)
  load(paste0("surrogates_", measures[k], "_", i, ".RData"))
  
  # CV
  n_surr = length(surrogates$surrogates)
  shuffle = sample(n_surr)
  folds = cut(shuffle, breaks = 5, labels = FALSE)
  
  default = list()
  optimumHyperpar = list()
  optimumTwoHyperpar = list()
  
  for(j in 1:5) {
    print(paste(j,i))
    testInd = which(folds == j, arr.ind = TRUE)
    trainInd = which(folds != j, arr.ind = TRUE)
    
    # Default calculation
    default1 = calculateDefault(surrogates = list(surrogates = surrogates$surrogates[trainInd], param.set = surrogates$param.set))
    # Calculate performance of these defaults on test datasets
    default[[j]] = calculatePerformance(list(surrogates = surrogates$surrogates[testInd], param.set = surrogates$param.set), default1$default)
    # Tunability hyperparameter specific
    optimumHyperpar[[j]] = calculateDatasetOptimum(surrogates = list(surrogates = surrogates$surrogates[testInd], param.set = surrogates$param.set), default[[j]], hyperpar = "one", n.points = 100000)
    # Tunability for two hyperparameters
    optimumTwoHyperpar[[j]] = calculateDatasetOptimum(list(surrogates = surrogates$surrogates[testInd], param.set = surrogates$param.set), default[[j]], hyperpar = "two", n.points = 10000)
    
    results_cv[[i]] = list(default = default, optimumHyperpar = optimumHyperpar, optimumTwoHyperpar = optimumTwoHyperpar)
    gc()
  }
  save(results, resultsPackageDefaults, results_cv, file = paste0("results_", measures[k], ".RData"))
}
names(results_cv) = learner.names
save(bmr_surrogate, results, resultsPackageDefaults, results_cv, file = paste0("results_", measures[k], ".RData"))
}

# overall tunability, cross-validated
for(i in seq_along(learner.names)){
  print(learner.names[i])
  print(mean(calculateTunability(results[[i]]$default, results[[i]]$optimum)))
  print(mean(results[[i]]$optimum$optimum - c(sapply(results_cv[[i]]$default, "[[", 2))))
}
for(i in seq_along(learner.names)){
  print(learner.names[i])
  print(rbind(colMeans(calculateTunability(results[[i]]$default, results[[i]]$optimumHyperpar)),
  colMeans(do.call(rbind, unlist(results_cv[[i]]$optimumHyperpar, recursive=FALSE)) - c(sapply(results_cv[[i]]$default, "[[", 2)))))
}


# Annex