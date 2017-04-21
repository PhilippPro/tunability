library(devtools)
# replace this soon
load_all("/home/probst/Paper/Exploration_of_Hyperparameters/OMLbots")

# This has to be replaced by the database extraction ----------------------------------------------
library(devtools)
load_all()
load("results.RData")
print(results$mlr.classif.xgboost$default$default)
# Set the learner
library(mlr)
lrn = makeLearner("classif.xgboost", predict.type = "prob")
lrn = setHyperPars(lrn, 
  nrounds = 2726,
  subsample = 0.9338,
  booster = "gbtree",
  max_depth = 9,
  min_child_weight = 3.5806,
  colsample_bytree = 0.8357,
  colsample_bylevel = 0.7025,
  lambda = 13.6807,
  alpha = 3.1665
)
# 
ps = makeParamSet(
  #makeIntegerParam("nrounds", lower = 200, upper = 2500, default = 200),
  makeNumericParam("eta", lower = -10, upper = 0, default = -6, 
    trafo = function(x) 2^x)
  # makeIntegerParam("max_depth", lower = 3, upper = 15, default = 3),
  # makeNumericParam("colsample_bytree", lower = 0.3, upper = 1, default = 0.6),
  # makeNumericParam("subsample", lower = 0.3, upper = 1, default = 0.6)
)

 library(mlrMBO)
 mbo.ctrl = makeMBOControl(save.on.disk.at = c(0, 5, 10, 20, 50, 75, 85, 95, 100, 101))
 mbo.ctrl = setMBOControlTermination(mbo.ctrl, iters = 100)
 surrogate.lrn = makeLearner("regr.randomForest", predict.type = "se")
 ctrl = mlr:::makeTuneControlMBO(learner = surrogate.lrn, mbo.control = mbo.ctrl)


#ctrl = makeTuneControlGrid(resolution = 10L)

library(parallelMap)

tasks = listOMLTasks(number.of.classes = 2L, number.of.missing.values = 0, 
  data.tag = "study_14", estimation.procedure = "10-fold Crossvalidation")

res.mbo = sys = list()
res.mbo2 = sys2 = list()
# remove too big datasets (more than 2 GB)
tasks = tasks[-c(26, 48, 50), ]
# Identify the ideal eta for each task!
for(i in seq_along(tasks$task.id)) {
  print(i)
  task = getOMLTask(tasks$task.id[i])
  task = convertOMLTaskToMlr(task)$mlr.task

  cols = which(colnames(task$env$data) != task$task.desc$target)
  target = task$task.desc$target
  # this would be correct!!!
  data = cbind(sapply(dummy.data.frame(task$env$data[,cols], sep = "_._"), as.numeric), task$env$data[task$task.desc$target])
  
  colnames(data) = make.names(colnames(data))
  
  task = makeClassifTask(data = data, target = target)
  
sys2[[i]] = system.time(res.mbo2[[i]] <-  train(lrn, task))
  
#  parallelStartSocket(10)
#  sys[[i]] = system.time(res.mbo[[i]] <- tuneParams(lrn, task, cv10, par.set = ps, control = ctrl,
#    show.info = TRUE, measures = auc))
#  parallelStop()
}

# Evaluate the ideal eta identified by mbo, the default and the ideal default with surrogate model with resampling
sum(sapply(sys2, "[[", 3)) # nrounds = 10, 10 evals: 1,5 h.

