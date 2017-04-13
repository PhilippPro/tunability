# library(mlr)
# 
# lrn = makeLearner("classif.xgboost", eval_metric = "auc", predict.type = "prob")
# 
# ps = makeParamSet(
#   makeIntegerParam("nrounds", lower = 200, upper = 2500, default = 200),
#   makeNumericParam("eta", lower = -7, upper = -5, default = -6, 
#     trafo = function(x) 2^x),
#   makeIntegerParam("max_depth", lower = 3, upper = 15, default = 3),
#   makeNumericParam("colsample_bytree", lower = 0.3, upper = 1, default = 0.6),
#   makeNumericParam("subsample", lower = 0.3, upper = 1, default = 0.6)
# )
# 
# 
# library(mlrMBO)
# library(parallelMap)
# 
# 
# task = makeClassifTask(data = data, target = "churn")
# 
# 
# mbo.ctrl = makeMBOControl(save.on.disk.at = c(0, 5, 10, 20, 50, 75, 85, 95))
# mbo.ctrl = setMBOControlTermination(mbo.ctrl, iters = 100)
# surrogate.lrn = makeLearner("regr.randomForest", predict.type = "se")
# ctrl = mlr:::makeTuneControlMBO(learner = surrogate.lrn, mbo.control = mbo.ctrl)
# 
# parallelStartMulticore(cpus = 10L)
# res.mbo = tuneParams(lrn, task, cv10, par.set = ps, control = ctrl, 
#   show.info = TRUE, measures = auc)
# parallelStop()
# 
