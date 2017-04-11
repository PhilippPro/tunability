#' Compare different surrogate models
#' @param measure.name Name of the measure to optimize
#' @param learner.name Name of learner
#' @param task.ids [\code{numeric}] ids of the tasks
#' @param lrn.par.set learner-parameter set which should include relevant bounds for flow
#' @param tbl.results df with getMlrRandomBotResults()
#' @param tbl.hypPars df with getMlrRandomBotHyperpars()
#' @param tbl.metaFeatures df with getMlrRandomBotHyperpars()
#' @param surrogate.mlr.lrns list of mlr learners that should be compared
#' @return surrogate model
compareSurrogateModels = function(measure.name, learner.name, task.ids, tbl.results, 
  tbl.hypPars, tbl.metaFeatures, lrn.par.set, surrogate.mlr.lrns){
  
  param.set = lrn.par.set[[which(names(lrn.par.set) == paste0(substr(learner.name, 5, 100), ".set"))]]$param.set
  
  #train mlr model on full table for measure
  mlr.mod.measure = list()
  task.data = makeBotTable(measure.name, learner.name, tbl.results, tbl.hypPars, tbl.metaFeatures = NULL)
  task.data = deleteNA(task.data)
  
  # get specific task ids
  if(!is.null(task.ids)) {
    uni = unique(task.data$task.id)
    task.ids = uni[uni %in% task.ids]
  } else {
    task.ids = unique(task.data$task.id)
  }
  
  mlr.tasks = list()
  for(i in seq_along(task.ids)) {
    task.idi = task.ids[i]
    mlr.tasks[[i]] = makeRegrTask(id = as.character(task.idi), subset(task.data, task.id == task.idi, select =  c("measure.value", names(param.set$pars))), target = "measure.value")
  }
  mlr.lrns = surrogate.mlr.lrns
  rdesc = makeResampleDesc("RepCV", reps = 2, folds = 10)
  mlr.benchmark = benchmark(mlr.lrns, mlr.tasks, resamplings = rdesc)
  
  lrns = list(makeLearner("classif.lda"), makeLearner("classif.rpart"))
  tasks = list(iris.task, sonar.task)
  rdesc = makeResampleDesc("CV", iters = 2L)
  meas = list(acc, ber)
  bmr = benchmark(lrns, tasks, rdesc, measures = meas)
  
  return(mlr.benchmark)
}