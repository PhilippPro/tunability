#' Compare different surrogate models
#' @param measure.name Name of the measure to optimize
#' @param learner.name Name of learner
#' @param data.ids [\code{numeric}] ids of the dataset
#' @param lrn.par.set learner-parameter set which should include relevant bounds for flow
#' @param tbl.results df with getMlrRandomBotResults()
#' @param tbl.hypPars df with getMlrRandomBotHyperpars()
#' @param tbl.metaFeatures df with getMlrRandomBotHyperpars()
#' @param surrogate.mlr.lrns list of mlr learners that should be compared
#' @param min.experiments minimum number of experiments that should be available for a dataset, otherwise the dataset is excluded
#' @return surrogate model
compareSurrogateModels = function(measure.name, learner.name, data.ids, tbl.results, 
  tbl.metaFeatures, tbl.hypPars, lrn.par.set, surrogate.mlr.lrns) {
  
  param.set = lrn.par.set[[which(names(lrn.par.set) == paste0(substr(learner.name, 5, 100), ".set"))]]$param.set
  #train mlr model on full table for measure
  task.data = makeBotTable(measure.name, learner.name, tbl.results, tbl.metaFeatures, tbl.hypPars, param.set, data.ids)
  task.data = data.frame(task.data)
  task.data = deleteNA(task.data)
  
  # get specific data ids
  if(!is.null(data.ids)) {
    uni = unique(task.data$data_id)
    task.ids = uni[uni %in% data.ids]
  } else {
    task.ids = unique(task.data$data_id)
  }
  
  mlr.tasks = list()
  for(i in seq_along(data.ids)) {
    data.idi = data.ids[i]
    data = subset(task.data, data_id == data.ids[i], select =  c("measure.value", names(param.set$pars)))
    # Rename column names because of weird "sample" behaviour of cubist
    colnames(data) = gsub("sample", "ampel", colnames(data))
    mlr.tasks[[i]] = makeRegrTask(id = as.character(data.idi), data, target = "measure.value")
  }
  mlr.lrns = surrogate.mlr.lrns
  measures = list(mse, rsq, kendalltau, spearmanrho)
  rdesc = makeResampleDesc("RepCV", reps = 10, folds = 5)
  mlr.benchmark = benchmark(mlr.lrns, mlr.tasks, resamplings = rdesc, keep.pred = FALSE, models = FALSE, measures = measures)
  
  return(mlr.benchmark)
}