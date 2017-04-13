#' Create surrogate models for different tasks
#' @param measure.name Name of the measure to optimize
#' @param learner.name Name of learner
#' @param task.ids [\code{numeric}] ids of the tasks
#' @param lrn.par.set learner-parameter set which should include relevant bounds for flow
#' @param tbl.results df with getMlrRandomBotResults()
#' @param tbl.hypPars df with getMlrRandomBotHyperpars()
#' @param tbl.metaFeatures df with getMlrRandomBotHyperpars()
#' @param min.experiments minimum number of experiments that should be available for a dataset, otherwise the dataset is excluded
#' @return surrogate model
makeSurrogateModels = function(measure.name, learner.name, task.ids, tbl.results, tbl.hypPars, 
  tbl.metaFeatures, lrn.par.set, surrogate.mlr.lrn, min.experiments = 100){
  param.set = lrn.par.set[[which(names(lrn.par.set) == paste0(substr(learner.name, 5, 100), ".set"))]]$param.set
  
  #train mlr model on full table for measure
  mlr.mod.measure = list()
  task.data = makeBotTable(measure.name, learner.name, tbl.results, tbl.hypPars, tbl.metaFeatures = NULL)

  task.data = deleteNA(task.data)
  
  bigger = names(table(task.data$task.id))[which(table(task.data$task.id) > min.experiments)]
  task.data = task.data[task.data$task.id %in% bigger,]

  # get specific task ids
  if(!is.null(task.ids)) {
    uni = unique(task.data$task.id)
    task.ids = uni[uni %in% task.ids]
  } else {
    task.ids = unique(task.data$task.id)
  }
  
  for(i in seq_along(task.ids)) {
    print(paste("surrogate train: task", i, "of", length(task.ids)))
    task.idi = task.ids[i]
    
    
    mlr.task.measure = makeRegrTask(id = as.character(task.idi), subset(task.data, task.id == task.idi, select =  c("measure.value", names(param.set$pars))), target = "measure.value")
    mlr.lrn = surrogate.mlr.lrn
    mlr.mod.measure[[i]] = train(mlr.lrn, mlr.task.measure)
  }
  return(list(surrogates = mlr.mod.measure, param.set = param.set))
}


#' Merge results, hyperpars and features tables and prepare for mlr.task input
#' @param measure.name.filter What measure to analyse
#' @param learner.name What learner to analyse
#' @param tbl.results df with getMlrRandomBotResults()
#' @param tbl.hypPars df with getMlrRandomBotHyperpars()
#' @param tbl.metaFeatures df with getMlrRandomBotHyperpars()
#' @return [\code{data.frame}] Complete table used for creating the surrogate model 
makeBotTable = function(measure.name, learner.name, tbl.results, tbl.hypPars, tbl.metaFeatures){
  
  measure.name.filter = measure.name
  learner.name.fiter = learner.name
  bot.table = tbl.results %>% 
    filter(., measure.name == measure.name.filter & learner.name == learner.name.fiter) %>%
    inner_join(., tbl.hypPars, by = "run.id") %>%
    select(., -measure.name, -flow.name, -flow.id, -flow.source,
      -setup.id, -data.name, -upload.time, -flow.version, -learner.name) %>%
    spread(., key = hyperpar.name, value = hyperpar.value, convert = TRUE) %>%
    select(., -run.id)
  bot.table$measure.value = as.numeric(bot.table$measure.value)
  bot.table = convertDataFrameCols(bot.table, chars.as.factor = TRUE)
  
  return(bot.table)
}
