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
  tbl.metaFeatures, lrn.par.set, surrogate.mlr.lrn, min.experiments = 100) {
  param.set = lrn.par.set[[which(names(lrn.par.set) == paste0(substr(learner.name, 5, 100), ".set"))]]$param.set
  
  #train mlr model on full table for measure
  mlr.mod.measure = list()
  task.data = makeBotTable(measure.name, learner.name, tbl.results, tbl.hypPars)

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
makeBotTable = function(measure.name, learner.name, tbl.results, tbl.metaFeatures, tbl.hypPars, param.set) {
  
  measure.name.filter = measure.name
  
  tbl.hypPars.learner = tbl.hypPars[tbl.hypPars$fullName == learner.name, ]
  tbl.hypPars.learner = spread(tbl.hypPars.learner, name, value)
  tbl.hypPars.learner = data.frame(tbl.hypPars.learner)
  # Convert the columns to the specific classes
  params = getParamIds(param.set)
  param_types = getParamTypes(param.set)
  for(i in seq_along(params))
    tbl.hypPars.learner[, params[i]] = conversion_function(tbl.hypPars.learner[, params[i]], param_types[i])
  
  bot.table = inner_join(tbl.results, tbl.hypPars.learner, by = "setup") %>%
    select(., -run_id, -setup, -fullName)

  if(learner.name == "mlr.classif.ranger"){
    n_feats = filter(tbl.metaFeatures, quality == "NumberOfFeatures") %>%
      select(., -quality)
    n_feats$value = as.numeric(n_feats$value)
    
    bot.table = inner_join(bot.table, n_feats, by = "data_id")
    bot.table$mtry = bot.table$mtry/bot.table$value
    bot.table = bot.table %>% select(., -value)
  }
  
  bot.table = inner_join(tbl.results, tbl.hypPars.learner, by = "setup") %>%
    select(., -data_id)
  
  colnames(bot.table)[4] = "measure.value"
  bot.table$measure.value = as.numeric(bot.table$measure.value)
  
  return(bot.table)
}


conversion_function = function(x, param_type) {
  if(param_type %in% c("integer", "numeric", "numericvector")) 
    x = as.numeric(x)
  if(param_type %in% c("character", "logical", "factor"))
    x = as.factor(x)
  return(x)
}
