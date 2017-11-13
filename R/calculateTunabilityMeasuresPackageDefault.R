#' Calculate default hyperparameter setting
#' @param surrogates Surrogate models
#' @param def Package defaults
calculatePackageDefaultPerformance = function(surrogates, def, tbl.metaFeatures, tbl.results) {
  surr = surrogates$surrogates
  preds = numeric(length(surr))
  for(i in seq_along(surr)) {
    print(paste("surrogate predict: task", i, "of", length(surr)))
    default = convertPackageDefault(def, surr[[i]], tbl.metaFeatures, tbl.results)
    preds[i] = predict(surr[[i]], newdata = default)$data$response
  }
  # Best default

  list(default = default, result = preds)
}

convertPackageDefault = function(def, surr, tbl.metaFeatures, tbl.results) {
  task_idi = surr$task.desc$id
  
  matching_task_data = unique(tbl.results[, c("task_id", "data_id")])
  n_feats = filter(tbl.metaFeatures, quality == "NumberOfFeatures") %>%
    select(., -quality) %>%
    inner_join(., matching_task_data, by = "data_id")
  p = as.numeric(filter(n_feats, task_id == task_idi)$value)
  
  if ("mtry" %in% names(def)) {
    def$mtry = floor(sqrt(p))/p
  }
  if ("gamma" %in% names(def)) {
    def$gamma = 1/p
  }
  def
}

#' Calculate optimal hyperparameter values for an algorithm
#' @param surrogate Surrogate models
#' @param hyperpar Number of hyperparameters that should be evaluated at once; Possible options: one, two and all
calculateDatasetOptimumPackageDefault = function(surrogates, default, hyperpar = "one", n.points = 10000, tbl.metaFeatures, tbl.results) {
  surr = surrogates$surrogates
  param.set = surrogates$param.set
  
  if (hyperpar == "one") {
    result = matrix(NA, length(surr), length(param.set$pars))
    # only do this for parameters that makes sense changing them
    for(i in seq_along(param.set$pars)) {
      print(names(param.set$pars)[i])
      rnd.points1 = generateRandomDesignWithDefaults(n.points, param.set, trafo = TRUE, default, subset = names(param.set$pars)[i])
      # deleteNAs
      rnd.points1 = deleteNA(rnd.points1)
      
      # Prediction 
      preds = matrix(NA, nrow(rnd.points1), length(surr))
      
      for(j in seq_along(surr)) {
        if (!(names(param.set$pars)[i] %in% c("mtry", "gamma"))) {
          rnd.points1 = convertPackageDefault(rnd.points1, surr[[j]], tbl.metaFeatures, tbl.results)
        }
        preds[, j] = predict(surr[[j]], newdata = rnd.points1)$data$response
      }
      # Best default
      # rnd.points1[apply(preds, 2, which.max),]
      result[, i] = diag(preds[apply(preds, 2, which.max), ])
    }
    result = data.frame(result)
    colnames(result) = names(param.set$pars)
    return(list(optimum = result))
  }
  if (hyperpar == "two") {
    result = array(NA, dim = c(length(surr), length(param.set$pars), length(param.set$pars)))
    
    for(i in seq_along(param.set$pars)[-length(param.set$pars)]) {
      for(j in seq_along(param.set$pars)[(i+1):length(param.set$pars)]) {
        print(c(names(param.set$pars)[i], names(param.set$pars)[j]))
        rnd.points1 = generateRandomDesignWithDefaults(n.points, param.set, trafo = TRUE, default, subset = names(param.set$pars)[c(i,j)])
        rnd.points1 = deleteNA(rnd.points1)
        
        # Prediction 
        preds = matrix(NA, nrow(rnd.points1), length(surr))
        for(k in seq_along(surr)) {
          if (!any(names(param.set$pars)[c(i,j)] %in% c("mtry", "gamma"))) {
          rnd.points1 = convertPackageDefault(rnd.points1, surr[[k]], tbl.metaFeatures, tbl.results)
          }
          preds[, k] = predict(surr[[k]], newdata = rnd.points1)$data$response
        }
        # Best default
        # rnd.points1[apply(preds, 2, which.max),]
        result[, i, j] = diag(preds[apply(preds, 2, which.max), ])
      }
    }
    return(list(optimum = result))
  }
}