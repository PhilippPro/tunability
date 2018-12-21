#' Calculate default hyperparameter setting
#' @param surrogates Surrogate models
calculateDefault = function(surrogates, n.points = 100000, normalization = FALSE) {
  surr = surrogates$surrogates
  param.set = surrogates$param.set
  rnd.points = generateRandomDesign(n.points, param.set, trafo = TRUE)
  rnd.points = deleteNA(rnd.points)
  
  preds = matrix(NA, nrow(rnd.points), length(surr))
  for(i in seq_along(surr)) {
    print(paste("surrogate predict: task", i, "of", length(surr)))
    preds[, i] = predict(surr[[i]], newdata = rnd.points)$data$response
  }
  # Best default in general
  if(normalization == FALSE) {
    average_preds = apply(preds, 1, mean)
  } else {
    new_preds = preds
    for(i in 1:ncol(preds)) {
      new_preds[, i] = scale(preds[, i])
    }
    average_preds = apply(new_preds, 1, mean)
  }
  
  best = which(average_preds == max(average_preds))[1]
  default = rnd.points[best,, drop = FALSE]
  rownames(default) = NULL
  
  list(default = rnd.points[best,, drop = FALSE], result = preds[best, ])
  
  # Default calculation with LOOCV
  #best_i = numeric(ncol(preds))
  #preds_i_best = numeric(ncol(preds))
  #default.loocv = list()
  # Best default with LOOCV
  #for(i in 1:ncol(preds)) {
  #  preds_i = rowMeans(preds[, -i])
  #  best_i[i] = which(preds_i == max(preds_i))[1]
  #  preds_i_best[i] = preds[best_i[i],i]
  #  default.loocv[[i]] = rnd.points[best_i[i],, drop = FALSE]
  #}
  
  #list(default = rnd.points[best,, drop = FALSE], result = preds[best, ], 
  #  default.loocv = rnd.points[best_i,], result.loocv = preds_i_best)
}

#' Calculate performance of hyperparameter setting
#' @param par.set Parameter setting
calculatePerformance = function(surrogates, default) {
  surr = surrogates$surrogates
  preds = numeric(length(surr))
  for(i in seq_along(surr)) {
    print(paste("surrogate predict: task", i, "of", length(surr)))
    preds[i] = predict(surr[[i]], newdata = default)$data$response
  }
  # Best default
  list(default = default, result = preds)
}

#' Calculate optimal hyperparameter values for an algorithm
#' @param surrogate Surrogate models
#' @param hyperpar Number of hyperparameters that should be evaluated at once; Possible options: one, two and all
calculateDatasetOptimum = function(surrogates, default, hyperpar = "all", n.points = 10000) {
  surr = surrogates$surrogates
  param.set = surrogates$param.set
  if (hyperpar == "all") {
    rnd.points = generateRandomDesign(n.points, param.set, trafo = TRUE)
    rnd.points = deleteNA(rnd.points)

    preds = matrix(NA, nrow(rnd.points), length(surr))
    for(i in seq_along(surr)) {
      print(paste("surrogate predict: task", i, "of", length(surr)))
      preds[, i] = predict(surr[[i]], newdata = rnd.points)$data$response
    }
    # Best Value
    rnd.points[apply(preds, 2, which.max),]
    return(list(optimum = diag(preds[apply(preds, 2, which.max), ]), par.sets = rnd.points[apply(preds, 2, which.max),, drop = FALSE]))
  }
  
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
        preds[, j] = predict(surr[[j]], newdata = rnd.points1)$data$response
      }
      # Best Value
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
          preds[, k] = predict(surr[[k]], newdata = rnd.points1)$data$response
        }
        # Best Value
        # rnd.points1[apply(preds, 2, which.max),]
        result[, i, j] = diag(preds[apply(preds, 2, which.max), ])
      }
    }
    return(list(optimum = result))
  }
}

#' Calculate tunability measures
#' @param surrogate Surrogate models
calculateTunability = function(default, optimumHyperpar, optimumTwoHyperpar = NULL) {
  optimumHyperpar$optimum - default$result
}

deleteNA = function(task.data) {
  for(i in 1:ncol(task.data)) {
    if(is.numeric(task.data[, i]))
      task.data[is.na(task.data[, i]), i] = -10 - 1
    if(is.factor(task.data[, i])) {
      task.data[, i] = addNA(task.data[, i])
      task.data[, i] = droplevels(task.data[, i])
    }
    if(is.logical(task.data[, i]))
      task.data[, i] = as.factor(task.data[, i])
  }
  task.data
}

generateRandomDesignWithDefaults = function(n.points, param.set, trafo, default, subset) {
  rnd.points.def = default$default[rep(1, n.points), , drop = FALSE]
  
  # Required Parameters and Values
  reqPar = as.character(sapply(sapply(param.set$pars, `[[`, 12), `[[`, 2))
  reqValue = as.character(sapply(sapply(param.set$pars, `[[`, 12), `[[`, 3))
  
  param.set1 = param.set
  # If there are dependent variables include them
  if(any(subset %in% reqPar)) {
    subset2 = unique(c(subset, names(param.set$pars)[reqPar %in% subset]))
  } else {
    subset2 = subset
  }
  
  param.set1$pars = param.set$pars[subset2]
  rnd.points1 = rnd.points.def
  
  # If one parameter is required by another set it to the specific value
  for(m in seq_along(subset)) {
  if(!is.null(param.set1$pars[[m]]$requires)) {
    reqParSubset = as.character(param.set1$pars[[m]]$requires[2])
    reqValueSubset = as.character(param.set1$pars[[m]]$requires[3])
    
    rnd.points1[, reqParSubset] = reqValueSubset
    
    for(l in seq_along(param.set$pars)) {
      if(reqPar[l] == reqParSubset & reqValue[l] != reqValueSubset)
        rnd.points1[, l] = -10 - 1
    }
    if (!(reqParSubset %in% subset))
      param.set1$pars[[m]]$requires = NULL
  }
  }
  
  rnd.points = generateRandomDesign(n.points, param.set1, trafo = TRUE)
  rnd.points1[, subset2] = rnd.points
  
  # Set the dependent values back to default
  back_to_default = subset2[!(subset2 %in% subset)]
  for(q in back_to_default) {
    if (q == "degree") {  # Spezialfall svm, da wir hier keinen sinnvollen Default haben und den Package default nehmen
      rnd.points1[!is.na(rnd.points1[,q]), q] = 3
    } else {
      rnd.points1[!is.na(rnd.points1[,q]), q] = default$default[,q]
    }
  }
  # Add the default
  rnd.points1 = rbind(default$default, rnd.points1)
  rnd.points1
}
  

