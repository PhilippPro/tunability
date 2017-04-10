#' Calculate default hyperparameter setting
#' @param surrogates Surrogate models
calculateDefault = function(surrogates, param.set) {
  surr = surrogates
  rnd.points = generateRandomDesign(10000, param.set)
  rnd.points[is.na(rnd.points)] = -10 - 1
  
  preds = matrix(NA, nrow(rnd.points), length(surr))
  for(i in seq_along(surr)) {
    preds[, i] = predict(surr[[i]], newdata = rnd.points)$data$response
  }
  # Best default
  average_preds = apply(preds[, 1:2], 1, mean)
  average_preds[average_preds == max(average_preds)]
  rnd.points[average_preds == max(average_preds),]
  list(default = rnd.points[average_preds == max(average_preds),], result = preds[average_preds == max(average_preds), ])
}

#' Create default hyperparameter setting
#' @param surrogate Surrogate models
#' @param hyperpar Number of hyperparameters that should be evaluated at once; Possible options: one, two and all
calculateDatasetOptimum = function(surrogate, default, hyperpar = "all", param.set, n.points = 10000) {
  surr = surrogates
  if (hyperpar == "all") {
    rnd.points = generateRandomDesign(n.points, param.set)
    rnd.points[is.na(rnd.points)] = -10 - 1
    
    preds = matrix(NA, nrow(rnd.points), length(surr))
    for(i in seq_along(surr)) {
      preds[, i] = predict(surr[[i]], newdata = rnd.points)$data$response
    }
    # Best default
    rnd.points[apply(preds, 2, which.max),]
    return(diag(preds[apply(preds, 2, which.max), ]))
  }
  
  if (hyperpar == "one") {
    result = matrix(NA, length(surr), length(param.set$pars))
    rnd.points.def = default$default[rep(1, n.points),]
    # only do this for parameters that makes sense changing them
    for(i in seq_along(param.set$pars)) { 
      print(names(param.set$pars)[i])
      
      # Generation of random points for one parameter
      param.set1 = param.set
      param.set1$pars = param.set$pars[i]
      rnd.points1 = rnd.points.def
      if(!is.null(param.set1$pars[[1]]$requires)) {
        rnd.points1[, as.character(param.set1$pars[[1]]$requires[2])] = as.character(param.set1$pars[[1]]$requires[3])
        param.set1$pars[[1]]$requires = NULL
      } 
      rnd.points = generateRandomDesign(n.points, param.set1)
      rnd.points1[, i] = rnd.points
      
      # Prediction 
      preds = matrix(NA, nrow(rnd.points), length(surr))
      for(j in seq_along(surr)) {
        preds[, j] = predict(surr[[j]], newdata = rnd.points1)$data$response
      }
      # Best default
      # rnd.points1[apply(preds, 2, which.max),]
      result[, i] = diag(preds[apply(preds, 2, which.max), ])
    }
    result = data.frame(result)
    colnames(result) = names(param.set$pars)
    return(result)
  }
  if (hyperpar == "two") {
    print("nothing")
  }
}

#' Calculate tunability measures
#' @param surrogate Surrogate models
calculateTunability = function(default, optimumHyperpar, optimumTwoHyperpar = NULL) {
  optimumHyperpar - default$result
}