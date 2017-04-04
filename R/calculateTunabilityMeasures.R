#' Calculate default hyperparameter setting
#' @param surrogate Surrogate models
calculateDefault = function(surrogate) {
  surr = surrogate
  rnd.points = generateRandomDesign(10000, lrn.par.set$classif.glmnet.set$param.set)
  preds = matrix(NA, nrow(rnd.points), length(surr))
  for(i in seq_along(surr)) {
    preds[, i] = predict(surr[[i]], newdata = rnd.points)$data$response
  }
  # Best default
  average_preds = apply(preds, 1, mean)
  average_preds[average_preds == max(average_preds)]
  rnd.points[average_preds == max(average_preds), ]
}

#' Create default hyperparameter setting
#' @param surrogate Surrogate models
#' @param hyperpar Number of hyperparameters that should be evaluated at once; Possible options: one, two and all
calculateDatasetOptimum = function(surrogate, hyperpar = "two") {
  
}

#' Calculate tunability measures
#' @param surrogate Surrogate models
calculateTunability = function(defaults, optimumHyperpar, optimumTwoHyperpar) {
  
}