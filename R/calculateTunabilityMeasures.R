#' Calculate default hyperparameter setting
#' @param surrogates Surrogate models
calculateDefault = function(surrogates, param.set) {
  surr = surrogates
  rnd.points = generateRandomDesign(10000, param.set)
  preds = matrix(NA, nrow(rnd.points), length(surr))
  for(i in seq_along(surr)) {
    preds[, i] = predict(surr[[i]], newdata = rnd.points)$data$response
  }
  # Best default
  average_preds = apply(preds[, 1:2], 1, mean)
  average_preds[average_preds == max(average_preds)]
  rnd.points[average_preds == max(average_preds),]
  preds[average_preds == max(average_preds), ]
}

#' Create default hyperparameter setting
#' @param surrogate Surrogate models
#' @param hyperpar Number of hyperparameters that should be evaluated at once; Possible options: one, two and all
calculateDatasetOptimum = function(surrogate, hyperpar = "two") {
  surr = surrogates
  rnd.points = generateRandomDesign(10000, param.set)
  preds = matrix(NA, nrow(rnd.points), length(surr))
  for(i in seq_along(surr)) {
    preds[, i] = predict(surr[[i]], newdata = rnd.points)$data$response
  }
  # Best default
  rnd.points[apply(preds, 2, which.max),]
  diag(preds[apply(preds, 2, which.max), ])
}

#' Calculate tunability measures
#' @param surrogate Surrogate models
calculateTunability = function(defaults, optimumHyperpar, optimumTwoHyperpar = NULL) {
  optimumHyperpar - defaults
}