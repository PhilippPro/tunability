#' Calculate default hyperparameter setting
#' @param surrogates Surrogate models
calculateDefaultForward = function(surrogates, n.points = 100000, n.default = 10) {
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
  average_preds = apply(preds, 1, mean)
  best = which(average_preds == max(average_preds))[1]
  default = rnd.points[best,, drop = FALSE]
  rownames(default) = NULL
  
  # Second best default
  
  for(i in 1:n.default) {
    print(paste("iteration", i, "of", n.default))
    average_preds_new = apply(preds, 1, function(x) mean(apply(rbind(preds[best,], x), 2, max)))
    best2 = which(average_preds_new == max(average_preds_new))[1]
    best = c(best, best2)
    default = rnd.points[best,, drop = FALSE]
    print(paste("Maximum", round(average_preds_new[which(average_preds_new == max(average_preds_new))], 4)))
  }
  list(default = default, result = preds[best, ])
  
  # Default calculation with LOOCV
}
