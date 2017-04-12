#' Calculate default hyperparameter space for tuning
#' @param surrogate Surrogate models
calculateTuningSpace = function(optimum, quant) {
  space = data.frame(row.names = c(quant, 1-quant))
  space2 = list()
  par.sets = optimum$par.sets
  for(i in 1:ncol(par.sets)) {
    if(is.numeric(par.sets[,i])) {
      space = cbind(space, quantile(par.sets[,i], c(quant, 1-quant)))
      colnames(space)[ncol(space)] = names(par.sets)[i]
    }
    if(is.factor(par.sets[,i]) | is.logical(par.sets[,i])) {
      logic = table(par.sets[,i]) / length(par.sets[,i]) > quant
      space2 = c(space2, list(names(table(par.sets[,i]))[logic]))
      names(space2)[length(space2)] = names(par.sets)[i]
    }
  }
  return(list(numerics = space, factors = space2))
}
