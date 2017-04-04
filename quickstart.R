library(devtools)
load_all()

surrogate = getSurrogateModels(measure.name, learner.name, task.ids)

defaults = calculateDefault(surrogate)
optimum = calculateDatasetOptimum(surrogate, hyperpar = "all")

# Tunability overall
overallTunability = calculateTunability(defaults, optimum)

# Tunability hyperparameter specific
optimumHyperpar = calculateDatasetOptimum(surrogate, hyperpar = "one")
tunability = calculateTunability(defaults, optimumHyperpar)

# Interactions
optimumTwoHyperpar = calculateDatasetOptimum(surrogate, hyperpar = "two")
tunability = calculateTunability(defaults, optimumHyperpar, optimumTwoHyperpar)

# Tuning Space
tuningSpace = calculateTuningSpace(surrogate, quantile)








