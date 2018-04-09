library(devtools)
#OMLbots_path = "/home/probst/Paper/Exploration_of_Hyperparameters/OMLbots"
#OMLbots_path = "C:/Promotion/Hyperparameters/OMLbots"
#load_all(OMLbots_path)
load_all()
lrn.par.set = getMultipleLearners()

# Get file from the figshare repository
#load(url("https://ndownloader.figshare.com/files/10462297"))


library(stringi)
learner.names = paste0("mlr.", names(lrn.par.set))
learner.names = stri_sub(learner.names, 1, -5)
measures = c("auc", "accuracy", "brier")

# Forward selection
defaults = list()

for(i in seq_along(learner.names)) {
  print(i)
  set.seed(199 + i)
  load(paste0("surrogates_", measures[k], "_", i, ".RData"))
  # Default calculation
  defaults[[length(defaults) + 1]] = calculateDefaultForward(surrogates, n.points = 100000, n.default = 10)
}
names(defaults) = stri_sub(learner.names, 13, 100)
save(defaults, file = "defaults.RData")

load("defaults.RData")

# Performance of first default
for(i in 1:6)
  print(c(stri_sub(learner.names[i], 13, 30), round(mean(defaults[[i]]$result[1,]), 4)))

# Performance of first 10 defaults
for(i in 1:6)
  print(c(stri_sub(learner.names[i], 13, 30), round(mean(apply(defaults[[i]]$result, 2, max)), 4)))


