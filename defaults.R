library(devtools)
#OMLbots_path = "/home/probst/Paper/Exploration_of_Hyperparameters/OMLbots"
#OMLbots_path = "C:/Promotion/Hyperparameters/OMLbots"
#load_all(OMLbots_path)
load_all()
lrn.par.set = getMultipleLearners()

# Get file from the figshare repository
load(url("https://ndownloader.figshare.com/files/10462297"))



library(stringi)
learner.names = paste0("mlr.", names(lrn.par.set))
learner.names = stri_sub(learner.names, 1, -5)
measures = c("auc", "accuracy", "brier")

# Forward selection
k = 1
i = 1
print(i)
set.seed(199 + i)
load(paste0("surrogates_", measures[k], "_", i, ".RData"))
# Default calculation
default1 = calculateDefaultForward(surrogates, n.points = 100000, n.default = 10)
