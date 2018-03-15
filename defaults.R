library(devtools)
#OMLbots_path = "/home/probst/Paper/Exploration_of_Hyperparameters/OMLbots"
#OMLbots_path = "C:/Promotion/Hyperparameters/OMLbots"
#load_all(OMLbots_path)
load_all()
lrn.par.set = getMultipleLearners()

# Get file from the figshare repository
load(url("https://ndownloader.figshare.com/files/10462297"))
