setwd("/nfsmb/koll/probst/Paper/Exploration_of_Hyperparameters/tunability/shiny")
load("results_all.RData")

# Nur absolut notwendige Information extrahieren
app_data = list()

measures = names(results_all)
classifiers = names(results_all$auc$bmr_surrogate)

for(i in measures) {
  for(j in classifiers) {
   app_data[[i]]$surrogate[[j]] = getBMRAggrPerformances(results_all[[i]]$bmr_surrogate[[j]], as.df = TRUE)
   app_data[[i]]$results = results_all[[i]]$results
   app_data[[i]]$resultsPackageDefaults = results_all[[i]]$resultsPackageDefaults
   app_data[[i]]$results_cv = results_all[[i]]$results_cv
   app_data[[i]]$lrn.par.set = results_all[[i]]$lrn.par.set
  }
}

save(app_data, file = "app_data.RData")

# auc und accuracy surrogate Zeug unterscheidet sich nicht!? -> neu rechnen -> nur den 6er von accuracy?, auch in Paper!
# resultsPackageDefaults fehlt bei der AUC
