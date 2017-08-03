#' # classif.glmnet: Defaults kommen nicht vor
#' # classif.rpart
#' # classif.kknn
#' head(hyperparameters)
#' ks = hyperparameters[hyperparameters$name == "k",]
#' sort(as.numeric(names(table(ks$value))))
#' 
#' head(evaluation_results)
#' evaluation_results[evaluation_results$setup]
#' setup_ids_kknn_7 = evaluation_results$setup[!(evaluation_results$setup %in% unique(hyperparameters$setup))]
#' 
#' 
#' 
#' 
#' #' @title addDefaultValues
#' #' If default values are not overwritten OpenML does not return values. This function adds values for the tuned hyperparameters so they can be used for the calculation of the pareto front.
#' #' @param res Long table with hyperparameters generated in getHyperparTable
#' #' @return [\code{data.frame}] Long Table with added values for the defaults.
#' #' @export
#' addDefaultValues = function(res) {
#'   learner.name = try(listOMLRunEvaluations(run.id = res$run.id[1])$learner.name)
#'   
#'   if(learner.name == "classif.glmnet") { # glmnet
#'     data_wide = spread(res, hyperpar.name, hyperpar.value)
#'     data_wide$s = NULL
#'     res = gather(data_wide, hyperpar.name, hyperpar.value, -run.id)
#'   }
#'   
#'   if(learner.name == "classif.rpart") { # rpart
#'     levels(res$hyperpar.value) = c(levels(res$hyperpar.value), 30, 20)
#'     data_wide = spread(res, hyperpar.name, hyperpar.value)
#'     data_wide$xval = NULL
#'     data_wide$maxdepth[is.na(data_wide$maxdepth)] = 30
#'     data_wide$minsplit[is.na(data_wide$minsplit)] = 20
#'     res = gather(data_wide, hyperpar.name, hyperpar.value, -run.id)
#'   }
#'   if(learner.name == "classif.kknn") { # kknn
#'     res$hyperpar.name = "k"
#'     levels(res$hyperpar.value) = c(levels(res$hyperpar.value), 7)
#'     res$hyperpar.value[is.na(res$hyperpar.value)] = 7
#'     res
#'   }
#'   if(learner.name == "classif.svm") { # svm
#'     levels(res$hyperpar.value) = c(levels(res$hyperpar.value), "radial", 3)
#'     data_wide = spread(res, hyperpar.name, hyperpar.value)
#'     data_wide$kernel[is.na(data_wide$kernel)] = "radial"
#'     nas = is.na(data_wide[data_wide$kernel == "polynomial",]$degree)
#'     data_wide[data_wide$kernel == "polynomial",]$degree[nas] = 3
#'     res = gather(data_wide, hyperpar.name, hyperpar.value, -run.id)
#'   }
#'   if(learner.name == "classif.ranger") { # ranger
#'     levels(res$hyperpar.value) = c(levels(res$hyperpar.value), TRUE, 500, FALSE)
#'     data_wide = spread(res, hyperpar.name, hyperpar.value)
#'     data_wide$verbose = NULL
#'     data_wide$num.threads = NULL
#'     data_wide$num.trees[is.na(data_wide$num.trees)] = 500
#'     data_wide$replace[is.na(data_wide$replace)] = TRUE
#'     data_wide$respect.unordered.factors[is.na(data_wide$respect.unordered.factors)] = FALSE
#'     res = gather(data_wide, hyperpar.name, hyperpar.value, -run.id)
#'   }
#'   if(learner.name == "classif.xgboost") { # xgboost
#'     levels(res$hyperpar.value) = c(levels(res$hyperpar.value), "gbtree", 1, 6)
#'     data_wide = spread(res, hyperpar.name, hyperpar.value)
#'     data_wide$verbose = NULL
#'     data_wide$nrounds[is.na(data_wide$nrounds)] = 1
#'     data_wide$booster[is.na(data_wide$booster)] = "gbtree"
#'     nas = is.na(data_wide[data_wide$booster == "gbtree",]$max_depth)
#'     data_wide[data_wide$booster == "gbtree",]$max_depth[nas] = 6
#'     res = gather(data_wide, hyperpar.name, hyperpar.value, -run.id)
#'   }
#'   return(res)
#' }