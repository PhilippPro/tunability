convertMtry = function(tbl.results, tbl.hypPars, tbl.metaFeatures) {
  features1 = tbl.metaFeatures
  results1 = tbl.results[, c("run.id", "task.id")]
  features1 = features1[, c("task.id", "number.of.features")]
  results1 = results1 %>%
    left_join(., features1, by = "task.id") %>%
    select(., -task.id)
  hypPars1 = tbl.hypPars[tbl.hypPars$hyperpar.name == "mtry", ] 
  hypPars1 = left_join(hypPars1, results1, by = "run.id")
  hypPars1 = unique(hypPars1)
  tbl.hypPars[tbl.hypPars$hyperpar.name == "mtry", ]$hyperpar.value = as.numeric(hypPars1$hyperpar.value) / hypPars1$number.of.features
  tbl.hypPars
}

# old
convertMtry = function(tbl.results, tbl.hypPars, tbl.metaFeatures) {
  features1 = tbl.metaFeatures
  results1 = tbl.results[, c("run.id", "task.id")]
  features1 = features1[, c("task.id", "number.of.features")]
  results1 = results1 %>%
    left_join(., features1, by = "task.id") %>%
    select(., -task.id)
  hypPars1 = tbl.hypPars[tbl.hypPars$hyperpar.name == "mtry", ] 
  hypPars1 = left_join(hypPars1, results1, by = "run.id")
  hypPars1 = unique(hypPars1)
  tbl.hypPars[tbl.hypPars$hyperpar.name == "mtry", ]$hyperpar.value = as.numeric(hypPars1$hyperpar.value) / hypPars1$number.of.features
  tbl.hypPars
}