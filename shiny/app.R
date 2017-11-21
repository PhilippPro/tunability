library(ggplot2)
library(plotly)
# library(tidyr)
library(shiny)
# library(shinydashboard)
# library(shinyjs)
# library(shinyBS)
library(data.table)
library(DT)
library(mlr)
library(devtools)

load_all()

results_auc = NULL
names = load("../results.RData")
for(i in seq_along(names))
  results_auc[[i]] = get(names[i])
names(results_auc) = names
results_accuracy = NULL
names = load("../results_accuracy.RData")
for(i in seq_along(names))
  results_accuracy[[i]] = get(names[i])
names(results_accuracy) = names
# names = load("../results_brier.RData")
# for(i in seq_along(names))
#   results_brier[[i]] = get(names[i])
# names(results_brier) = names
results_all = list(auc = results_auc, accuracy = results_accuracy)

#load(file = "../results.RData")
#load(file = "../surrogates.RData")

server = function(input, output) {
  
  measure.names = names(results_all)
  learner.names = names(results_all$auc$results)
  
  output$measureAll = renderUI({
    selectInput('meas', 'Measure', measure.names, selected = measure.names[1], multiple = FALSE)
  })
  
  output$algorithm = renderUI({
    selectInput('algo', 'Algorithm', learner.names, selected = learner.names[1], multiple = FALSE)
  })
  
  bmrInput = reactive({
    inputi = "mlr.classif.glmnet"
    if(!is.null(input$algo))
      inputi = input$algo
    measur = "auc"
    if(!is.null(input$meas))
      measur = input$meas
    results_all[[which(measure.names == measur)]]$bmr_surrogate[[which(learner.names == inputi)]]
  })
  
  bmrAggr = reactive({
    perfs = data.table(getBMRAggrPerformances(bmrInput(), as.df = T, drop = T))[, -"task.id"]
    # delete datasets with missing results
    error.results = which(is.na(perfs$kendalltau.test.mean) | perfs$rsq.test.mean < 0)
    error.results = unlist(lapply(unique(floor(error.results/5 - 0.0001)), 
      function(x) x + seq(0.2, 1, 0.2)))*5
    if(length(error.results)!=0)
      perfs = perfs[-error.results,]
    perfs = data.frame(perfs[, lapply(list(mse = mse.test.mean, rsq = rsq.test.mean, kendalltau = kendalltau.test.mean, 
      spearmanrho = spearmanrho.test.mean),function(x) mean(x)), by = "learner.id"])
    perfs$learner.id =  sub('.*\\.', '', as.character(perfs$learner.id))
    perfs
  })
  
  output$logscale = renderUI({
    selectInput('logscale', 'Logarithmic scale', c("No", "Yes"), selected = "No", multiple = FALSE)
  })
  
  output$bmr_measure = renderUI({
    measures = getBMRMeasureIds(bmrInput())
    selectInput('bmr_measure', 'Measure', measures, selected = measures[1], multiple = FALSE)
  })
  
  output$bmr_result = renderTable({
    bmrAggr() 
  }, digits = 5)
  
  output$plot1 = renderPlot({
    measure = bmrInput()$measures[[which(sapply(bmrInput()$measures, `[[`, 1) == input$bmr_measure)]]
    if (input$logscale == "Yes") {
      plotBMRSummary(bmrInput(), measure = measure) + scale_x_log10() + ggtitle("Performance on datasets")
    } else {
      plotBMRSummary(bmrInput(), measure = measure) + ggtitle("Performance on datasets")
    }
  })
  
  output$plot2 = renderPlot({
    measure = bmrInput()$measures[[which(sapply(bmrInput()$measures, `[[`, 1) == input$bmr_measure)]]
    plotBMRRanksAsBarChart(bmrInput(), measure = measure, pos = "stack") + ggtitle("Frequency of ranks")
  })
  
  output$task = renderUI({
    selectInput('taski', 'Task', c("classification", "regression"), selected = "classification", multiple = FALSE)
  })
  
  output$defaultchoice = renderUI({
    selectInput('defaultchoice', 'Defaults', c("Calculated defaults", "Package defaults"), selected = "Calculated defaults", multiple = FALSE)
  })
  
  resultsInput = reactive({
    if (input$defaultchoice == "Calculated defaults") {
      results_all[[input$meas]]$results[[input$algo]]
    } else {
      results_all[[input$meas]]$resultsPackageDefaults[[input$algo]]
    }
  })
  
  output$defaults = renderTable({
    resultsInput()$default$default
  }, digits = 3)
  
  overall = reactive({
    calculateTunability(resultsInput()$default, results_all[[input$meas]]$results[[input$algo]]$optimum)
  })
  
  tunabilityValues = reactive({
    calculateTunability(resultsInput()$default, resultsInput()$optimumHyperpar)
  })
  
  tunabilityValuesMean = reactive({
    colMeans(calculateTunability(resultsInput()$default, resultsInput()$optimumHyperpar))
  })
  
  output$scaled = renderUI({
    selectInput('scaled', 'Scaled', c(TRUE, FALSE), selected = FALSE, multiple = FALSE)
  })
  
  output$overallTunability = renderTable({
    if (input$scaled) {
      mean(overall()/overall())
    } else {
      mean(overall())
    }
  }, colnames = FALSE, digits = 3)
  
  output$tunability = renderTable({
    if (input$scaled) {
      data.frame(t(colMeans(tunabilityValues()/overall(), na.rm = T)))
    } else {
      data.frame(t(tunabilityValuesMean()))
    }
  }, digits = 3)
  
  output$plot3 = renderPlotly({
    dataf = data.frame(overall(), tunabilityValues())
    colnames(dataf)[1] = "overall"
    column.names = colnames(dataf)
    dataf = stack(dataf)
    dataf$ind = factor(dataf$ind, column.names)
    ggplot(dataf, aes(x = ind, y = values)) + geom_boxplot() + scale_y_continuous(limits=c(input$yrange[1],input$yrange[2])) + 
      ylab("tunability per dataset") #+ xlab("parameter") # for the x axis label  # + ggtitle(substring(learner.names[i], 13))
  })
  
  output$visual = renderUI({
    selectInput('visual', 'Visualization of the tunability', c("Density", "Histogram"), selected = "Density", multiple = FALSE)
  })
  
  output$visual3 = renderUI({
    selectInput('visual3', 'Hyperparameter', c(names(tunabilityValuesMean())), selected = "All", multiple = FALSE)
  })
  
  output$plot4 = renderPlotly({
    dataf = data.frame(resultsInput()$optimum$par.set[,input$visual3])
    name = input$visual3
    num = is.numeric(dataf[,1])
    
    inputi = "mlr.classif.glmnet"
    if(!is.null(input$algo))
      inputi = input$algo
    
    if(num) {
      dataf = dataf[dataf[,1]!=-11, , drop = F]
      learner.i = which(learner.names == inputi)
      TRAFO = is.null(lrn.par.set[[learner.i]][[2]]$pars[[name]]$trafo)
      if(TRAFO) {
        ggplot(data=dataf, aes(dataf[,1])) + geom_histogram(aes(y=..density..), bins = 6, col = "black", fill = "white") + xlab(name)
      } else {
        ggplot(data=dataf, aes(dataf[,1])) + geom_histogram(aes(y=..density..), bins = 6, col = "black", fill = "white") + xlab(name) + scale_x_continuous(trans = "log10")
      }
    } else {
      ggplot(data=dataf, aes(dataf[,1])) + geom_bar(aes(y = (..count..)/sum(..count..)), col = "black", fill = "white") + 
        xlab(name) + ylab("relative frequency")
    }
  })
  
  
  # output$visual2 = renderUI({
  #   selectInput('visual2', 'Hyperparameter', c("All", names(tunabilityValuesMean())), selected = "All", multiple = FALSE)
  # })
  # output$plot5 = renderPlotly({
  #   if (input$visual2 == "All") {
  #     if (input$scaled) {
  #       x = overall()/overall()
  #     } else {
  #       x = overall()
  #     }
  #   } else {
  #     if (input$scaled) {
  #       x = tunabilityValues()[, input$visual2]/overall()
  #     } else {
  #       x = tunabilityValues()[, input$visual2]
  #     }
  #   }
  #   if (input$visual == "Density") {
  #     ggplot(data.frame(x), aes(x)) + geom_density() + ggtitle("Density of the Overall Tunability")
  #     } else {
  #     ggplot(data.frame(x), aes(x)) + geom_histogram(bins = input$bins, stat = "bin", fill = "green", colour = "black") + 
  #         xlim(range(x)) + ggtitle("Histogram of the Overall Tunability")
  #   }
  # })
  
  output$quantile = renderUI({
    numericInput('quantile', 'Quantile for Tuning Space Calculation', 0.1, min = 0, max = 1)
  })
  
  tuningSpace = reactive({
    calculateTuningSpace(results_all[[input$meas]]$results[[input$algo]]$optimum, quant = input$quantile)
  })
  
  output$tuningSpaceNumerics = renderTable({
    tuningSpace()$numerics
  }, rownames = TRUE, digits = 3)
  
  output$tuningSpaceFactors = renderTable({
    tuningSpace()$factors
  })
  
  output$combi = renderUI({
    selectInput('combination', 'Combinations of two hyperparameters', 
      c("Tunability", "Interaction effect", "Performance gain"), 
      selected = "Tunability", multiple = FALSE)
  })
  
  output$combiTable <- renderTable({
    tab = colMeans(resultsInput()$optimumTwoHyperpar$optimum, dims = 1, na.rm = TRUE) - mean(resultsInput()$default$result)
    if(input$combination == "Tunability") {
      diag(tab) = tunabilityValuesMean()
    } else {
      if(input$combination == "Interaction effect") {
        tab = tab - outer(tunabilityValuesMean(), tunabilityValuesMean(), '+')
      } else {
        tab = tab - outer(tunabilityValuesMean(), tunabilityValuesMean(), pmax)
      }
    }
    colnames(tab) = rownames(tab) = names(tunabilityValuesMean())
    tab
  }, rownames = TRUE, digits = 4)
  
  
  output$par.set = renderUI({
    tagList(makeLearnerParamUI(results_all[[input$meas]]$results[[input$algo]]))
  })
  
  
  output$performanceHypParSetting = renderTable({
    var_names = colnames(results_all[[input$meas]]$results[[input$algo]]$optimum$par.sets)
    par.set = numeric()
    for(i in 1:length(var_names)) {
      par.set[i] = input[[var_names[i]]]
    }
    par.set
    #calculatePerformance(surrogates_all[[input$algo]], par.set)$preds
  })
  # performanceHypParSetting = reactive({
  #   calculatePerformance(surrogates_all[[input$algo]], par.set)
  # })
  
}

makeLearnerParamUI = function(results_algo) {
  par.set = results_algo$ optimum$par.sets
  inp = list()
  for(i in 1:ncol(par.set)) {
    par.type = class(par.set[,i])
    par.id = names(par.set)[i]
    if (par.type == "numeric")
      inp[[i]] = numericInput(par.id, par.id, results_algo$default$default[i])
    if (par.type == "factor")
      inp[[i]] = selectInput(par.id, par.id, choices = unique(par.set[,i]), selected = results_algo$default$default[i])
  }
  inp
}

ui = fluidPage(
  titlePanel("Summary of the benchmark results"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("measureAll"),
      uiOutput("algorithm")
    ),
    tabsetPanel(
      tabPanel("Surrogate models comparison", 
        fluidRow(
          column(12, "Average mean of different surrogate models (for not-NA results)", tableOutput("bmr_result"))),
        fluidRow(
          column(6, uiOutput("logscale")), column(6, uiOutput("bmr_measure"))),
        plotOutput("plot1"),
        plotOutput("plot2")
      ),
      tabPanel("Defaults and Tunability", 
        fluidRow(column(12, uiOutput("defaultchoice"))),
        fluidRow(
          column(12, h4("Defaults"), tableOutput("defaults"))), 
        hr(),
        fluidRow(
          column(12, h4("Tunability"), 
            column(12, uiOutput("scaled")),
            column(12, fluidRow(
              column(1, "Overall mean tunability", tableOutput("overallTunability")), 
              column(11, "Hyperparameters (mean)", tableOutput("tunability"))
            )))),
        plotlyOutput("plot3", inline = F),
        sliderInput("yrange",  "Y-range:", min = 0, max = 0.5, value = c(0, 0.025), width = "800px"),
        
        hr(),
        fluidRow(column(12, h4("Tuning Space"),
          column(12, uiOutput("quantile")),
          column(12, "Numerics", align="left", tableOutput("tuningSpaceNumerics")),
          column(12, "Factors", align="left", tableOutput("tuningSpaceFactors"))
        )),
        hr(),
        fluidRow(column(12, h4("Histogram of best hyperparameter on each of the datasets (Prior for tuning)")),
          column(12, uiOutput("visual3"))),
        plotlyOutput("plot4", inline = F)
        #fluidRow(column(6, uiOutput("visual")),
        #  column(6, uiOutput("visual2")))
        #plotlyOutput("plot5", inline = F),
        
        # conditionalPanel(
        # condition = "input.visual == 'Histogram'",
        #   sliderInput("bins",  "Number of bins:", min = 1, max = 50, value = 30)
        # )
        
      ),
      tabPanel("Interaction effects",
        fluidRow(column(12, uiOutput("combi")),
          column(12, tableOutput("combiTable")))
      ),
      tabPanel("Arbitrary Parameter setting", 
        fluidRow(column(12, uiOutput("par.set")), 
          column(12, tableOutput("performanceHypParSetting"))))
    )
  )
)


shinyApp(ui = ui, server = server)