library(ggplot2)
library(plotly)
# library(tidyr)
library(shiny)
library(shinyjs)
#library(shinydashboard)
# library(shinyBS)
library(data.table)
library(DT)
library(ParamHelpers)
library(mlr)
#library(devtools)
library(checkmate)
library(glmnet)
library(kknn)
library(rpart)
library(e1071)
library(ranger)
library(xgboost)

server = function(input, output) {
  
  load("app_data.RData")
  source("helpers.R")
  
  measure.names = names(app_data)
  learner.names = names(app_data$auc$results)
  
  output$measureAll = renderUI({
   selectInput('meas', 'Measure', measure.names, selected = measure.names[1], multiple = FALSE)
  })
  
  output$algorithm = renderUI({
   selectInput('algo', 'Algorithm', learner.names, selected = learner.names[1], multiple = FALSE)
  })
  
  output$defaultchoice <- renderUI({
    selectInput('defaultchoice', 'Defaults', c("Calculated defaults", "Package defaults"), selected = "Calculated defaults", multiple = FALSE)
  })
   
   bmrInput = reactive({
     inputi = "mlr.classif.glmnet"
     if(!is.null(input$algo))
       inputi = input$algo
     measur = "auc"
     if(!is.null(input$meas))
       measur = input$meas
     app_data[[which(measure.names == measur)]]$surrogate[[which(learner.names == inputi)]]
   })
   
   bmrAggr = reactive({
     perfs = data.table(bmrInput())[, -"task.id"]
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
     measures = gsub("\\..*","",colnames(bmrInput())[-c(1,2)])
     selectInput('bmr_measure', 'Measure', measures, selected = measures[1], multiple = FALSE)
   })

   output$bmr_result = renderTable({
     bmrAggr()
   }, digits = 5)
   
  #df_test = app_data$auc$surrogate$mlr.classif.glmnet
  #plotBMRSummary
  #bmr_measure = gsub("\\..*","",colnames(app_data$auc$surrogate$mlr.classif.glmnet)[-c(1,2)])
  #learner_ids = gsub("\\..*","",learner.names)

  output$plot1 = renderPlot({
     sel_meas = paste0(input$bmr_measure, ".test.mean")
     p = ggplot(bmrInput(), aes_string(x = sel_meas, y = "task.id", col = "learner.id", shape = "learner.id"))
     p = p + geom_point(size = 4L, position = position_jitter(width = 0, height = 0.05))
     p = p + scale_shape_manual(values = rep(19, length(learner.names)))
     p = p + ylab("")
     p = p + xlab(sel_meas)
     if (ifelse(!is.null(input$logscale), input$logscale == "Yes" , TRUE)) {
     #if (input$logscale == "Yes") {
       p + scale_x_log10() + ggtitle("Performance on datasets")
     } else {
       p + ggtitle("Performance on datasets")
     }
   })

  output$task = renderUI({
    selectInput('taski', 'Task', c("classification", "regression"), selected = "classification", multiple = FALSE)
  })



  resultsInput = reactive({
    if (input$defaultchoice == "Calculated defaults") {
      app_data[[input$meas]]$results[[input$algo]]
    } else {
      app_data[[input$meas]]$resultsPackageDefaults[[input$algo]]
    }
  })

  output$defaults = renderTable({
    resultsInput()$default$default
  }, digits = 3)

  overall = reactive({
    calculateTunability(resultsInput()$default, app_data[[input$meas]]$results[[input$algo]]$optimum)
  })

  tunabilityValues = reactive({
    calculateTunability(resultsInput()$default, resultsInput()$optimumHyperpar)
  })

  tunabilityValuesMean = reactive({
    colMeans(calculateTunability(resultsInput()$default, resultsInput()$optimumHyperpar))
  })

  output$scaled = renderUI({
    selectInput('scaled', 'Scaled (per Dataset)', c(TRUE, FALSE), selected = FALSE, multiple = FALSE)
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
      ylab("tunability per dataset") + xlab("hyperparameter") # for the x axis label  # + ggtitle(substring(learner.names[i], 13))
  })

  output$visual = renderUI({
    selectInput('visual', 'Visualization of the tunability', c("Density", "Histogram"), selected = "Density", multiple = FALSE)
  })

  output$visual3 = renderUI({
      selectInput('visual3', 'Hyperparameter', c(names(tunabilityValuesMean())), selected = "All", multiple = FALSE)
  })

  output$plot4 = renderPlotly({
    dataf = data.frame(app_data[[input$meas]]$results[[input$algo]]$optimum$par.sets[,input$visual3])
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
        ggplot(data=dataf, aes(dataf[,1])) + geom_histogram(aes(y=..density..), bins = input$nrbin, col = "black", fill = "white") + xlim(range(dataf[,1])) + xlab(name)
      } else {
        ggplot(data=dataf, aes(dataf[,1])) + geom_histogram(aes(y=..density..), bins = input$nrbin, col = "black", fill = "white") + xlim(range(dataf[,1])) + xlab(paste(name, "(log-scale)")) + scale_x_continuous(trans = "log10")
      }
    } else {
      ggplot(data=dataf, aes(dataf[,1])) + geom_bar(aes(y = (..count..)/sum(..count..)), col = "black", fill = "white") +
        xlab(name) + ylab("relative frequency")
    }
  })

  output$quantile = renderUI({
    numericInput('quantile', 'Quantile for Tuning Space Calculation', 0.1, min = 0, max = 1)
  })

  tuningSpace = reactive({
    calculateTuningSpace(app_data[[input$meas]]$results[[input$algo]]$optimum, quant = input$quantile)
  })

  output$tuningSpaceNumerics = renderTable({
    tuningSpace()$numerics
  }, rownames = TRUE, digits = 3)

  output$tuningSpaceFactors = renderTable({
    tuningSpace()$factors
  })

  output$combi = renderUI({
    selectInput('combination', 'Combinations of two hyperparameters',
      c("Tunability", "Performance gain", "Interaction effect"),
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
    tagList(makeLearnerParamUI(app_data[[input$meas]]$results[[input$algo]]))
  })


  output$performanceHypParSetting = renderTable({
    var_names = colnames(app_data[[input$meas]]$results[[input$algo]]$optimum$par.sets)
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
  par.set = results_algo$optimum$par.sets
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
  hr(),
  wellPanel(fluidRow(column(12, h4("General settings"))),
  fluidRow(column(4,uiOutput("measureAll")),column(4,uiOutput("algorithm")),column(4,uiOutput("defaultchoice")))),
  hr(),
  fluidRow(column(12, h4(p("(around 10 seconds loading time for each panel)", style = "color:blue")))),
  tabsetPanel(
    tabPanel("Surrogate models comparison", 
      fluidRow(
        column(12, h4("Average mean of different surrogate models (for not-NA results)"), tableOutput("bmr_result"))),
      hr(),
      column(12, h4("Distribution of the performances on the different datasets")),
      fluidRow(
        column(6, uiOutput("logscale")), column(6, uiOutput("bmr_measure"))),
      plotOutput("plot1", width = "95%")#,
      #plotOutput("plot2", width = "95%")
    ),
    
    tabPanel("Defaults and Tunability", 
      fluidRow(
        column(12, h4("Defaults"), tableOutput("defaults"))), 
      hr(),
      fluidRow(
        column(12, h4("Tunability"),
          column(12, uiOutput("scaled")),
          column(12, fluidRow(
            column(1, h5("Overall mean tunability"), tableOutput("overallTunability")),
            column(11, h5("Hyperparameters (mean)"), tableOutput("tunability"))
          )))),
      plotlyOutput("plot3", width = "95%", inline = F),
      sliderInput("yrange",  "Y-range:", min = 0, max = 0.5, value = c(0, 0.025), width = "800px"),
      hr(),
      fluidRow(column(12, h4("Tuning Space"),
        column(12, uiOutput("quantile")),
        column(12, "Numerics", align="left", tableOutput("tuningSpaceNumerics")),
        column(12, "Factors", align="left", tableOutput("tuningSpaceFactors"))
      )),
      hr(),
      fluidRow(column(12, h4("Histogram of best hyperparameter on each of the datasets (possible prior for tuning)")),
        column(12, uiOutput("visual3"))),
      plotlyOutput("plot4", width = "95%", inline = F),
      sliderInput("nrbin",  "Number of bins:", min = 0, max = 50, value = c(6), width = "800px")
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
        column(12, h4("Combined tunability and interaction effects")),
        column(12, tableOutput("combiTable")))
      #)
      #)
    )
  )
)



shinyApp(ui = ui, server = server)