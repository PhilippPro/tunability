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
  
  rv <- reactiveValues()
  rv$setupComplete <- FALSE
  
  ## simulate data load
  observe({
    if(input$btn_data){
      ## set my condition to TRUE
      rv$setupComplete <- TRUE
    }
    
    ## the conditional panel reads this output
    output$setupComplete <- reactive({
      return(rv$setupComplete)
    })
    outputOptions(output, 'setupComplete', suspendWhenHidden=FALSE)
  })
  
  
  measure.names = names(app_data)
  learner.names = names(app_data$auc$results)
  
  output$measureAll = renderUI({
   selectInput('meas', 'Performance measure', measure.names, selected = measure.names[1], multiple = FALSE)
  })
  
  output$algorithm = renderUI({
   selectInput('algo', 'Algorithm', learner.names, selected = learner.names[1], multiple = FALSE)
  })
  
  output$defaultchoice <- renderUI({
    selectInput('defaultchoice', 'Defaults', c("Optimal defaults", "Package defaults"), selected = "Optimal defaults", multiple = FALSE)
  })
   
   bmrInput = reactive({
     inputi = "glmnet"
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
       spearmanrho = spearmanrho.test.mean),function(x) mean(x)), by = "surrogate"])
     perfs
   })

   output$logscale = renderUI({
     selectInput('logscale', 'Logarithmic scale', c("No", "Yes"), selected = "No", multiple = FALSE)
   })
   
   output$bmr_measure = renderUI({
     measures = gsub("\\..*","",colnames(bmrInput())[-c(1,2)])
     selectInput('bmr_measure', 'Performance measure', measures, selected = measures[2], multiple = FALSE)
   })

   output$bmr_result = renderTable({
     bmrAggr()
   }, digits = 5)
   
  #df_test = app_data$auc$surrogate$glmnet
  #plotBMRSummary
  #bmr_measure = gsub("\\..*","",colnames(app_data$auc$surrogate$glmnet)[-c(1,2)])
  #learner_ids = gsub("\\..*","",learner.names)
   url <- a("Bot Paper", href="https://arxiv.org/pdf/1806.10961.pdf")
   
   output$tab <- renderUI({
     tagList("In the plot below the performances of the surrogate models on the different datasets is depicted. 
   The data_id's correspond to the dataset ids of OpenML, for details see also the", url, ".")
   })

  output$plot1 = renderPlot({
     sel_meas = paste0(input$bmr_measure, ".test.mean")
     p = ggplot(bmrInput(), aes_string(x = sel_meas, y = "task.id", col = "surrogate", shape = "surrogate"))
     p = p + geom_point(size = 4L, position = position_jitter(width = 0, height = 0.05))
     p = p + scale_shape_manual(values = rep(19, length(learner.names)))
     p = p + ylab("Data_id")
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
    if (input$defaultchoice == "Optimal defaults") {
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
      mean(overall()/overall(), na.rm = TRUE)
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
    ggplot(dataf, aes(x = ind, y = values)) + geom_boxplot() + coord_cartesian(ylim = c(input$yrange[1],input$yrange[2])) +
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

    inputi = "glmnet"
    
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
    numericInput('quantile', 'Quantile for tuning space calculation', 0.1, min = 0, max = 1)
  })

  tuningSpace = reactive({
    tab = calculateTuningSpace(app_data[[input$meas]]$results[[input$algo]]$optimum, quant = input$quantile)
    tab$numerics = cbind(Quantile = rownames(tab$numerics), tab$numerics)
    tab
  })

  output$tuningSpaceNumerics = renderTable({
    tuningSpace()$numerics
  }, rownames = FALSE, digits = 3)

  output$tuningSpaceFactors = renderTable({
    tuningSpace()$factors
  })

  output$combi = renderUI({
    selectInput('combination', 'Measures',
      c("Tunability", "Joint gain", "Interaction effect"),
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
  conditionalPanel(condition = "!output.setupComplete",
    column(12, h2(p("Tunability Shiny App"))),
    column(12, h5(p("This app contains additional material for the paper 'Tunability: Importance of Hyperparameters of Machine Learning Algorithms'.
      For starting the app, just click the button:"))),
    column(12, align = "center", actionButton(inputId = "btn_data", label = "Start the shiny app!", width = '400px', 
      style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
    br(),
    hr(),
    br(),
    hr(),
    br(),
    hr(),
    column(10, h3(p("Tunability: Importance of Hyperparameters of Machine Learning Algorithms"))),
    br(),
    br(),
    hr(),
    column(10, h5(p("Authors: Philipp Probst, Bernd Bischl, Anne-Laure Boulesteix"))),
    br(),
    hr(),
    column(10, h4(p("Paper Abstract"))),
    column(10, h5(p("Modern supervised machine learning algorithms involve hyperparameters that have to be set before running them. 
Options for setting hyperparameters are default values from the software package, manual configuration by the user or configuring them for optimal predictive performance by a tuning procedure. 
      The goal of this paper is two-fold. 
      Firstly, we formalize the problem of tuning from a statistical point of view, define data-based defaults and suggest general measures quantifying the tunability of hyperparameters of algorithms. 
      Secondly, we conduct a large-scale benchmarking study based on 38 datasets from the OpenML platform and six common machine learning algorithms. 
      We apply our measures to assess the tunability of their parameters. 
      Our results yield default values for hyperparameters and enable users to decide whether it is worth conducting a possibly time consuming tuning strategy, to focus on the most important hyperparameters and to choose adequate hyperparameter spaces for tuning. ")))
  ),
  conditionalPanel(condition = "output.setupComplete",
    titlePanel("Tunability Shiny App"),
    hr(),
    wellPanel(fluidRow(column(12, h4("General settings"))),
      fluidRow(column(4,uiOutput("measureAll")),column(4,uiOutput("algorithm")),column(4,uiOutput("defaultchoice")))),
    hr(),
    fluidRow(column(12, h4(p("(around 10 seconds loading time for each panel)", style = "color:blue")))),
    tabsetPanel(
      tabPanel("Surrogate models comparison", 
        fluidRow(column(12, h2("Comparison of the quality of surrogate models")),
          column(12, h5("The calculation of the tunability is based on the surrogate models. 
        Hence, it is important to evaluate the performance of the surrogate model. 
        In this panel five different surrogate models are compared. 
        For the final calculation of the tunability measures the ranger surrogate models is chosen because it provides good and stable results.
        See also section 5.1 in the paper."))),
        hr(),
        fluidRow(column(12, h4("Average performances of the surrogate models on the different datasets")),
          column(12, tableOutput("bmr_result"))),
        hr(),
        fluidRow(column(12, h4("Distribution of the surrogate model performances on the different datasets")),
          column(12, uiOutput("tab")),
          column(6, uiOutput("logscale")), column(6, uiOutput("bmr_measure")),
          plotOutput("plot1", width = "95%"))#,
        #plotOutput("plot2", width = "95%")
      ),
      
      tabPanel("Defaults and tunability", 
        fluidRow(column(12, h2("Defaults and tunability")), 
          column(12, h5("In this panel the defaults and the correspending tunabilities are depicted. For details see sections 3.2, 3.3, 3.4, 5.2 and 5.3 in the paper."))),
        hr(),
        fluidRow(column(12, h4("Defaults")),
          column(12, h5("This table contains the default hyperparameter values that are used for the calculation of the tunability values.
        The optimal defaults were calculated by taking the best average performance of a hyperparameter setting on all datasets. 
        The package defaults are given by the corresponding R-packages.")),
          column(12, tableOutput("defaults"))), 
        hr(),
        fluidRow(
          column(12, h4("Tunability")),
          column(12, h5("The tunability values are calculated by taking the best performance of a hyperparameter setting on a 
          dataset (overall and for single hyperparameters) and subtracting the performance of the default hyperparameter setting.")),
          column(12, h4("Mean tunability over the datasets")),
          column(12, h5("For the following table the mean of the tunabilities of all the datasets is taken to provide one measure of tunability for each parameter.
            The scaled version divides the tunability per hyperparameter by the overall tunability of the algorithm per dataset and takes the mean afterwards.")),
          column(12, fluidRow(
            column(1, h5("Overall mean tunability"), tableOutput("overallTunability")),
            column(11, h5("Hyperparameters"), tableOutput("tunability"))
          )),
          column(12, uiOutput("scaled"))
        ),
        fluidRow(column(12, h4("Boxplot of tunability values per dataset")),
          br(),
          br(),
          column(12, h4("Tunability values per dataset")),
          plotlyOutput("plot3", width = "95%", inline = F),
          sliderInput("yrange",  "Y-axis limits:", min = 0, max = 0.5, value = c(0, 0.06), width = "800px")
        )),
      tabPanel("Combined tunability",
        fluidRow(column(12, h2("Tunability of hyperparameter combinations and joint gain")), 
          column(12, h5("In this panel the tunabilities of hyperparameter combinations and joint gains can be seen. For details see section 3.5 and 5.4 in the paper."))),
        hr(),
        fluidRow(column(12, uiOutput("combi")),
          column(12, h4("Combined tunability and interaction effects")),
          column(12, h5("The tunability values of the single hyperparameters are depicted on the diagonal, the combined tunabilities on the upper right of 
          the table. For details of the calculation (also of the joint gain) see section 3.5 in the paper.")),
          column(12, tableOutput("combiTable")))
        #)
        #)
      ),
      tabPanel("Tuning space",
        fluidRow(column(12, h2("Hyperparameter ranges for tuning and priors")), 
          column(12, h5("In this panel the optimal hyperparameter ranges are depicted. For details see sections 3.6 and 5.5 in the paper."))),
        hr(),
        fluidRow(column(12, h4("Tuning Space"),
          column(12, h5("The tuning space is calculated by taking the best hyperparameters on each dataset and calculating the quantiles of these.")),
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
        # ))
      )
    )
  )
)



shinyApp(ui = ui, server = server)