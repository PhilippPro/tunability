# library(ggplot2)
# library(tidyr)
library(shiny)
# library(shinydashboard)
# library(shinyjs)
# library(shinyBS)
library(data.table)
library(DT)
library(mlr)
load(file = "../results.RData")
# surrogate models evtl. entfernen aus der Datei

server = function(input, output) {
  
  learner.names = names(results)
  
  output$algorithm = renderUI({
    selectInput('algo', 'Algorithm', learner.names, selected = learner.names[1], multiple = FALSE)
  })
  
  bmrInput = reactive({
    bmr_surrogate[[which(learner.names == input$algo)]]
  })
  
  bmrAggr = reactive({
    perfs = data.table(getBMRAggrPerformances(bmrInput(), as.df = T, drop = T))[, -"task.id"]
    perfs = data.frame(perfs[, lapply(list(mse = mse.test.mean, rsq = rsq.test.mean, kendalltau = kendalltau.test.mean, 
      spearmanrho = spearmanrho.test.mean),function(x) mean(x, na.rm = T)), by = "learner.id"])
    perfs$learner.id =  sub('.*\\.', '', as.character(perfs$learner.id))
    perfs
  })
  
  output$bmr_result = renderTable({
    bmrAggr()
  }, digits = 5)
  
  output$plot1 = renderPlot({
    plotBMRSummary(bmrInput())
  })
  
  output$plot2 = renderPlot({
    plotBMRRanksAsBarChart(bmrInput(), pos = "stack")
  })
  
  output$task = renderUI({
    selectInput('taski', 'Task', c("classification", "regression"), selected = "classification", multiple = FALSE)
  })
  
  output$defaults = renderTable({
    results[[input$algo]]$default$default
  }, digits = 3)
  
  overall = reactive({
    calculateTunability(results[[input$algo]]$default, results[[input$algo]]$optimum)
  })
  
  output$overallTunability = renderTable({
    mean(overall())
  }, colnames = FALSE, digits = 3)
  
  
  
  tunabilityValues = reactive({
    calculateTunability(results[[input$algo]]$default, results[[input$algo]]$optimumHyperpar)
  })
  
 tunabilityValuesMean = reactive({
   colMeans(calculateTunability(results[[input$algo]]$default, results[[input$algo]]$optimumHyperpar))
 })
 
 output$scaled = renderUI({
   selectInput('scaled', 'Scaled', c(TRUE, FALSE), selected = FALSE, multiple = FALSE)
 })
   
 output$tunability = renderTable({
   if (input$scaled == FALSE) {
     data.frame(t(tunabilityValuesMean()))
   } else {
     data.frame(t(colMeans(tunabilityValues()/overall(), na.rm = T)))
   }
 }, digits = 3)
  
 output$visual = renderUI({
   selectInput('visual', 'Visualization', c("Density", "Histogram"), selected = "Density", multiple = FALSE)
 })
 
 output$visual2 = renderUI({
   selectInput('visual2', 'Hyperparameter', c("All", names(tunabilityValuesMean())), selected = "All", multiple = FALSE)
 })
 
 output$plot3 = renderPlot({
   if (input$visual2 == "All") {
     x = overall()
   } else {
     x = tunabilityValues()[, input$visual2]
   }
   if (input$visual == "Density")
     plot(density(x), main = "Density of the Overall Tunability")
   else {
     bins = seq(min(x), max(x), length.out = input$bins + 1)
     hist(x, breaks = bins, main = "Histogram of the Overall Tunability")
   }
 })
 
  output$tuningSpaceNumerics = renderTable({
    results[[input$algo]]$tuningSpace$numerics
  }, rownames = TRUE, digits = 3)
  
  output$tuningSpaceFactors = renderTable({
    results[[input$algo]]$tuningSpace$factors
  })
  
  output$combi = renderUI({
    selectInput('combination', 'Tunability of combinations of two hyperparameters', 
      c("Tunability", "Interaction effect", "Performance gain"), 
      selected = "Tunability", multiple = FALSE)
  })
  
  output$combiTable <- renderTable({
    tab = colMeans(results[[input$algo]]$optimumTwoHyperpar$optimum, dims = 1, na.rm = TRUE) - mean(results[[input$algo]]$default$result)
    if(input$combination == "Tunability") {
      diag(tab) = tunabilityValuesMean()
    } else {
    if(input$combination == "Interaction effect") {
      tab = tab - mean(outer(tunabilityValuesMean(), tunabilityValuesMean(), '+'))
    } else {
      tab = tab - outer(tunabilityValuesMean(), tunabilityValuesMean(), pmax)
    }
    }
    colnames(tab) = rownames(tab) = names(tunabilityValuesMean())
    tab
  }, rownames = TRUE, digits = 4)
}

ui = fluidPage(
  titlePanel("Summary of the benchmark results (AUC)"),
  
  sidebarLayout(
    sidebarPanel(
      #uiOutput("task"),
      uiOutput("algorithm")
    ),
    
    tabsetPanel(
      tabPanel("Surrogate models comparison", 
        fluidRow(
          column(12, "Average mean of different surrogate models", tableOutput("bmr_result"))),
        "Performance on datasets", plotOutput("plot1"), 
        "Frequency of ranks", plotOutput("plot2")),
      tabPanel("Defaults and Tunability", 
        fluidRow(
          column(12, "Defaults", tableOutput("defaults"))), 
        fluidRow(
          column(12, "Tunability", 
            column(12, fluidRow(uiOutput("scaled"))),
            column(12, fluidRow(
            column(1, "Overall mean tunability", tableOutput("overallTunability")), 
            column(11, "Hyperparameters", tableOutput("tunability"))
          )))),
        fluidRow(column(6, uiOutput("visual")),
          column(6, uiOutput("visual2"))),
        plotOutput("plot3"),
        
        conditionalPanel(
          condition = "input.visual== 'Histogram'",
          sliderInput("bins",  "Number of bins:", min = 1, max = 50, value = 30)
        ),
      
      fluidRow(column(12, "Tuning Space",
        column(12, "Numerics", align="left", tableOutput("tuningSpaceNumerics")),
        column(12, "Factors", align="left", tableOutput("tuningSpaceFactors"))
      ))
      ),
      tabPanel("Interaction effects",
      uiOutput("combi"),
      tableOutput("combiTable")
      )
    )
  )
)


shinyApp(ui = ui, server = server)
