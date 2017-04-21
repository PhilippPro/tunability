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
  
  output$overallTunability = renderTable({
    mean(results[[input$algo]]$overallTunability)
  }, colnames = FALSE, digits = 3)
  
  output$visual = renderUI({
    selectInput('visual', 'Visualization', c("Density", "Histogram"), selected = "density", multiple = FALSE)
  })
  
  output$plot3 <- renderPlot({
    if(input$visual == "Density")
      plot(density(results[[input$algo]]$overallTunability), main = "Density of the Overall Tunability")
    else 
      hist(results[[input$algo]]$overallTunability, main = "Density of the Overall Tunability")
  })
  
  output$tunability = renderTable({
    data.frame(t(results[[input$algo]]$tunability))
  }, digits = 3)
  
  output$tuningSpaceNumerics = renderTable({
    results[[input$algo]]$tuningSpace$numerics
  }, rownames = TRUE, digits = 3)
  
  output$tuningSpaceFactors = renderTable({
    results[[input$algo]]$tuningSpace$factors
  })
  
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
          column(12, "Tunability", fluidRow(
            column(1, "Overall mean tunability", tableOutput("overallTunability")), 
            column(11, "Hyperparameters", tableOutput("tunability"))
          ))),
        uiOutput("visual"),
        plotOutput("plot3"), 
      
      fluidRow(column(12, "Tuning Space",
        column(12, "Numerics", align="left", tableOutput("tuningSpaceNumerics")),
        column(12, "Factors", align="left", tableOutput("tuningSpaceFactors"))
      ))
      )
    )
  )
)


shinyApp(ui = ui, server = server)
