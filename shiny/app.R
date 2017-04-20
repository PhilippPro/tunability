# library(ggplot2)
# library(tidyr)
library(shiny)
# library(shinydashboard)
# library(shinyjs)
# library(shinyBS)
library(DT)
library(mlr)
load(file = "../results.RData")
# surrogate models evtl. entfernen aus der Datei

server = function(input, output) {
  
  learner.names = names(results)
  
  bmrInput = reactive({
    bmr_surrogate[[which(learner.names == input$algo )]]
  })
  
  bmrAggr = reactive({
    perfs = data.table(getBMRAggrPerformances(bmrInput(), as.df = T, drop = T))
    namen = unique(perfs$learner.id)
    perfs = data.frame(t(perfs[, mean(mse.test.mean), by = "learner.id"]$V1))
    colnames(perfs) = namen
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
  
  output$algorithm = renderUI({
    selectInput('algo', 'Algorithm', learner.names, selected = learner.names[1], multiple = FALSE)
  })
  
  output$defaults = renderTable({
    results[[input$algo]]$default$default
  }, digits = 3)
  
  output$overallTunability = renderTable({
    mean(results[[input$algo]]$overallTunability)
  }, colnames = FALSE, digits = 3)
  
  output$plot3 <- renderPlot({
    plot(density(results[[input$algo]]$overallTunability), main = "Density of the Overall Tunability")
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
  titlePanel("Summary of the benchmark results"),
  
  sidebarLayout(
    sidebarPanel(
      #uiOutput("task"),
      uiOutput("algorithm")
    ),
    
    tabsetPanel(
      tabPanel("Surrogate models comparison", 
        fluidRow(
          column(12, "Average mean square error of different surrogate models", tableOutput("bmr_result"))),
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
