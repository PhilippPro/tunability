library(ggplot2)
library(tidyr)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(DT)
library(mlr)
load(file = "../results.RData")
# surrogate models evtl. entfernen aus der Datei

server = function(input, output) {
  
  
  #load(file = "./hyperpars.RData")
  learner.names = names(results)
  
  output$plot1 <- renderPlot({
    plotBMRSummary(bmrInput())
  })
  output$plot2 <- renderPlot({
    plotBMRRanksAsBarChart(bmrInput(), pos = "stack")
  })
  # a = "mlr.classif.glmnet"
  # bmr_surrogate[a]
  
  bmrInput <- reactive({
    i = which(learner.names == input$algo)
    bmr_surrogate[[i]]
  })
  
  output$task = renderUI({
    selectInput('taski', 'Task', c("classification", "regression"), selected = "classification", multiple = FALSE)
  })
  
  output$algorithm = renderUI({
    selectInput('algo', 'Algorithm', learner.names, selected = learner.names[1], multiple = FALSE)
  })
  
  output$defaults = renderTable({
    results[[input$algo]]$default$default
  })
  
  # defaults = reactive({ 
  #   results[[input$algo]]$default$default
  #   })
  
  # hyperparTable = reactive({
  #   hyperpars[hyperpars$run.id %in% unique(results[results$flow.name == input$lrn & results$data.name == input$ds,]$run.id), ]
  # })
  

  
  # hyperparValues = reactive({
  #   #tidyr::spread(hyperpars, hyperpar.name, hyperpar.value, fill = NA)
  #   df = reshape(hyperparTable(), idvar = "run.id", timevar = "hyperpar.name", direction = "wide")
  #   colnames(df) = gsub("hyperpar.value.", "", colnames(df))
  #   df
  # })
  
  # output$summary.vis.hist = renderUI({
  #   list(
  #     column(9,
  #       sliderInput("summary.vis.hist.nbins", "Number of bins", min = 1L, max = 100L,
  #         value = 30L, step = 1L, width = "95%")
  #     ),
  #     column(3,
  #       radioButtons("summary.vis.dens", "Show density?", choices = c("Yes", "No"),
  #         selected = "Yes", inline = TRUE)
  #     )
  #   )
  # })

}

# shinyUI(fluidPage(
#   
#   titlePanel("Tabsets"),
#   
#   sidebarLayout(
#     
#     sidebarPanel(
#       # Inputs excluded for brevity
#     ),
#     
#     mainPanel(
#       tabsetPanel(
#         tabPanel("Plot", plotOutput("plot")), 
#         tabPanel("Summary", verbatimTextOutput("summary")), 
#         tabPanel("Table", tableOutput("table"))
#       )
#     )
#   )
# ))




ui = fluidPage(
  titlePanel("Summary of the benchmark results"),
  
  sidebarLayout(
    sidebarPanel(
      #uiOutput("task"),
      uiOutput("algorithm")
    ),
    
    tabsetPanel(
      tabPanel("Surrogate models comparison", 
        "Performance on different tasks", plotOutput("plot1"), 
        plotOutput("plot1")),
      tabPanel("Best defaults", tableOutput("defaults"))
    )
  )
)

shinyApp(ui = ui, server = server)


# 
# box(width = 12, title = "Variable Visualization", id = "summary.vis.box",
#   fluidRow(
#     column(12,
#       uiOutput("summary.vis.hist")),
#     column(12,
#       plotOutput("summary.vis")
#     )
#   )
# )