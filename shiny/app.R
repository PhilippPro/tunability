library(ggplot2)
library(tidyr)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(DT)

server = function(input, output) {
  
  #load(file = "./results.RData")
  #load(file = "./hyperpars.RData")
  
  output$task = renderUI({
    selectInput('taski', 'Task', c("classification", "regression"), selected = "classification", multiple = FALSE)
  })
  
  output$algorithm = renderUI({
    selectInput('algo', 'Algorithm', 1:4, selected = 3, multiple = FALSE)
  })
  
  # hyperparTable = reactive({
  #   hyperpars[hyperpars$run.id %in% unique(results[results$flow.name == input$lrn & results$data.name == input$ds,]$run.id), ]
  # })
  
  # output$learnerTable = DT::renderDataTable({
  #   hyperparValues()
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

ui = fluidPage(
  titlePanel("Tunability of Hyperparameters"),
  
  uiOutput("task"),
  uiOutput("algorithm")

  # 
  # mainPanel(
  #   DT::dataTableOutput("learnerTable")
  # ),
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
  )

shinyApp(ui = ui, server = server)