library(shiny)
library(ggplot2)
library(dplyr)

mod_scatter_ui <- function(id){
  
  ns <- NS(id)
  tagList(
    plotOutput(outputId = ns("scatter")),
    #actionButton(ns("browser"), "browser")
  )
} 

mod_scatter_server <- function(id, tbl_list, i=1, subset = NULL, show_names) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      observeEvent(input$browser, browser())
      
      output$scatter <- renderPlot(gg_plot())
      
      # extract the dataset from the nested list of tibbles
      dataset <- reactive(tbl_list[i,2][[1]][[1]])
      
      # assuming that the first column is the protein name, then 2 and 3 are the data values
      label_column <- reactive(colnames(dataset())[1])
      data_cols <- reactive(colnames(dataset())[2:3])
      
      
      base_plot <- reactive({
        dataset() |> 
          ggplot(mapping = aes(x=.data[[data_cols()[[1]]]], y=.data[[data_cols()[[2]]]])) +
          geom_point(size=0.8, colour = "grey") +
          theme_bw()
      })
      
      highlight_set <- reactive({
        req(isTruthy(subset()))
        subset()[i,2][[1]][[1]]
      })

      highlighted_plot <- reactive({
        req(highlight_set())
        base_plot() +
          geom_point(data=highlight_set(), colour = "red")
      })

      highlighted_name_plot <- reactive({
        req(highlight_set(), show_names())
        highlighted_plot() +
          geom_text(data=highlight_set(), colour = "red" , aes(label = .data[[label_column()]]), vjust = 1)
      })

      gg_plot <- reactive({
        
        if(!isTruthy(subset())) return (base_plot())
        if(show_names()) return (highlighted_name_plot())
        else highlighted_plot()
      })
      
  })
}      


