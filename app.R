library(shiny)
library(bslib)
library(ggplot2)
library(bsicons)

# https://rstudio.github.io/bslib/articles/dashboards/index.html

# tooltip(
# bs_icon("info-circle"),
# "Tooltip message"
# )


ui <- page_fillable(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  title = "Data explorer",  
  tabsetPanel(
    id = "main_panel",
    type = "hidden",
    tabPanelBody(
      "landing_page",
      papercard,
      br(),
      h4("Data types available to explore from this paper", style = "text-align: center;"),
      layout_columns(
        fill = FALSE,
        value_boxes[["single_cell"]],
        value_boxes[["proteomics"]],
        value_boxes[["imaging"]]
      )
    ),
    tabPanelBody(
      "data1",
      h2("Some pretty pictures here"),
      actionButton(inputId = "back_to_main", "Back to main")
    )
  ) 
)

server <- function(input, output, session) {
  
  observeEvent(input$single_cell, {

    updateTabsetPanel(session, "main_panel", selected = "data1")
  })
  
  observeEvent(input$back_to_main, {
    updateTabsetPanel(session, "main_panel", selected = "landing_page")
  })
  
  # gg_plot <- reactive({
  #   ggplot(penguins) +
  #     geom_density(aes(fill = !!input$color_by), alpha = 0.2) +
  #     theme_bw(base_size = 16) +
  #     theme(axis.title = element_blank())
  # })
  # 
  # output$bill_length <- renderPlot(gg_plot() + aes(bill_length_mm))
  # output$bill_depth <- renderPlot(gg_plot() + aes(bill_depth_mm))
  # output$body_mass <- renderPlot(gg_plot() + aes(body_mass_g))
}


shinyApp(ui, server)