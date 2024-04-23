library(shiny)
library(bslib)
library(ggplot2)
library(bsicons)

# https://rstudio.github.io/bslib/articles/dashboards/index.html

# R dtplyr: How to Efficiently Process Huge Datasets with a data.table Backend 
# https://www.appsilon.com/post/r-dtplyr?utm_source=community&utm_medium=shiny4all%20&utm_campaign=blog

# tooltip(
# bs_icon("info-circle"),
# "Tooltip message"
# )
fig4 <- readRDS("data/fig4.rds")

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
      actionButton(inputId = "back_to_main", "Back to main"),
      card(
        card_header("UMAP")#,
        #plotOutput("proteomics")
      )
    ),
    tabPanelBody(
      "data2",
      h2("Some pretty pictures here"),
      actionButton(inputId = "back_to_main2", "Back to main"),
      card(
        card_header("Some proteomics data"),
        plotOutput("proteomics"),
        selectizeInput(
          inputId   = "prot_to_highlight", 
          label     = 'Select protein(s) to highlight', 
          choices   = unique(fig4$`Protein IDs`), 
          multiple  = TRUE
        ),
        actionButton("browser", "browser")
      )
    )
  ) 
)

server <- function(input, output, session) {
  
  observeEvent(input$browser, browser())
  
  observeEvent(input$single_cell, {
    updateTabsetPanel(session, "main_panel", selected = "data1")
  })
  observeEvent(input$proteomics, {
    updateTabsetPanel(session, "main_panel", selected = "data2")
  })
  observeEvent(input$back_to_main, {
    updateTabsetPanel(session, "main_panel", selected = "landing_page")
  })
  observeEvent(input$back_to_main2, {
    updateTabsetPanel(session, "main_panel", selected = "landing_page")
  })
  
  output$proteomics <- renderPlot({
    plot_to_display()
  })
  
  plot_to_display <- reactive({
    
    p <- fig4 |>
      ggplot(mapping = aes(x=log10(TPM), y=log10(Copy_number))) +
      geom_point(size=0.8, colour = "grey") +
      facet_wrap(vars(cell_type), nrow=1) +
      theme_bw()
    
    if (isTruthy(filtered_subset())) {
      p <- p +
        geom_point(data=filtered_subset(), colour = "red")
    }
    p
  })
  
  observeEvent(input$prot_to_highlight, {
    
    if(is.null(input$prot_to_highlight)) {
      filtered_subset(NULL)
    } else {
    
    filt <- fig4 |>
      #lazy_dt() |>
      dplyr::filter(`Protein IDs` %in% input$prot_to_highlight)
      #as_tibble()
    
    filtered_subset(filt)
    }
  })
  
  filtered_subset <- reactiveVal()
  
}


shinyApp(ui, server)