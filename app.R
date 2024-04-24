library(shiny)
library(bslib)
library(ggplot2)
library(bsicons)
library(dplyr)
library(nplyr)

# https://rstudio.github.io/bslib/articles/dashboards/index.html

# R dtplyr: How to Efficiently Process Huge Datasets with a data.table Backend 
# https://www.appsilon.com/post/r-dtplyr?utm_source=community&utm_medium=shiny4all%20&utm_campaign=blog

# tooltip(
# bs_icon("info-circle"),
# "Tooltip message"
# )
fig4 <- readRDS("data/fig4.rds")

all_proteins <- fig4[[2]] |>
  purrr::map(purrr::pluck(1)) |>
  unlist() |>
  unique()

# have this as a list of tibbles so we don't need to filter within the app


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
      #h2("Some pretty pictures here", ),
      #actionButton(inputId = "back_to_main2", "Back to main"),
      card(
        full_screen = TRUE,
        card_header(
          "Some proteomics data",
          popover(
            bsicons::bs_icon("gear", class = "ms-auto"),
            title = "Highlight options",
            selectizeInput(
              inputId   = "prot_to_highlight", 
              label     = 'Select protein(s) to highlight', 
              choices   = all_proteins, 
              multiple  = TRUE
            ),
            checkboxInput(
              inputId = "show_names",
              label   = "display protein names"
            )
          ),
          actionButton(inputId = "back_to_main2", "Back to main"),
          class = "d-flex align-items-center gap-1"
        ),
        layout_columns(
          card(
            card_header("FoB"),
            plotOutput("proteomics1"),
          ),
          card(
            card_header("MZ"),
            plotOutput("proteomics2"),
          )
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
  
  output$proteomics1 <- renderPlot({
    plot1()
  })
  
  output$proteomics2 <- renderPlot({
    plot2()
  })
  
  plot1 <- reactive({
    i <- 1
    title <- pull(fig4[i,1])
    dataset <- pull(fig4[i,2])[[1]]
    data_cols <- colnames(dataset)[2:3]
    label_column <- colnames(dataset)[1]
        
    p <- dataset |> 
      ggplot(mapping = aes(x=.data[[data_cols[[1]]]], y=.data[[data_cols[[2]]]])) +
      geom_point(size=0.8, colour = "grey") +
      theme_bw()
    
    if (isTruthy(filtered_subset())) {
      filt <- pull(filtered_subset()[i,2])[[1]] #|>
      p <- p +
        geom_point(data=filt, colour = "red")
    }
    if (input$show_names){
      p <- p +
        geom_text(data=filt, colour = "red" , aes(label = .data[[label_column]]), vjust = 1)
    }
    
    p
  })
  
  plot2 <- reactive({
    
    ggplot()
    
    # p <- fig4 |> 
    #   filter(cell_type == "MZ") |>
    #   ggplot(mapping = aes(x=log10(TPM), y=log10(Copy_number))) +
    #     geom_point(size=0.8, colour = "grey") +
    #     theme_bw()
    # 
    # if (isTruthy(filtered_subset())) {
    #   filt <- filtered_subset() |>
    #     filter(cell_type == "MZ")
    #   p <- p +
    #     geom_point(data=filt, colour = "red")
    # }
    # if (input$show_names){
    #   p <- p +
    #     geom_text(data=filt, colour = "red" , aes(label = `Protein IDs`), vjust = 1)
    # }
    # p
  })
  
  observeEvent(input$prot_to_highlight, {
    
    if(is.null(input$prot_to_highlight)) {
      filtered_subset(NULL)
    } else {
    
    filt <- fig4 |>
      #lazy_dt() |>
      nplyr::nest_filter(data, `Protein IDs` %in% input$prot_to_highlight)
      #as_tibble()
    
    filtered_subset(filt)
    }
  })
  
  filtered_subset <- reactiveVal()
  
}


shinyApp(ui, server)