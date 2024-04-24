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
# 
# this is a list of tibbles so we don't need to filter for cell type within the app
fig4 <- readRDS("data/fig4.rds")

all_proteins <- fig4[[2]] |>
  purrr::map(purrr::pluck(1)) |>
  unlist() |>
  unique()

create_card <- function(i, data = fig4){
  card(
    full_screen = TRUE,
    card_header(fig4[[1]][[i]]),
    mod_scatter_ui(paste0("prot",i))
  )
}


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
            checkboxInput(
              inputId = "show_names",
              label   = "display selected names on plot"
            ),
            selectizeInput(
              inputId   = "prot_to_highlight", 
              label     = 'Select protein(s) to highlight', 
              choices   = all_proteins, 
              multiple  = TRUE
            )
          ),
          actionButton(inputId = "back_to_main2", "Back to main"),
          class = "d-flex align-items-center gap-1"
        ),
        layout_columns(
          create_card(1),
          create_card(2),
          create_card(3),
          create_card(4)
        ),
        actionButton("browser", "browser")
      )
    )
  ) 
)

server <- function(input, output, session) {
  
  filtered_subset <- reactiveVal()
  display_names <- reactive(input$show_names)
  
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

  mod_scatter_server(id = "prot1", fig4, i=1, subset = filtered_subset, show_names = display_names)
  mod_scatter_server(id = "prot2", fig4, i=2, subset = filtered_subset, show_names = display_names)
  mod_scatter_server(id = "prot3", fig4, i=3, subset = filtered_subset, show_names = display_names)
  mod_scatter_server(id = "prot4", fig4, i=4, subset = filtered_subset, show_names = display_names)

  # this didn't work  
  # for (x in 1:4){
  #   mod_scatter_server(id = paste0("prot",x), fig4, i=x, subset = filtered_subset, show_names = display_names)
  # }

  observeEvent(input$prot_to_highlight, ignoreNULL = FALSE, {
    
    if(is.null(input$prot_to_highlight)) {
      filtered_subset(NULL)
    } else {
      filt <- fig4 |>
        nplyr::nest_filter(data, `Protein IDs` %in% input$prot_to_highlight)
      filtered_subset(filt)
    }
  })

}

shinyApp(ui, server)
