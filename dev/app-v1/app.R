library(shiny)
library(tidyverse)
library(reticulate)
library(DT)
library(bslib)

# --- ENVIRONMENT CONFIG ---
# source_python("python/backend.py") # Loaded below after path check

# --- FILE SETUP ---
# Update this to your ACTUAL file name (no dummy)
csv_filename <- "dd-abcd-6_0_minimal_noimag.csv" 

if (!file.exists(csv_filename)) {
  # Fallback for testing if the user hasn't renamed the file yet
  if (file.exists("dd-abcd-6_0_minimal_noimag-dummy.csv")) {
    csv_filename <- "dd-abcd-6_0_minimal_noimag-dummy.csv"
  } else {
    stop(paste("CRITICAL ERROR: Could not find", csv_filename))
  }
}

# Get Absolute Path and Initialize
abs_path <- tools::file_path_as_absolute(csv_filename)
source_python("python/backend.py") # Load the script

# res <- semantic_search("give variables of BMI", data_path = "../../data/")       # Run the init function
# res[[3]] |> length()
# # --- UI HELPERS ---
# unique_sources <- get_unique_values("source")
# unique_types   <- get_unique_values("type_var")


# load dictionary ---------------------------------------------------------

dd <- read.csv(
  csv_filename
)

# --- UI ---
ui <- page_fillable(
  theme = bs_theme(preset = "flatly"),
  
  tags$head(
    tags$style(HTML("
      .card { height: 100%; } 
      .form-group { margin-bottom: 15px; }
    "))
  ),
  
  title = "ABCD Semantic Search",
  
  layout_columns(
    # col_widths = c(3, 6, 3),
    col_widths = c(3, 9),
    fill = TRUE,
    
    # --- LEFT: SEARCH INPUT ---
    card(
      card_header("Search Parameters"),
      
      textAreaInput("search_query", "Describe what you are looking for:", 
                    placeholder = "e.g., bullying at school",
                    height = "150px"),
      
      # [UPDATED] Similarity Cutoff Slider
      sliderInput("cutoff", "Similarity Threshold:", 
                  min = 0.0, max = 1.0, value = 0.25, step = 0.05),
      
      helpText("Higher values = stricter matching. Lower values = more results."),
      
      actionButton("run_search", "Search", 
                   class = "btn-primary w-100", icon = icon("magnifying-glass"))
    ),
    
    # --- MIDDLE: RESULTS ---
    card(
      full_screen = TRUE, 
      navset_card_tab(
        nav_panel(
          "Explore",
          DTOutput("results_table", height = "100%") 
        ),
        nav_panel(
          "Export",
          div(
            class = "p-3",
            h4("Download Data"),
            downloadButton("download_csv", "Download CSV", class = "btn-success"),
            br(), br(),
            verbatimTextOutput("export_summary")
          )
        )
      )
    )
    
    # # --- RIGHT: FILTERS ---
    # card(
    #   card_header("Refine Results"),
    #   div(
    #     style = "overflow-y: auto; max-height: 80vh;", 
    #     checkboxGroupInput("filter_source", "Filter by Source:",
    #                        choices = unique_sources, selected = unique_sources),
    #     hr(),
    #     checkboxGroupInput("filter_type", "Filter by Type:",
    #                        choices = unique_types, selected = unique_types)
    #   )
    # )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  # Initialize with empty frame
  search_results <- reactiveVal(data.frame())
  raw_vec = reactiveVal(NULL)
  
  observeEvent(input$run_search, {
    req(input$search_query)
    
    id <- showNotification("Searching...", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    tryCatch({
      # [UPDATED] Pass 'cutoff' instead of 'top_k'
      # Python returns a sorted DataFrame of all rows > cutoff
      
      raw_vec(semantic_search(input$search_query, data_path = "../../data")[[3]])
      # browser()
      raw_df <- dd |> 
        filter(
          label %in% raw_vec()
        )
      
      # raw_df <- semantic_search(input$search_query, cutoff = input$cutoff)
      
      if (!is.null(raw_df) && is.data.frame(raw_df)) {
        # Optional: Warn if 0 results found
        if (nrow(raw_df) == 0) {
          showNotification("No matches found. Try lowering the Similarity Threshold.", type = "warning", duration = 5)
        }
        search_results(raw_df)
      }
      
    }, error = function(e) {
      showNotification("Python Error", type = "error")
      print(e)
    })
  })
  

  
  # Output Table
  output$results_table <- renderDT({
    req(search_results())
    datatable(
      search_results(), 
      filter = "top",
      options = list(pageLength = 15, scrollX = TRUE, dom = 'tp'),
      rownames = FALSE, 
      selection = "single"
    )
  })
  
  # Export Logic
  output$export_summary <- renderText({
    req(raw_vec())
    print(raw_vec())
  })
  
  output$download_csv <- downloadHandler(
    filename = function() { paste("search_results_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(filtered_data(), file, row.names = FALSE) }
  )
}

shinyApp(ui, server)