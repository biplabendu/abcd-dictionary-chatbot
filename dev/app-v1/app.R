library(shiny)
library(tidyverse)
library(reticulate)
library(reactable)
library(bslib)

# use_python("/usr/bin/python3.10", required = TRUE)


# options(shiny.autoreload = TRUE)
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
# use_python(".venv/bin/python")
source_python("python/backend.py") # Load the script

# res <- semantic_search("give variables of BMI", data_path = "../../data/")       # Run the init function
# res[[3]] |> length()
# # --- UI HELPERS ---
# unique_sources <- get_unique_values("source")
# unique_types   <- get_unique_values("type_var")


# load dictionary ---------------------------------------------------------

dd <- readr::read_csv(
  csv_filename,
  col_types = readr::cols(.default = readr::col_character())
)

# --- UI ---
ui <- page_fillable(
  theme = bs_theme(preset = "flatly"),
  
  tags$head(
    tags$style(HTML("
      .card { height: 100%; } 
      .form-group { margin-bottom: 15px; }
      .no-gap { gap: 0 !important; }
    "))
  ),
  
  title = "ABCD Semantic Search",
  
  div(
    class = "bg-primary text-white p-3 rounded-2",
    h2("ABCD Data Dictionary Semantic Search", class = "m-0")
  ),
  
  layout_sidebar(
    
    sidebar = sidebar(
      width = 300,
      card_header("Search Parameters"),
      
      textAreaInput("search_query", "Describe what you are looking for:", 
                    placeholder = "e.g., bullying at school",
                    height = "150px"),
      
      # # [UPDATED] Similarity Cutoff Slider
      sliderInput("cutoff", "Similarity Threshold:", 
                  min = 0.2, max = 1.0, value = 0.25, step = 0.05),
      
      # helpText("Higher values = stricter matching. Lower values = more results."),
      
      actionButton("run_search", "Search", 
                   class = "btn-primary w-100", icon = icon("magnifying-glass"))
    ),
      navset_card_tab(
        nav_panel(
          "Explore",
          card(
            full_screen = TRUE,
            layout_sidebar(
              class = "no-gap",
              sidebar = sidebar(
                position = "right",
                open = "closed",
                card_header("Table Options"),
                helpText("Add filters or settings here."),
                actionButton(
                  "delete_selected_rows",
                  "Delete Selected Rows",
                  class = "btn-danger w-100"
                )
              ),
              div(
                reactableOutput("results_table", width = "100%"),
                div(class = "text-muted small p-2", textOutput("table_counts"))
              ),
              fill = TRUE
            )
          ),
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
      ),
    fill = TRUE
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  # Initialize with empty frame
  search_results <- reactiveVal(data.frame(dd[0, ]))
  raw_vec = reactiveVal(NULL)
  
  observeEvent(input$run_search, {
    req(input$search_query)
    
    id <- showNotification("Searching...", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    tryCatch({
      # [UPDATED] Pass 'cutoff' instead of 'top_k'
      # Python returns a sorted DataFrame of all rows > cutoff
      res <- semantic_search(isolate(input$search_query), data_path = "../../data", cutoff = isolate(input$cutoff))
      # raw_vec(semantic_search(input$search_query, data_path = "../../data")[[3]])
      
      raw_df <- dd[res[[2]] + 1, ] |> 
        mutate(
          similarity = round(res[[1]], 3)
        ) |> 
          relocate(similarity, name, label)
      
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
  output$results_table <- reactable::renderReactable({
    req(search_results())
    reactable::reactable(
      search_results(),
      # defaultColDef = reactable::colDef(minWidth = 150),
      columns = list(
        label = reactable::colDef(minWidth = 450),
        table_label = reactable::colDef(minWidth = 280),
        name = reactable::colDef(minWidth = 200)
      ),
      selection = "multiple",
      searchable = TRUE,
      filterable = TRUE,
      pagination = FALSE,
      highlight = TRUE,
      bordered = TRUE,
      striped = TRUE,
      height = "70vh"
    )
  })

  observeEvent(input$delete_selected_rows, {
    selected <- reactable::getReactableState("results_table", "selected")
    if (is.null(selected) || length(selected) == 0) {
      showNotification("No rows selected.", type = "warning")
      return()
    }

    updated <- search_results()
    updated <- updated[-selected, , drop = FALSE]
    search_results(updated)
    showNotification("Selected rows deleted.", type = "message")
  })

  output$table_counts <- renderText({
    req(search_results())
    paste("Rows:", nrow(search_results()), "| Columns:", ncol(search_results()))
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