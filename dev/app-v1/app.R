library(shiny)
library(tidyverse)
library(reticulate)
library(reactable)
library(bslib)

# --- FILE SETUP & BACKEND INIT ---
# (Preserving existing logic exactly as requested)
csv_filename <- "dd-abcd-6_0_minimal_noimag.csv" 

if (!file.exists(csv_filename)) {
  if (file.exists("dd-abcd-6_0_minimal_noimag-dummy.csv")) {
    csv_filename <- "dd-abcd-6_0_minimal_noimag-dummy.csv"
  } else {
    stop(paste("CRITICAL ERROR: Could not find", csv_filename))
  }
}

abs_path <- tools::file_path_as_absolute(csv_filename)
source_python("python/backend.py")
# initialize_backend(abs_path) 

# Load dictionary for R-side lookups and filter population
dd <- readr::read_csv(
  csv_filename,
  col_types = readr::cols(.default = readr::col_character())
)

# --- PREPARE FILTER CHOICES ---
# Extract unique sorted values for the UI
choices_source <- unique(dd$source) %>% na.omit() %>% sort()
choices_domain <- if("domain" %in% names(dd)) unique(dd$domain) %>% na.omit() %>% sort() else character(0)

# --- UI ---
ui <- page_fillable(
  theme = bs_theme(preset = "flatly"),
  
  # Custom CSS for scrollable filter boxes and card heights
  tags$head(
    tags$style(HTML("
      .card { height: 100%; } 
      .form-group { margin-bottom: 15px; }
      .no-gap { gap: 0 !important; }
      .scrollable-checkboxes {
        max_height: 200px;
        overflow-y: auto;
        padding: 5px;
        border: 1px solid #e9ecef;
        border-radius: 4px;
        background-color: #f8f9fa;
      }
      .filter-actions { font-size: 0.8rem; margin-bottom: 5px; }
    "))
  ),
  
  title = "ABCD Semantic Search",
  
  # Header
  div(
    class = "bg-primary text-white p-3 rounded-2 mb-2",
    h2("ABCD Data Dictionary Semantic Search", class = "m-0")
  ),
  
  layout_sidebar(
    
    # --- LEFT SIDEBAR (Search Inputs) ---
    sidebar = sidebar(
      width = 320,
      card_header("Search Parameters"),
      
      textAreaInput("search_query", "Describe what you are looking for:", 
                    placeholder = "e.g., bullying at school, sleep disorders...",
                    height = "150px"),
      
      sliderInput("cutoff", "Similarity Threshold:", 
                  min = 0.2, max = 1.0, value = 0.25, step = 0.05),
      
      helpText("Higher values = stricter matching."),
      
      # Search Button (Will update visually on click)
      actionButton("run_search", "Search Variables", 
                   class = "btn-primary w-100 mb-3", icon = icon("magnifying-glass")),
      
      # [NEW] Explanatory Text
      div(
        class = "small text-muted border-top pt-3",
        tags$h6("Capabilities:", class = "fw-bold"),
        tags$ul(
          class = "ps-3",
          tags$li("Finds variables by meaning (e.g., 'sadness' finds 'depression')."),
          tags$li("Filters results by similarity score.")
        ),
        tags$h6("Limitations:", class = "fw-bold"),
        tags$ul(
          class = "ps-3",
          tags$li("Not a Q&A chatbot; returns raw dictionary entries."),
          tags$li("Results depend on the quality of variable descriptions.")
        )
      )
    ),
    
    # --- MAIN CONTENT ---
    navset_card_tab(
      nav_panel(
        "Explore",
        card(
          full_screen = TRUE,
          layout_sidebar(
            class = "no-gap",
            
            # --- RIGHT SIDEBAR (Filters & Actions) ---
            sidebar = sidebar(
              position = "right",
              open = "open",  # [NEW] Open by default
              width = 350,
              card_header("Refine Results"),
              
              # 1. Delete Button
              div(
                class = "mb-4 border-bottom pb-3",
                h6("Actions", class = "fw-bold text-uppercase text-secondary small"),
                actionButton(
                  "delete_selected_rows",
                  "Delete Selected Rows",
                  class = "btn-outline-danger w-100",
                  icon = icon("trash")
                )
              ),
              
              # 2. Filters (Accordion style)
              h6("Filters", class = "fw-bold text-uppercase text-secondary small"),
              accordion(
                open = c("Source", "Domain"), # Open panels by default
                
                # SOURCE FILTER
                accordion_panel(
                  "Source",
                  div(class = "filter-actions",
                      actionLink("all_source", "Select All"), " | ",
                      actionLink("none_source", "Deselect All")
                  ),
                  div(
                    class = "scrollable-checkboxes",
                    checkboxGroupInput("filter_source", label = NULL, 
                                       choices = choices_source, 
                                       selected = choices_source)
                  )
                ),
                
                # DOMAIN FILTER
                accordion_panel(
                  "Domain",
                  div(class = "filter-actions",
                      actionLink("all_domain", "Select All"), " | ",
                      actionLink("none_domain", "Deselect All")
                  ),
                  div(
                    class = "scrollable-checkboxes",
                    checkboxGroupInput("filter_domain", label = NULL, 
                                       choices = choices_domain, 
                                       selected = choices_domain)
                  )
                )
              )
            ),
            
            # --- TABLE DISPLAY ---
            div(
              reactableOutput("results_table", width = "100%", height = "100%"),
              div(class = "text-muted small p-2", textOutput("table_counts"))
            ),
            fill = TRUE
          )
        )
      ),
      nav_panel(
        "Export",
        div(
          class = "p-3",
          h4("Download Data"),
          p("Download the filtered dataset currently shown in the Explore tab."),
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
  
  # Store the "Master" search result (before manual filtering)
  # Initialize empty, using dd structure
  master_results <- reactiveVal(dd[0, ])
  
  # --- 1. SEARCH EVENT ---
  observeEvent(input$run_search, {
    req(input$search_query)
    
    # [VISUAL FEEDBACK]
    updateActionButton(session, "run_search", label = "Searching...", icon = icon("spinner", class = "fa-spin"))
    on.exit({
      updateActionButton(session, "run_search", label = "Search Variables", icon = icon("magnifying-glass"))
    })
    
    tryCatch({
      # Call Python Backend (No changes to this logic)
      res <- semantic_search(
        isolate(input$search_query), 
        data_path = "../../data", # Assuming this path is correct for your environment
        cutoff = isolate(input$cutoff)
      )
      
      # Python returns tuple: (similarities, indices, sentences)
      # Reconstruct DataFrame using R-side 'dd' object
      indices <- res[[2]]
      similarities <- res[[1]]
      
      if (length(indices) > 0) {
        # Extract rows using 1-based indexing
        raw_df <- dd[indices + 1, ] %>% 
          mutate(similarity = round(similarities, 3)) %>% 
          relocate(similarity, name, label)
        
        master_results(raw_df)
        
        showNotification(paste("Found", nrow(raw_df), "variables."), type = "message")
      } else {
        master_results(dd[0, ])
        showNotification("No matches found. Try lowering the Similarity Threshold.", type = "warning")
      }
      
    }, error = function(e) {
      showNotification("Python Error", type = "error")
      print(e)
    })
  })
  
  # --- 2. FILTERING LOGIC ---
  # Applies manual filters (Right Sidebar) to the Master Search Results
  filtered_data <- reactive({
    data <- master_results()
    
    if (nrow(data) == 0) return(data)
    
    # Apply Source Filter
    if (!is.null(input$filter_source)) {
      data <- data %>% filter(source %in% input$filter_source)
    } else {
      # If nothing selected, show nothing
      return(data[0,])
    }
    
    # Apply Domain Filter (if column exists)
    if ("domain" %in% names(data) && !is.null(input$filter_domain)) {
      data <- data %>% filter(domain %in% input$filter_domain)
    } else if ("domain" %in% names(data)) {
      return(data[0,])
    }
    
    data
  })
  
  # --- 3. HELPER EVENTS (Select All/None) ---
  observeEvent(input$all_source, updateCheckboxGroupInput(session, "filter_source", selected = choices_source))
  observeEvent(input$none_source, updateCheckboxGroupInput(session, "filter_source", selected = character(0)))
  
  observeEvent(input$all_domain, updateCheckboxGroupInput(session, "filter_domain", selected = choices_domain))
  observeEvent(input$none_domain, updateCheckboxGroupInput(session, "filter_domain", selected = character(0)))
  
  # --- 4. TABLE RENDER ---
  output$results_table <- reactable::renderReactable({
    # Use filtered data
    data <- filtered_data()
    
    reactable::reactable(
      data,
      columns = list(
        label = reactable::colDef(minWidth = 450, name = "Description"),
        name = reactable::colDef(minWidth = 200, name = "Variable Name"),
        similarity = reactable::colDef(minWidth = 100, name = "Score"),
        source = reactable::colDef(minWidth = 150),
        domain = reactable::colDef(minWidth = 150)
      ),
      selection = "multiple",
      searchable = TRUE,
      filterable = TRUE,
      pagination = TRUE,
      highlight = TRUE,
      bordered = TRUE,
      striped = TRUE,
      height = "75vh",
      theme = reactableTheme(
        rowSelectedStyle = list(backgroundColor = "#e6f3ff", boxShadow = "inset 2px 0 0 0 #007bc2")
      )
    )
  })
  
  # --- 5. DELETE ROW LOGIC ---
  observeEvent(input$delete_selected_rows, {
    selected_indices <- reactable::getReactableState("results_table", "selected")
    
    if (is.null(selected_indices) || length(selected_indices) == 0) {
      showNotification("No rows selected.", type = "warning")
      return()
    }
    
    # Identify the specific variable names to remove from the view
    current_view <- filtered_data()
    vars_to_remove <- current_view$name[selected_indices]
    
    # Update the MASTER list (so they stay deleted even if filters change)
    current_master <- master_results()
    new_master <- current_master %>% filter(!name %in% vars_to_remove)
    
    master_results(new_master)
    showNotification("Selected rows deleted.", type = "message")
  })
  
  # --- 6. OUTPUTS ---
  output$table_counts <- renderText({
    paste("Showing", nrow(filtered_data()), "variables")
  })
  
  # Debug/Summary for Export tab
  output$export_summary <- renderText({
    paste("Ready to export", nrow(filtered_data()), "rows.")
  })
  
  output$download_csv <- downloadHandler(
    filename = function() { paste("search_results_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(filtered_data(), file, row.names = FALSE) }
  )
}

shinyApp(ui, server)