# ====================== #
# SERVER LOGIC
# ====================== #
library(shiny)
library(shinyjs)

# 1. Load the modules
source("modules/dataDeletion.R")
source("modules/dataAddition.R")
source("modules/dataEditing.R")

# 2. Define server
server <- function(input, output, session) {
  # Call common data handler from global.R
  shared_data <- common_data_handler(input, output, session)
  
  # Call modules with shared data
  deletion_results <- dataDeletionModule(input, output, session, shared_data, detect_id_column)
  editing_results <- dataEditingModule(input, output, session, shared_data, detect_id_column)
  addition_results <- dataAdditionModule(input, output, session, shared_data, detect_id_column)
  
  # Update shared data when available
  observe({
    if (!is.null(deletion_results$data1())) {
      shared_data$data1 <- deletion_results$data1()
    }
  })
  observe({
    if (!is.null(deletion_results$data2())) {
      shared_data$data2 <- deletion_results$data2()
    }
  })
  
  observe({
    if (!is.null(editing_results$comparison_done())) {
      shared_data$comparison_done <- editing_results$comparison_done()
    }
  })
  
  observe({
    if (!is.null(editing_results$edit_distances())) {
      shared_data$edit_distances <- editing_results$edit_distances()
    }
  })
  
  observe({
    if (!is.null(deletion_results$comparison_done())) {
      shared_data$comparison_done <- deletion_results$comparison_done()
    }
  })

  # Overview tab: Number of Added Posts
  output$overview_added_count <- renderText({
    req(addition_results$comparison_done(), addition_results$added_posts())
    if (is.null(addition_results$added_posts())) {
      return("Number of Added Posts: 0 (No valid data)")
    }
    paste("Number of Added Posts:", nrow(addition_results$added_posts()))
  })
}
