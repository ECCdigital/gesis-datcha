# ====================== #
# modules/dataEditing.R
# ====================== #

#===== Text Changes Module =====#
dataEditingModule <- function(input, output, session, shared_data, detect_id_column) {
  # ensure www/ exists
  if (!dir.exists("www")) dir.create("www")
  # overwrite diffobj.css in www/
  file.copy(diffobj_css(), "www/diffobj.css", overwrite = TRUE)
  
  
  # Get data from shared_data - MODIFIED to use reactive expressions
  data1 <- reactive({
    req(shared_data$data1)
    shared_data$data1
  })
  
  data2 <- reactive({
    req(shared_data$data2)
    shared_data$data2
  })
  
  comparison_done <- reactive({
    req(shared_data$comparison_done)
    shared_data$comparison_done
  })
  
  #=======Levenshtein distance
  # Function to calculate Levenshtein distance between matching posts
  calculate_edit_distance <- function(df1, df2, manual1 = NULL, manual2 = NULL) {
    id1 <- detect_id_column(data1(), input$id_col_1)
    id2 <- detect_id_column(df2, manual2)
    if (is.null(id1) || is.null(id2)) return(NULL)
    matching <- df1 %>%
      inner_join(df2, by = setNames(id2, id1),
                 suffix = c("_1", "_2"))
    if (!nrow(matching)) return(NULL)
    matching %>%
      mutate(
        edit_distance = stringdist::stringdist(text_1, text_2, method = "lv"),
        normalized_distance = edit_distance / pmax(nchar(text_1), nchar(text_2))
      ) %>%
      select(!!sym(id1), text_1, text_2, edit_distance, normalized_distance)
  }

  # Calculate edit distances between matching posts
  edit_distances <- eventReactive(input$compare, {
    req(data1(), data2())
    calculate_edit_distance(data1(), data2(),
                           input$id_col_1, input$id_col_2)
  })

  # Reactive Data Editing Section
  output$data_editing_ui <- renderUI({
    req(comparison_done())
    tagList(
      htmlOutput("edit_distance_summary_ui")
    )
  })

  # Editing statistics UI
  output$editing_stats_ui <- renderUI({
    req(edit_distances())
    
    dist_data <- edit_distances()
    mean_edit <- ifelse(nrow(dist_data) > 0, mean(dist_data$edit_distance, na.rm = TRUE), 0)
    mean_norm <- ifelse(nrow(dist_data) > 0, mean(dist_data$normalized_distance, na.rm = TRUE), 0)
    edited_count <- sum(dist_data$edit_distance > 0, na.rm = TRUE)
    total_posts <- nrow(dist_data)
    ratio <- ifelse(total_posts > 0, round((edited_count/total_posts)*100, 1), 0)
    
    tagList(
      div(style = "margin-bottom: 15px;",
          strong("Mean Edit Distance"), br(),
          span(style = "font-size: 1.2em;", round(mean_edit, 3)),
          span(icon("info-circle"), id = "edit_dist_info",
               `data-toggle` = "tooltip", title = "Average Levenshtein distance between original and edited posts")
      ),
      div(style = "margin-bottom: 15px;",
          strong("Mean Normalized Distance"), br(),
          span(style = "font-size: 1.2em;", round(mean_norm, 4)),
          span(icon("info-circle"), id = "norm_dist_info",
               `data-toggle` = "tooltip", title = "Average edit distance normalized by post length (0-1 scale)")
      ),
      div(style = "margin-bottom: 15px;",
          strong("Edited Post Ratio"), br(),
          span(style = "font-size: 1.2em;", paste0(ratio, "%")),
          span(icon("info-circle"), id = "edit_ratio_info",
               `data-toggle` = "tooltip", title = "Percentage of matched posts that were edited")
      )
    )
  })


  # Update the most_edited_posts output to use the numeric input
  output$most_edited_posts <- renderDT({
    req(edit_distances(), input$num_edited_posts)
    
    df <- edit_distances()[order(-edit_distances()$edit_distance), ][1:input$num_edited_posts, ]
    
    # Apply HTML diff with inline styles for both columns
    apply_diff <- function(text_1, text_2) {
      html <- as.character(
        diffChr(text_1, text_2, format = "html", mode= 'sidebyside', 
                color.mode = 'rgb', style = list(html.output = "diff.w.style"))
      )
      # Strip diff hunk headers like: <div class='diffobj-header'>@@ 1 @@</div>
      html <- gsub("<div class='diffobj-line'><div class='diffobj-header'>@@.*?@@</div></div>", "", html)
      paste0("<div style='white-space:pre-wrap;'>", html, "</div>")
    }
    
    df$text_1 <- unname(mapply(apply_diff, df$text_1, df$text_2, SIMPLIFY = FALSE))
    df$text_2 <- unname(mapply(apply_diff, df$text_2, df$text_1, SIMPLIFY = FALSE))
    
    datatable(
      data.frame(
        ID = df[[1]],
        Differences = I(df$text_1),
        Edit_distance = df$edit_distance,
        Normalized_distance = df$normalized_distance
      ),
      escape = FALSE,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        scrollX = FALSE,
        columnDefs = list(
          list(width = '8%', targets = 0),
          list(width = '8%', targets = 1),
          list(width = '72%', targets = 2),
          list(width = '10%', targets = 3),
          list(width = '10%', targets = 4)
        )
      ),
      class = 'cell-border stripe hover compact nowrap'
    )
  })

  #Calculate Levenshtein distance 
  output$edit_distance_summary_ui <- renderUI({
    req(edit_distances())
    dist_data <- edit_distances()
    
    if (is.null(dist_data)) return(HTML("No matching posts to compare"))
    
    
    # Calculate means only if we have valid data
    mean_edit <- ifelse(nrow(dist_data) > 0, mean(dist_data$edit_distance, na.rm = TRUE), 0)
    mean_norm <- ifelse(nrow(dist_data) > 0, mean(dist_data$normalized_distance, na.rm = TRUE), 0)
    
    tagList(
      tags$span("Mean Edit Distance: ", round(mean(dist_data$edit_distance), 2),
                tags$span(icon("info-circle"), id = "edit_distance_info"),
                tags$br(),
                tags$span("Mean Normalized Distance: ", round(mean(dist_data$normalized_distance), 3),
                          tags$span(icon("info-circle"), id = "normalized_info"),
                          
                          # Initialize tooltips
                          bsTooltip("edit_distance_info", 
                                    "Levenshtein distance counts the minimum number of single-character edits needed to change one text into another",
                                    placement = "right"),
                          bsTooltip("normalized_info", 
                                    "Normalized distance divides the edit distance by the length of the longer text (range 0-1)",
                                    placement = "right")
                )))
  })

  # Calculate and display the number of edited posts
  output$edited_post_count <- renderText({
    req(edit_distances())
    edited <- sum(edit_distances()$edit_distance > 0, na.rm = TRUE)
    total <- nrow(edit_distances())
    
    paste("Number of Edited Posts:", edited, "out of", total, "matched posts")
  })

  # Display the edited post ratio as a percentage
  output$edited_post_ratio <- renderUI({
    req(edit_distances())
    total <- nrow(edit_distances())
    edited <- sum(edit_distances()$edit_distance > 0, na.rm = TRUE)
    ratio <- if (total > 0) round((edited / total) * 100, 1) else 0
    
    tagList(
      tags$span(paste("Edited Post Ratio:", ratio, "%"),
                tags$span(icon("info-circle"), id = "edited_ratio_info")),
      bsTooltip("edited_ratio_info", 
                "Percentage of matched posts that were changed between the two datasets", 
                placement = "right")
    )
  })
  
  # Return reactive values/expressions that need to be accessed by parent module
  return(list(
    edit_distances = edit_distances,
    comparison_done = reactive({ !is.null(edit_distances()) })
  ))
}

# UI function for the module
dataEditingUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Your UI elements here
    htmlOutput(ns("data_editing_ui")),
    htmlOutput(ns("editing_stats_ui")),
    DTOutput(ns("most_edited_posts"))
    # ...other UI elements
  )
}