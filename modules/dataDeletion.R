# ====================== #
# modules/dataDeletion.R
# ====================== #

dataDeletionModule <- function(input, output, session, shared_data, detect_id_column) {
  # Create a reactive value to track if comparison was done
  
  comparison_done <- reactiveVal(FALSE)

  # Observe when compare button is pressed
  observeEvent(input$compare, {
    comparison_done(TRUE)
    shared_data$comparison_done <- TRUE
  })

  # ===== 4.1 Data Loading and Preparation

  # Load the first dataset
  data1 <- reactive({
    req(input$file1)  # Ensure the file is uploaded
    read.csv(input$file1$datapath, stringsAsFactors = FALSE)
  })

  # Load the second dataset
  data2 <- reactive({
    req(input$file2)  # Ensure the file is uploaded
    read.csv(input$file2$datapath, stringsAsFactors = FALSE)
  })

  output$deletion_quality_indicators <- renderUI({
    req(comparison_done(), removed_posts(), data1(), input$date1, input$date2)

    days_diff <- as.numeric(difftime(input$date2, input$date1, units = "days"))
    total_posts <- nrow(data1())
    removed_count <- nrow(removed_posts())
    remaining_count <- nrow(remaining_posts())
    
    completeness <- round((remaining_count / total_posts * 100), 1)
    data_loss <- round((removed_count / total_posts * 100), 1)
    daily_removed <- ifelse(days_diff > 0, round(removed_count/days_diff, 1), "N/A")
    daily_removal_percent <- ifelse(days_diff > 0, round((removed_count / days_diff / total_posts * 100), 2), "N/A")

    tagList(
      div(style = "margin-bottom: 15px;",
          strong("Consistency"), br(),
          span(style = "color: #28a745; font-size: 1.2em;", paste0(completeness, "%"))
      ),
      div(style = "margin-bottom: 15px;",
          strong("Data Loss"), br(),
          span(style = "color: #dc3545; font-size: 1.2em;", paste0(data_loss, "%"))
      ),
      div(style = "margin-bottom: 15px;",
          strong("Daily Removal"), br(),
          span(style = "font-size: 1.2em;", paste(daily_removed, "posts/day"))
      ),
      div(style = "margin-bottom: 15px;",
          strong("Removal Rate"), br(),
          span(style = "font-size: 1.2em;", paste(daily_removal_percent, "%/day"))
      )
    )
  })

  # Date validation message
  output$date_validation_msg <- renderUI({
    if (is.null(input$date1) || is.null(input$date2)) {
      return(div(style = "color: red;", "Please enter dates for both datasets"))
    }
    if (input$date2 <= input$date1) {
      return(div(style = "color: red;", "Error: Dataset 2 date must be after Dataset 1"))
    }
    return(NULL)
  })

  output$removed_per_day <- renderText({
    req(comparison_done(), removed_posts(), input$date1, input$date2)
    days <- as.numeric(difftime(input$date2, input$date1, units = "days"))
    if(days <= 0) return("Daily removal rate: N/A")
    paste("Daily Removed Posts:", round(nrow(removed_posts())/days, 1), "posts/day")
  })

  output$daily_removal_count <- renderText({
    req(removed_posts(), input$date1, input$date2)
    days <- as.numeric(difftime(input$date2, input$date1, units = "days"))
    if(days <= 0) return("Total days: N/A")
    paste("Time period:", days, "days")
  })

  # Daily removal rate calculation - with percentage and total days
  output$removal_rate_daily <- renderText({
    req(comparison_done(), removed_posts(), input$date1, input$date2, data1())
    days_diff <- as.numeric(difftime(input$date2, input$date1, units = "days"))
    if(days_diff <= 0) return("Invalid date range")
    total_posts <- nrow(data1())
    daily_removal_percent <- round((nrow(removed_posts()) / days_diff / total_posts * 100), 2)
    paste("Daily Removal Rate:", daily_removal_percent, "% of total posts/day (over", days_diff, "days)")
  })

  # # Enable/disable compare button based on date validity
  # observe({
  #   if (!is.null(input$date1) && !is.null(input$date2) && input$date2 > input$date1) {
  #     shinyjs::enable("compare")
  #   } else {
  #     shinyjs::disable("compare")
  #   }
  # })

  # Calculate the number of posts in Dataset 1
  output$dataset1_count <- renderText({
    req(comparison_done(), data1())
    paste("Number of posts in Dataset 1:", nrow(data1()))
  })

  # Calculate the number of posts in Dataset 2
  output$dataset2_count <- renderText({
    req(comparison_done(), data2())
    paste("Number of posts in Dataset 2:", nrow(data2()))
  })

  # Identify removed posts (present in the first dataset but missing in the second)
  removed_posts <- eventReactive(input$compare, {
    req(data1(), data2())
    df1 <- data1(); df2 <- data2()
    id1 <- detect_id_column(df1, input$id_col_1)
    id2 <- detect_id_column(df2, input$id_col_2)
    if (is.null(id1) || is.null(id2)) {
      showNotification("No valid ID column found in one or both datasets.", type = "error")
      return(NULL)
    }
    df1 %>% filter(!( !!sym(id1) %in% df2[[id2]] ))
  })

  # Get remaining posts
  remaining_posts <- eventReactive(input$compare, {
    req(data1(), data2())
    df1 <- data1(); df2 <- data2()
    id1 <- detect_id_column(df1, input$id_col_1)
    id2 <- detect_id_column(df2, input$id_col_2)
    if (is.null(id1) || is.null(id2)) return(NULL)
    df1 %>% filter( !!sym(id1) %in% df2[[id2]] )
  })

  # Display the number of removed posts
  output$removed_count <- renderText({
    req(comparison_done(), removed_posts())
    paste("Number of Deleted Posts:", nrow(removed_posts()))
  })

  # Calculate completeness
  output$completeness <- renderText({
    req(comparison_done(), data1(), data2())
    completeness <- (nrow(data2()) / nrow(data1())) * 100
    paste("Completeness:", round(completeness, 1), "%")
  })

  # Calculate data loss
  output$data_loss <- renderText({
    req(comparison_done(), removed_posts())
    data_loss <- (nrow(removed_posts()) / nrow(data1())) * 100
    paste("Data Loss:", round(data_loss, 1), "%")
  })

  # ===== Generate Word Frequency Plots ===== #
  output$word_freq_plot_removed <- renderHighchart({
    req(comparison_done(), removed_posts())

    text_data <- removed_posts()$text  # Assumption: The text column is named "text"

    if (is.null(text_data) || length(text_data) == 0) {
      showNotification("Error: No text data found in removed posts.", type = "error")
      return(NULL)
    }

    cleaned_text <- text_processor$clean(text_data, use_stem = FALSE, use_lemma = TRUE)
    word_freq <- text_processor$get_freq(cleaned_text) %>%
      filter(freq > 1) %>%
      slice(1:100)  # Add slicing to match original behavior

    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "Removed Posts") %>%
      hc_tooltip(crosshairs = TRUE, shared = FALSE, useHTML = TRUE,
                  formatter = JS(paste0("function() {
                                      var result='';
                                      result='<br/><span style=\\'color:'+this.series.color+'\\'>'+this.point.category+'</span>:<b> '
                                      +this.point.y + '</b>';
                                      return result;
      }"))) %>%
      hc_xAxis(categories = word_freq[1:100,]$word,
                labels = list(style = list(fontSize = '11px')), max = 20, scrollbar = list(enabled = TRUE)) %>%
      hc_add_series(name = "Word", data = word_freq[1:100,]$freq, type = "column",
                    color = "#4CAF50", showInLegend = FALSE)
  })

  output$word_freq_plot_remaining <- renderHighchart({
    req(comparison_done(), data1(), data2())

    # just grab the already‐filtered remaining_posts() df
    rem <- remaining_posts()
    text_data <- rem$text

    if (is.null(text_data) || length(text_data) == 0) {
      showNotification("Error: No text data found in remaining posts.", type = "error")
      return(NULL)
    }

    # Clean text first
    cleaned_text <- text_processor$clean(text_data,  use_stem = FALSE, use_lemma = TRUE)
    word_freq <- text_processor$get_freq(cleaned_text) %>%
      filter(freq > 1) %>%
      slice(1:100)

    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "Remaining Posts") %>%
      hc_tooltip(crosshairs = TRUE, shared = FALSE, useHTML = TRUE,
                  formatter = JS(paste0("function() {
                                      var result='';
                                      result='<br/><span style=\\'color:'+this.series.color+'\\'>'+this.point.category+'</span>:<b> '
                                      +this.point.y + '</b>';
                                      return result;
      }"))) %>%
      hc_xAxis(categories = word_freq[1:100,]$word,
                labels = list(style = list(fontSize = '11px')), max = 20, scrollbar = list(enabled = TRUE)) %>%
      hc_add_series(name = "Word", data = word_freq[1:100,]$freq, type = "column",
                    color = "#2196F3", showInLegend = FALSE)
  })

  
  # ===== Topic Modeling Reactive Values =====
  current_topic <- reactiveVal(0)  # 0 means no topic selected
  
  observeEvent(input$prev_topic, {
    if(current_topic() > 1) {
      current_topic(current_topic() - 1)
    }
  })
  
  observeEvent(input$next_topic, {
    if(current_topic() < input$num_topics) {
      current_topic(current_topic() + 1)
    }
  })
  
  observeEvent(input$clear_topic, {
    current_topic(0)  # Clear selection
  })
  
  # Helper: Create LDAvis JSON – keeps only documents that survived DTM filtering
  topicmodels_json_ldavis_safe <- function(fitted, original_texts, dtm) {
    library(dplyr)
    library(stringi)
    
    # Get indices of documents that survived filtering
    valid_rows <- which(rowSums(as.matrix(dtm)) > 0)
    
    # Subset everything to match the filtered DTM
    phi    <- posterior(fitted)$terms %>% as.matrix()
    theta  <- posterior(fitted)$topics[valid_rows, , drop = FALSE]
    vocab  <- colnames(phi)
    
    cleaned_valid <- original_texts[valid_rows]
    doc_length <- vapply(cleaned_valid, function(x) stri_count(x, regex = "\\S+"), integer(1))
    
    term_freq <- colSums(as.matrix(dtm))
    
    json <- tryCatch({
      LDAvis::createJSON(
        phi = phi,
        theta = theta,
        vocab = vocab,
        doc.length = doc_length,
        term.frequency = term_freq,
        mds.method = stats::cmdscale
      )
    }, error = function(e) {
      LDAvis::createJSON(
        phi = phi,
        theta = theta,
        vocab = vocab,
        doc.length = doc_length,
        term.frequency = term_freq,
        mds.method = function(x) prcomp(x)$x[, 1:2]
      )
    })
    
    json
  }

  # ──────────────────────────────────────────────────────────────
  # In renderUI($ldavis_output) – replace the whole modeling part:
  # ──────────────────────────────────────────────────────────────
  
  output$ldavis_output <- renderUI({
    req(comparison_done(), input$num_topics)
    
    dataset_list <- list(
      "Removed Posts"   = removed_posts(),
      "Remaining Posts" = remaining_posts(),
      "Combined View"   = bind_rows(
        removed_posts()   %>% mutate(group = "removed"),
        remaining_posts() %>% mutate(group = "remaining")
      )
    )
    
    dataset <- dataset_list[[input$topic_dataset]]
    
    if (nrow(dataset) < 10 || all(dataset$text %in% c("", " ", NA))) {
      return(div(class = "alert alert-warning",
                 "Not enough meaningful documents for topic modeling (min ~10 non-empty required)"))
    }
    
    withProgress(message = 'Checking dataset size...', value = 0.1, {
      
      MAX_DOCS_FOR_TOPIC_MODELING <- 15000
      
      if (nrow(dataset) > MAX_DOCS_FOR_TOPIC_MODELING) {
        view_name <- switch(input$topic_dataset,
                            "Removed Posts"   = "Removed Posts view",
                            "Remaining Posts" = "Remaining Posts view",
                            "Combined View"   = "Combined View (Removed + Remaining)",
                            "Added Posts"     = "Added Posts view",           # for addition module
                            "Original Posts"  = "Original Posts view",       # for addition module
                            input$topic_dataset)  # fallback
        
        return(
          div(class = "alert alert-warning", style = "margin: 20px;",
              icon("exclamation-triangle"),
              tags$strong("Topic modeling not available for this view"),
              tags$p(
                "The ", strong(view_name), " contains ", nrow(dataset), " posts.",
                tags$br(),
                "Topic modeling is limited to ", MAX_DOCS_FOR_TOPIC_MODELING, " documents to avoid long waits and to ensure smooth performance.",
                tags$br(), tags$br(),
                # "Try switching to a ",
                # # if (input$topic_dataset != "Removed Posts")   tags$strong("Removed Posts"),
                # # if (input$topic_dataset != "Remaining Posts") tags$strong("Remaining Posts"),
                # # if (input$topic_dataset != "Added Posts")     tags$strong("Added Posts"),
                # # if (input$topic_dataset != "Original Posts")  tags$strong("Original Posts"),
                # " view with fewer posts to see topic modeling results.",
                # tags$br(), tags$br(),
                
              )
          )
        )
      }
      
      # ── Only continue if size is ok ──
      withProgress(message = 'Generating topics...', value = 0.4, {
        
        cleaned <- text_processor$clean(dataset$text, use_stem = FALSE, use_lemma = TRUE)
        
        valid_idx <- which(nzchar(trimws(cleaned)))
        if (length(valid_idx) < 10) {
          return(div(class = "alert alert-danger",
                     "After cleaning, too few documents contain any words → topic modeling impossible"))
        }
        
        cleaned_valid <- cleaned[valid_idx]
        
        corpus <- Corpus(VectorSource(cleaned_valid))
        dtm <- DocumentTermMatrix(corpus)
        dtm <- dtm[rowSums(as.matrix(dtm)) > 0, ]
        
        if (nrow(dtm) < 8 || ncol(dtm) < 5) {
          return(div(class = "alert alert-danger",
                     "After cleaning & filtering: insufficient terms/documents for LDA"))
        }
        
        lda_model <- tryCatch(
          LDA(dtm, k = input$num_topics, control = list(seed = 1234)),
          error = function(e) NULL
        )
        
        if (is.null(lda_model)) {
          return(div(class = "alert alert-danger", "LDA failed to converge"))
        }
        
        json <- topicmodels_json_ldavis_safe(lda_model, cleaned_valid, dtm)
        
        div(
          style = "width: 100%; height: 80vh; min-height: 650px; max-height: 90vh; 
                 border: 1px solid #ddd; border-radius: 8px; overflow: auto; 
                 background: white; margin-bottom: 25px;",
          
          div(
            id = "ldavis-wrapper-deletion",
            style = "min-width: 1000px; min-height: 800px; padding: 10px; box-sizing: border-box;",
            LDAvis::renderVis(json)
          )
        )
        
      })  # closes inner withProgress
      
    })    # closes outer withProgress
    
  })      # closes renderUI    # ← closes outer withProgress
  

# ===== Sentiment Analysis ===== #
get_sentiment_distribution <- function(text_vector) {
  if (is.null(text_vector)) {
    return(data.frame(
      category = c("Negative", "Neutral", "Positive"),
      percentage = c(0, 0, 0)
    ))
  }
  
  # Process in chunks for large datasets
  chunk_size <- 500
  chunks <- split(text_vector, ceiling(seq_along(text_vector)/chunk_size))
  
  results <- lapply(chunks, function(chunk) {
    sentences <- get_sentences(chunk)
    sentiment(sentences)
  })
  
  all_scores <- unlist(lapply(results, function(x) x$sentiment))
  
  category <- cut(all_scores, 
                  breaks = c(-Inf, -0.01, 0.01, Inf),
                  labels = c("Negative", "Neutral", "Positive"))
  
  counts <- table(factor(category, levels = c("Negative", "Neutral", "Positive")))
  percentages <- prop.table(counts) * 100
  
  data.frame(
    category = names(percentages),
    percentage = as.numeric(percentages)
  )
}

# Optimized version of sentiment_by for extreme posts
get_extreme_posts <- function(df, n = 1, type = "positive") {
  if (nrow(df) == 0) return("No data")
  
  # Process in chunks
  chunk_size <- 500
  chunks <- split(df, ceiling(seq_len(nrow(df))/chunk_size))
  
  all_scores <- lapply(chunks, function(chunk) {
    sentences <- get_sentences(chunk$text)
    scores <- sentiment_by(sentences)
    data.frame(text = chunk$text, score = scores$ave_sentiment)
  }) %>% bind_rows()
  
  if (type == "positive") {
    all_scores %>% 
      arrange(desc(score)) %>% 
      slice_head(n = n) %>% 
      pull(text) %>% 
      as.character()
  } else {
    all_scores %>% 
      arrange(score) %>% 
      slice_head(n = n) %>% 
      pull(text) %>% 
      as.character()
  }
}

# Sentiment distribution plots 
output$sentiment_plot_removed <- renderHighchart({
  req(comparison_done(), removed_posts())
  
  withProgress(message = 'Analyzing sentiment...', value = 0.5, {
    sentiment_data <- get_sentiment_distribution(removed_posts()$text)
  })
  
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(categories = c("Negative", "Neutral", "Positive")) %>%
    hc_yAxis(title = list(text = "Percentage"), labels = list(format = "{value}%")) %>%
    hc_add_series(name = "Removed Posts", 
                  data = sentiment_data$percentage, 
                  color = "#4CAF50") %>%
    hc_tooltip(pointFormat = "<b>{point.category}</b>: {point.y:.1f}%") %>%
    hc_plotOptions(series = list(pointPadding = 0.1, groupPadding = 0.1))
})

output$sentiment_plot_remaining <- renderHighchart({
  req(comparison_done(), remaining_posts())
  
  withProgress(message = 'Analyzing sentiment...', value = 0.5, {
    sentiment_data <- get_sentiment_distribution(remaining_posts()$text)
  })
  
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(categories = c("Negative", "Neutral", "Positive")) %>%
    hc_yAxis(title = list(text = "Percentage"), labels = list(format = "{value}%")) %>%
    hc_add_series(name = "Remaining Posts", 
                  data = sentiment_data$percentage, 
                  color = "#2196F3") %>%
    hc_tooltip(pointFormat = "<b>{point.category}</b>: {point.y:.1f}%") %>%
    hc_plotOptions(series = list(pointPadding = 0.1, groupPadding = 0.1))
})

# ----- Most Extreme Posts - Dynamic Boxes ----- #
# Reactive expressions for text
most_positive_removed_text <- reactive({
  req(comparison_done(), removed_posts())
  withProgress(message = 'Finding most positive...', value = 0.5, {
    get_extreme_posts(removed_posts(), type = "positive")
  })
})

most_negative_removed_text <- reactive({
  req(comparison_done(), removed_posts())
  withProgress(message = 'Finding most negative...', value = 0.5, {
    get_extreme_posts(removed_posts(), type = "negative")
  })
})

most_positive_remaining_text <- reactive({
  req(comparison_done(), remaining_posts())
  withProgress(message = 'Finding most positive...', value = 0.5, {
    get_extreme_posts(remaining_posts(), type = "positive")
  })
})

most_negative_remaining_text <- reactive({
  req(comparison_done(), remaining_posts())
  withProgress(message = 'Finding most negative...', value = 0.5, {
    get_extreme_posts(remaining_posts(), type = "negative")
  })
})

# Dynamic UI boxes
output$most_positive_removed_box <- renderUI({
  text <- most_positive_removed_text()
  if(is.null(text) || text == "No data") return(div("No data"))
  
  div(style = paste0("background: #f0f8ff; padding: 10px; border-radius: 5px;",
                      "min-height: 50px; max-height: 300px;",
                      "overflow-y: auto; white-space: pre-wrap;"),
      text)
})

output$most_negative_removed_box <- renderUI({
  text <- most_negative_removed_text()
  if(is.null(text) || text == "No data") return(div("No data"))
  
  div(style = paste0("background: #fff0f0; padding: 10px; border-radius: 5px;",
                      "min-height: 50px; max-height: 300px;",
                      "overflow-y: auto; white-space: pre-wrap;"),
      text)
})

output$most_positive_remaining_box <- renderUI({
  text <- most_positive_remaining_text()
  if(is.null(text) || text == "No data") return(div("No data"))
  
  div(style = paste0("background: #f0f8ff; padding: 10px; border-radius: 5px;",
                      "min-height: 50px; max-height: 300px;",
                      "overflow-y: auto; white-space: pre-wrap;"),
      text)
})

output$most_negative_remaining_box <- renderUI({
  text <- most_negative_remaining_text()
  if(is.null(text) || text == "No data") return(div("No data"))
  
  div(style = paste0("background: #fff0f0; padding: 10px; border-radius: 5px;",
                      "min-height: 50px; max-height: 300px;",
                      "overflow-y: auto; white-space: pre-wrap;"),
      text)
})

# ===== Keyness Analysis Module ===== #
keyness_analyzer <- list(
  prepare_data = function(removed_posts, remaining_posts) {
    combined_df <- data.frame(
      text = c(text_processor$clean(removed_posts$text), 
                text_processor$clean(remaining_posts$text)),
      group = c(rep("removed", nrow(removed_posts)), 
                rep("remaining", nrow(remaining_posts)))
    )
    
    frequency_table_creator(
      df = combined_df,
      text_field = "text",
      grouping_variable = "group",
      grouping_variable_target = "removed",
      remove_punct = TRUE,
      remove_symbols = TRUE,
      remove_numbers = TRUE,
      lemmatize = TRUE
    )
  },
  
  calculate_keyness = function(frequency_table) {
    keyness_measure_calculator(
      frequency_table,
      log_likelihood = TRUE,
      ell = TRUE,
      bic = TRUE,
      perc_diff = TRUE,
      relative_risk = TRUE,
      log_ratio = TRUE,
      odds_ratio = TRUE,
      sort = "decreasing",
      sort_by = "ell"  # Changed to sort by effect size
    )
  }
)

# Reactive keyness analysis
keyness_results <- reactive({
  req(removed_posts(), remaining_posts())
  
  withProgress(message = 'Analyzing key terms...', value = 0.5, {
    freq_table <- keyness_analyzer$prepare_data(removed_posts(), remaining_posts())
    measures <- keyness_analyzer$calculate_keyness(freq_table)
    
    # Helper function for filtering
    filter_terms <- function(use_type, n = 5) {
      measures %>%
        filter(word_use == use_type, 
                log_likelihood > 3.84) %>%  # Remove ell > 0 condition
        arrange(desc(log_likelihood)) %>%  # Sort by log_likelihood first
        slice(1:n)
    }
    
    list(
      overuse = filter_terms("overuse"),
      underuse = filter_terms("underuse"),
      all = measures %>% 
        filter(log_likelihood > 3.84) %>%
        arrange(desc(ell))
    )
  })
})

# UI controls remain the same
output$keyness_controls <- renderUI({
  req(keyness_results())
  
  tabsetPanel(
    id = "keyness_tabs",
    tabPanel("Removed Posts", value = "removed"),
    tabPanel("Remaining Posts", value = "remaining"),
    tabPanel("Combined View", value = "combined")
  )
})


# Update the plot to show ELL values
output$keyness_plot <- renderHighchart({
  req(keyness_results(), input$keyness_tabs)
  
  # Create keyness_data based on tab selection
  if(input$keyness_tabs == "removed") {
    keyness_data <- keyness_results()$overuse %>%
      mutate(
        color = "#4CAF50",
        y = ell  # Use ELL instead of log_likelihood
      )
    title_text <- "Terms Distinctive of Removed Posts (by Effect Size)"
    
  } else if(input$keyness_tabs == "remaining") {
    keyness_data <- keyness_results()$underuse %>%
      mutate(
        color = "#2196F3",
        y = ell
      )
    title_text <- "Terms Distinctive of Remaining Posts (by Effect Size)"
    
  } else {
    overuse_top <- keyness_results()$overuse %>% 
      slice(1:5) %>%
      mutate(
        color = "#4CAF50",
        y = ell
      )
    
    underuse_top <- keyness_results()$underuse %>% 
      slice(1:5) %>%
      mutate(
        color = "#2196F3",
        y = ell   # Negative ELL for visualization
      )
    
    keyness_data <- bind_rows(overuse_top, underuse_top) %>%
      arrange(desc(abs(y)))
    
    title_text <- "Keyness Analysis: Effect Size Comparison"
  }
  
  # Create the highchart object
  highchart() %>%
    hc_chart(
      type = "bar",
      height = 500,
      marginLeft = 100,
      marginBottom = 100
    ) %>%
    hc_title(
      text = title_text
    ) %>%
    hc_subtitle(
      text = paste0("Comparing ", nrow(removed_posts()), " removed posts to ", 
                    nrow(remaining_posts()), " remaining posts")
    ) %>%
    hc_xAxis(
      categories = keyness_data$word,
      labels = list(
        style = list(fontSize = "11px"),
        rotation = 0
      )
    ) %>%
    hc_yAxis(
      title = list(text = "Effect Size (ELL) [0-1]"),
      labels = list(format = "{value:.6f}"),  # Show more decimal places
      plotLines = if(input$keyness_tabs == "combined") 
        list(list(value = 0, color = "#666", width = 1, zIndex = 5)) else NULL
    ) %>%
    hc_tooltip(
      formatter = JS("function() {
  var corpus = this.point.y > 0 ? 'Removed' : 'Remaining';
  var ell = (this.point.y).toFixed(6);  // Just format directly, no scaling needed
  var ll = this.point.log_likelihood.toFixed(2);
  var ratio = this.point.log_ratio ? this.point.log_ratio.toFixed(2) : 'N/A';
  return '<b>' + this.point.category + '</b><br>' +
          'More frequent in: <b>' + corpus + '</b><br>' +
          'Effect Size (ELL): ' + ell + '<br>' +
          'Log-likelihood: ' + ll + '<br>' +
          'Log Ratio: ' + ratio;
}")
    ) %>%
    hc_plotOptions(
      series = list(
        colorByPoint = TRUE,
        minPointLength = 3
      ),
      bar = list(
        groupPadding = 0.1,
        pointPadding = 0.1
      )
    ) %>%
    hc_add_series(
      data = lapply(1:nrow(keyness_data), function(i) {
        list(
          y = keyness_data$y[i],
          color = keyness_data$color[i],
          log_likelihood = keyness_data$log_likelihood[i],
          log_ratio = keyness_data$log_ratio[i]
        )
      }),
      showInLegend = FALSE
    )
})

# Updated interpretation with ELL
output$keyness_interpretation <- renderUI({
  req(keyness_results(), input$keyness_tabs)
  
  if(input$keyness_tabs == "removed") {
    top_terms <- keyness_results()$overuse %>%
      slice(1:5) %>%
      mutate(info = paste0(word, " (LL: ", round(log_likelihood, 1), ", ELL: ", sprintf("%.6f", ell), ")"))
    
    HTML(paste0(
      "<div style='margin-top: 20px; background: #f8f9fa; padding: 15px; border-radius: 5px;'>",
      "<h5>Understanding Key Terms in Removed Posts</h5>",
      "<div style='color: #AA0114;'>",
      paste("- ", top_terms$info, collapse = "<br>"),
      "</div>",
      "<p style='margin-top: 10px; font-size: 0.9em; color: #666;'>",
      "These words appear much more often in removed posts than in the remaining ones.<br>",
      "<strong>Example:</strong> If the word 'protest' appears a lot in removed posts but not in remaining ones, it will show up here.<br>",
      "<strong>LL (Log-likelihood)</strong> tells us how statistically significant the difference is (a value above 3.84 means it's important).<br>",
      "<strong>ELL (Effect Size)</strong> shows how strong that difference is (closer to 1 = bigger difference).",
      "</p>",
      "</div>"
    ))
    
    
  } else if(input$keyness_tabs == "remaining") {
    top_terms <- keyness_results()$underuse %>%
      slice(1:5) %>%
      mutate(info = paste0(word, " (LL: ", round(log_likelihood, 1), ", ELL: ", sprintf("%.6f", ell), ")"))
    
    HTML(paste0(
      "<div style='margin-top: 20px; background: #f8f9fa; padding: 15px; border-radius: 5px;'>",
      "<h5>Understanding Key Terms in Remaining Posts</h5>",
      "<div style='color: #4472C4;'>",
      paste("- ", top_terms$info, collapse = "<br>"),
      "</div>",
      "<p style='margin-top: 10px; font-size: 0.9em; color: #666;'>",
      "These words show up more often in remaining posts compared to removed ones.<br>",
      "<strong>Example:</strong> If the word 'update' is common in remaining posts but not in removed ones, it will appear here.<br>",
      "LL (Log-likelihood) and ELL (Effect Size) explain how important and how strong the difference is.",
      "</p>",
      "</div>"
    ))
    
  } else {
    top_removed <- keyness_results()$overuse %>%
      slice(1:5) %>%
      mutate(info = paste0(word, " (LL: ", round(log_likelihood, 1), ", ELL: ", sprintf("%.6f", ell), ")"))
    
    top_remaining <- keyness_results()$underuse %>%
      slice(1:5) %>%
      mutate(info = paste0(word, " (LL: ", round(log_likelihood, 1), ", ELL: ", sprintf("%.6f", ell), ")"))
    
    HTML(paste0(
      "<div style='margin-top: 20px; background: #f8f9fa; padding: 15px; border-radius: 5px;'>",
      "<h5>Quick Guide: Comparing Key Terms</h5>",
      "<div style='columns: 2;'>",
      "<div style='color: #AA0114;'>",
      "<strong>Common in Removed Posts:</strong><br>",
      paste("- ", top_removed$info, collapse = "<br>"),
      "</div>",
      "<div style='color: #4472C4; margin-left: 30px;'>",
      "<strong>Common in Remaining Posts:</strong><br>",
      paste("- ", top_remaining$info, collapse = "<br>"),
      "</div>",
      "</div>",
      "<p style='margin-top: 10px; font-size: 0.9em; color: #666;'>",
      "This helps you understand which words are more typical in each group.<br>",
      "LL tells us if it's a meaningful difference (above 3.84 = likely real).<br>",
      "ELL shows how big the difference is (0 to 1 scale, closer to 1 = bigger).<br>",
      "<strong>Example:</strong> 'banned' might appear more in removed posts, while 'sale' might appear more in remaining posts.",
      "</p>",
      "</div>"
    ))
  }
})

return(list(
  comparison_done = comparison_done,
  removed_posts = removed_posts,
  remaining_posts = remaining_posts,
  data1 = data1,  
  data2 = data2   
))
}