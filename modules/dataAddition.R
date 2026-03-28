# ====================== #
# modules/dataAddition.R
# ====================== #

dataAdditionModule <- function(input, output, session, shared_data,detect_id_column) {
  
  # Create a reactive value to track if comparison was done
  comparison_done <- reactiveVal(FALSE)
  
  # Update shared data when comparison is done
  observeEvent(input$compare, {
    req(shared_data$file1_uploaded, shared_data$file2_uploaded)
    comparison_done(TRUE)
    shared_data$comparison_done <- TRUE
  })
  
  # ===== Data Loading and Preparation
  data1 <- reactive({
    req(shared_data$data1)
    shared_data$data1
  })
  
  data2 <- reactive({
    req(shared_data$data2)
    shared_data$data2
  })
  
  # ===== Core Analysis Functions
  # Identify added posts (present in the second dataset but missing in the first)
  added_posts <- eventReactive(input$compare, {
    req(data1(), data2())
    df1 <- data1(); df2 <- data2()
    id1 <- detect_id_column(df1, input$id_col_1)
    id2 <- detect_id_column(df2, input$id_col_2)
    if (is.null(id1) || is.null(id2)) {
      showNotification("No valid ID column found in one or both datasets.", type = "error")
      return(data.frame(text = character(0), cleaned_text = character(0), stringsAsFactors = FALSE))
    }
    if (!"text" %in% names(df2)) {
      showNotification("Error: 'text' column not found in Dataset 2.", type = "error")
      return(data.frame(text = character(0), cleaned_text = character(0), stringsAsFactors = FALSE))
    }
    df <- df2 %>% filter(!(!!sym(id2) %in% df1[[id1]]))
    
    df$cleaned_text <- if (nrow(df) > 0) {
      text_processor$clean(df$text, use_stem = FALSE, use_lemma = TRUE)
    } else {
      character(0)
    }
    df
  })
 
  
  # Data Addition Indicators
  output$addition_quality_indicators <- renderUI({
    req(comparison_done(), added_posts(), data1(), input$date1, input$date2)
    
    days_diff <- as.numeric(difftime(input$date2, input$date1, units = "days"))
    old_posts <- nrow(data1())
    new_posts <- nrow(data2())
    added_count <- nrow(added_posts())
    original_count <- nrow(original_posts())
    
    consistency <- round((original_count / new_posts * 100), 1)
    growth <- round((added_count / old_posts * 100), 1)
    daily_addition <- ifelse(days_diff > 0, round(added_count / days_diff, 1), "N/A")
    daily_addition_percent <- ifelse(days_diff > 0, round((added_count / days_diff / old_posts * 100), 2), "N/A")
    
    tagList(
      div(style = "margin-bottom: 15px;",
          strong("Consistency"), br(),
          span(style = "color: #28a745; font-size: 1.2em;", paste0(consistency, "%"))
      ),
      div(style = "margin-bottom: 15px;",
          strong("Data Addition"), br(),
          span(style = "color: #28a745; font-size: 1.2em;", paste0(growth, "%"))
      ),
      div(style = "margin-bottom: 15px;",
          strong("Daily Addition"), br(),
          span(style = "font-size: 1.2em;", paste(daily_addition, "posts/day"))
      ),
      div(style = "margin-bottom: 15px;",
          strong("Addition Rate"), br(),
          span(style = "font-size: 1.2em;", paste(daily_addition_percent, "%/day"))
      )
    )
  })
  
  # Get original posts (present in both datasets)
  original_posts <- eventReactive(input$compare, {
    req(data1(), data2())
    df1 <- data1(); df2 <- data2()
    id1 <- detect_id_column(df1, input$id_col_1)
    id2 <- detect_id_column(df2, input$id_col_2)
    if (is.null(id1) || is.null(id2)) {
      showNotification("No valid ID column found in one or both datasets.", type = "error")
      return(data.frame(text = character(0), cleaned_text = character(0), stringsAsFactors = FALSE))
    }
    if (!"text" %in% names(df2)) {
      showNotification("Error: 'text' column not found in Dataset 2.", type = "error")
      return(data.frame(text = character(0), cleaned_text = character(0), stringsAsFactors = FALSE))
    }
    df <- df2 %>% filter(!!sym(id2) %in% df1[[id1]])  # ← ADD THIS
    
    df$cleaned_text <- if (nrow(df) > 0) {
      text_processor$clean(df$text, use_stem = FALSE, use_lemma = TRUE)
    } else {
      character(0)
    }
    df
})
  
  # Calculate the number of posts in Dataset 1
  output$dataset1_count_addition <- renderText({
    req(comparison_done(), data1())
    paste("Number of posts in Dataset 1:", nrow(data1()))
  })
  
  # Calculate the number of posts in Dataset 2
  output$dataset2_count_addition <- renderText({
    req(comparison_done(), data2())
    paste("Number of posts in Dataset 2:", nrow(data2()))
  })
  
  # Display the number of added posts
  output$added_count <- renderText({
    req(comparison_done(), added_posts())
    if (is.null(added_posts())) {
      return("Number of Added Posts: 0 (No valid data)")
    }
    paste("Number of Added Posts:", nrow(added_posts())) # Display the count of added posts
  })
  
  # Calculate consistency
  output$completeness <- renderText({
    req(comparison_done(), data1(), data2())
    completeness <- (nrow(data2()) / nrow(data1())) * 100
    paste("Completeness:", round(completeness, 1), "%")
  })
  
  # ===== Word Frequency Analysis ===== #
  # ===== Word Frequency – Added Posts (100% bullet-proof + fixed hc_add_series_empty) =====
  output$word_freq_plot_added <- renderHighchart({
    req(comparison_done(), added_posts())
    
    tryCatch({
      text_data <- added_posts()$text
      
      # ── Early exit for empty / single-row cases ─────────────────────────────
      if (is.null(text_data) || length(text_data) == 0 || 
          all(is.na(text_data) | trimws(text_data) == "")) {
        return(
          highchart() %>%
            hc_title(text = "Added Posts") %>%
            hc_subtitle(text = "No text content available")
        )
      }
      
      cleaned_text <- added_posts()$cleaned_text
      
      n_valid_docs   <- sum(nzchar(trimws(cleaned_text)))
      n_unique_lines <- length(unique(cleaned_text[nzchar(trimws(cleaned_text))]))
      
      if (n_valid_docs < 1) {
        return(
          highchart() %>%
            hc_title(text = "Added Posts") %>%
            hc_subtitle(text = "No valid text after cleaning")
        )
      }
      
      word_freq <- text_processor$get_freq(cleaned_text) %>%
        filter(freq > 1) %>%
        slice_head(n = 100)
      
      if (nrow(word_freq) == 0) {
        return(
          highchart() %>%
            hc_title(text = "Added Posts") %>%
            hc_subtitle(text = "Only 1 post → no meaningful word frequencies possible")
        )
      }
      
      # ── Normal plot ─────────────────
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_title(text = "Added Posts") %>%
        hc_tooltip(crosshairs = TRUE, shared = FALSE, useHTML = TRUE,
                   formatter = JS("function() {
                   var result = '<br/><span style=\"color:' + this.series.color + '\">' + 
                                this.point.category + '</span>:<b> ' + this.point.y + '</b>';
                   return result;
                 }")) %>%
        hc_xAxis(categories = word_freq$word,
                 labels = list(style = list(fontSize = '11px')), 
                 max = 20, scrollbar = list(enabled = TRUE)) %>%
        hc_add_series(name = "Word", data = word_freq$freq, type = "column",
                      color = "#4CAF50", showInLegend = FALSE)
      
    }, error = function(e) {
      # Never break the UI again
      highchart() %>%
        hc_title(text = "Added Posts") %>%
        hc_subtitle(text = paste("Error prevented:", e$message))
    })
  })
  
  output$word_freq_plot_original <- renderHighchart({
    req(comparison_done(), original_posts())
    
    text_data <- original_posts()$text
    
    cleaned_text <- original_posts()$cleaned_text
    
    n_valid_docs   <- sum(nzchar(trimws(cleaned_text)))
    n_unique_lines <- length(unique(cleaned_text[nzchar(trimws(cleaned_text))]))
    
    # if (n_valid_docs < 5) {
    #   showNotification("Original posts: too few documents with meaningful content after cleaning (< 5)", 
    #                    type = "warning")
    #   return(highchart() %>% 
    #            hc_title(text = "Original Posts") %>% 
    #            hc_subtitle(text = "Too few valid documents for word analysis"))
    # }
    # 
    # if (n_unique_lines <= 3) {
    #   showNotification("Original posts: very repetitive / near-identical content detected", 
    #                    type = "warning", duration = 8)
    #   # still render → user sees the repetition problem directly
    # }
    
    if (is.null(cleaned_text)) return(NULL)
    
    word_freq <- tryCatch({
      text_processor$get_freq(cleaned_text) %>%
        filter(freq > 1) %>%
        slice_head(n = 100)
    }, error = function(e) {
      showNotification(paste("Error calculating word frequencies for original posts:", e$message), type = "error")
      return(NULL)
    })
    
    # if (is.null(word_freq) || nrow(word_freq) == 0) {
    #   showNotification("No valid words found after processing original posts.", type = "warning")
    #   return(NULL)
    # }
    # 
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "Original Posts") %>%
      hc_tooltip(crosshairs = TRUE, shared = FALSE, useHTML = TRUE,
                 formatter = JS("function() {
                                   var result = '<br/><span style=\"color:' + this.series.color + '\">' + this.point.category + '</span>:<b> ' + this.point.y + '</b>';
                                   return result;
                 }")) %>%
      hc_xAxis(categories = word_freq$word,
               labels = list(style = list(fontSize = '11px')), max = 20, scrollbar = list(enabled = TRUE)) %>%
      hc_add_series(name = "Word", data = word_freq$freq, type = "column",
                    color = "#2196F3", showInLegend = FALSE)
  })
  #outputOptions(output, "word_freq_plot_original", suspendWhenHidden = TRUE)
  
  # ── New output for alerts ────────────────────────────────────────────────
  output$word_freq_added_alert <- renderUI({
    req(comparison_done(), added_posts())
    
    text_data <- added_posts()$text
    if (length(text_data) == 0 || all(is.na(text_data) | trimws(text_data) == "")) {
      return(div(class = "alert alert-warning", icon("exclamation-triangle"),
                 "No usable text content in added posts."))
    }
    
    cleaned <- added_posts()$cleaned_text
    
    n_valid <- sum(nzchar(trimws(cleaned)))
    n_unique <- length(unique(cleaned[nzchar(trimws(cleaned))]))
    
    if (n_valid < 5) {
      return(div(class = "alert alert-warning", icon("exclamation-triangle"),
                 "Too few documents with meaningful content after cleaning (", n_valid, ")."))
    }
    
    if (n_unique <= 3) {
      return(div(class = "alert alert-info", icon("info-circle"),
                 "Very low text diversity in added posts — most texts are near-identical or repetitive."))
    }
    
    NULL  # no message needed
  })
  
  output$word_freq_original_alert <- renderUI({
    req(comparison_done(), original_posts())
    
    text_data <- original_posts()$text
    if (length(text_data) == 0 || all(is.na(text_data) | trimws(text_data) == "")) {
      return(div(class = "alert alert-warning", icon("exclamation-triangle"),
                 "No usable text content in original posts."))
    }
    
    cleaned <- original_posts()$cleaned_text
    n_valid <- sum(nzchar(trimws(cleaned)))
    n_unique <- length(unique(cleaned[nzchar(trimws(cleaned))]))
    
    if (n_valid < 5) {
      return(div(class = "alert alert-warning", icon("exclamation-triangle"),
                 "Too few documents with meaningful content after cleaning (", n_valid, ")."))
    }
    
    if (n_unique <= 3) {
      return(div(class = "alert alert-info", icon("info-circle"),
                 "Very low text diversity in original posts — most texts are near-identical or repetitive."))
    }
    
    NULL
  })
  
  # ===== Keyness Analysis Module ===== #
  keyness_analyzer_addition <- list(
    prepare_data = function(added_posts, original_posts) {
      # ── Safety guard for zero added posts ─────────────────────────────
      n_added    <- nrow(added_posts)
      n_original <- nrow(original_posts)
      
      if (n_added == 0 && n_original == 0) {
        return(data.frame(word = character(0), 
                          group = character(0), 
                          freq = integer(0)))
      }
      
      if (n_added == 0) {
        # Still create a valid (empty) added group so frequency_table_creator doesn't crash
        added_clean <- character(0)
      } else {
        added_clean <- added_posts$cleaned_text
      }
      
      if (n_original == 0) {
        original_clean <- character(0)
      } else {
        original_clean <- original_posts$cleaned_text
      }
      
      combined_df <- data.frame(
        text  = c(added_clean, original_clean),
        group = c(rep("added",    length(added_clean)),
                  rep("original", length(original_clean)))
      )
      
      frequency_table_creator(
        df = combined_df,
        text_field = "text",
        grouping_variable = "group",
        grouping_variable_target = "added",
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
        sort_by = "ell"
      )
    }
  )
  
  # Reactive keyness analysis – PROTECTED against 0 added posts
  keyness_results_addition <- reactive({
    req(added_posts(), original_posts())
    
    # ── EARLY SAFE RETURN when nothing was added ─────────────────────
    if (nrow(added_posts()) == 0) {
      empty_tbl <- tibble(
        word           = character(0),
        log_likelihood = numeric(0),
        ell            = numeric(0),
        log_ratio      = numeric(0),
        word_use       = character(0)
      )
      return(list(
        overuse   = empty_tbl,
        underuse  = empty_tbl,
        all       = empty_tbl,
        max_ll_overall = 0,
        max_ell_overall = 0,
        top_term  = list(word = "—", log_likelihood = 0, ell = 0)
      ))
    }
    
    withProgress(message = 'Analyzing key terms...', value = 0.5, {
      freq_table <- keyness_analyzer_addition$prepare_data(added_posts(), original_posts())
      measures   <- keyness_analyzer_addition$calculate_keyness(freq_table)
      
      max_ll_overall <- max(measures$log_likelihood, na.rm = TRUE)
      max_ell_overall <- max(measures$ell, na.rm = TRUE)
      
      top_term_info <- measures %>%
        slice_max(log_likelihood, n = 1, with_ties = FALSE) %>%
        select(word, log_likelihood, ell) %>%
        as.list()
      
      filter_terms <- function(use_type, n = 5) {
        measures %>%
          filter(word_use == use_type, 
                 log_likelihood > 3.84) %>%
          arrange(desc(log_likelihood)) %>%
          slice(1:n)
      }
      
      list(
        overuse = filter_terms("overuse"),
        underuse = filter_terms("underuse"),
        all = measures %>% 
          filter(log_likelihood > 3.84) %>%
          arrange(desc(ell)),
        max_ll_overall = max_ll_overall,
        max_ell_overall = max_ell_overall,
        top_term = top_term_info
      )
    })
  })
  
  # ===== Improved Keyness Alert for Data Addition =====
  keyness_alert_addition <- reactive({
    req(keyness_results_addition(), input$keyness_tabs_addition)
    
    results <- keyness_results_addition()
    n_added    <- nrow(added_posts())
    n_original <- nrow(original_posts())
    
    # ── NEW: Guard for completely empty results (0 added posts) ─────────────────
    if (nrow(results$all) == 0) {
      if (input$keyness_tabs_addition == "added") {
        return("No added posts → keyness analysis not applicable for this view.")
      } else {
        return(NULL)   # let original/combined tabs show their normal alert (or nothing)
      }
    }
    
    # Helper function (same logic as in deletion, but adapted for addition)
    get_alert <- function(max_ll, max_ell, top_term, view_name) {
      if (max_ll < 3.84) {
        paste0(
          "⚠️ Keyness analysis limited in ", view_name, ": ",
          "All terms have log-likelihood below 3.84. ",
          "Highest term is '", top_term$word, "' (LL = ", round(top_term$log_likelihood, 2),
          ", ELL = ", round(top_term$ell, 6), "). ",
          "No terms meet the statistical significance threshold (p < 0.05). ",
          "(Group sizes: Added = ", n_added, ", Original = ", n_original, ")"
        )
        
      } else if (max_ll < 10) {
        paste0(
          "⚠️ Low keyness detected in ", view_name, ": ",
          "Most distinctive term is '", top_term$word, "' (LL = ", round(top_term$log_likelihood, 2),
          ", ELL = ", round(top_term$ell, 6), "). ",
          "Although some terms are technically significant, the differences appear weak in practice. ",
          "Interpret results with caution. ",
          "(Group sizes: Added = ", n_added, ", Original = ", n_original, ")"
        )
        
      } else {
        NULL   # No alert needed
      }
    }
    
    # Determine which view is active and get the appropriate max_ll / top term
    if (input$keyness_tabs_addition == "added") {
      if (nrow(results$overuse) > 0) {
        top <- results$overuse %>% 
          slice_max(log_likelihood, n = 1, with_ties = FALSE) %>% 
          select(word, log_likelihood, ell)
        max_ll <- top$log_likelihood
        max_ell <- top$ell
      } else {
        # fallback if no overuse terms
        top <- results$all %>% slice_max(log_likelihood, n = 1, with_ties = FALSE)
        max_ll <- top$log_likelihood
        max_ell <- top$ell
      }
      get_alert(max_ll, max_ell, top, "Added Posts")
      
    } else if (input$keyness_tabs_addition == "original") {
      if (nrow(results$underuse) > 0) {
        top <- results$underuse %>% 
          slice_max(log_likelihood, n = 1, with_ties = FALSE) %>% 
          select(word, log_likelihood, ell)
        max_ll <- top$log_likelihood
        max_ell <- top$ell
      } else {
        top <- results$all %>% slice_max(log_likelihood, n = 1, with_ties = FALSE)
        max_ll <- top$log_likelihood
        max_ell <- top$ell
      }
      get_alert(max_ll, max_ell, top, "Original Posts")
      
    } else {  # Combined View
      if (nrow(results$all) > 0) {
        top <- results$all %>% 
          slice_max(log_likelihood, n = 1, with_ties = FALSE) %>% 
          select(word, log_likelihood, ell)
        max_ll <- results$max_ll_overall
        max_ell <- results$max_ell_overall
      } else {
        top <- tibble(word = "—", log_likelihood = 0, ell = 0)
        max_ll <- 0
        max_ell <- 0
      }
      get_alert(max_ll, max_ell, top, "Combined View")
    }
  })
  
  # # === DEBUG: Why keyness works here ===
  # keyness_debug_addition <- reactive({
  #   req(added_posts(), original_posts())
  #   
  #   clean_added    <- text_processor$clean(added_posts()$text,    use_stem = FALSE, use_lemma = TRUE)
  #   clean_original <- text_processor$clean(original_posts()$text, use_stem = FALSE, use_lemma = TRUE)
  #   
  #   freq_added    <- text_processor$get_freq(clean_added)    %>% slice_head(n = 10)
  #   freq_original <- text_processor$get_freq(clean_original) %>% slice_head(n = 10)
  #   
  #   freq_table <- keyness_analyzer_addition$prepare_data(added_posts(), original_posts())
  #   measures   <- keyness_analyzer_addition$calculate_keyness(freq_table)
  #   
  #   list(
  #     `Unique cleaned texts - Added`     = length(unique(clean_added)),
  #     `Unique cleaned texts - Original`  = length(unique(clean_original)),
  #     `Top 10 words in Added`            = freq_added,
  #     `Top 10 words in Original`         = freq_original,
  #     `Top 20 Keyness values (ELL)`      = measures %>% 
  #       slice_head(n = 20) %>% 
  #       select(word, log_likelihood, ell, word_use)
  #   )
  # })
  
  # Render keyness plot
  output$keyness_plot_addition <- renderHighchart({
    req(keyness_results_addition(), input$keyness_tabs_addition)
    
    # Tab-specific logic
    if (input$keyness_tabs_addition == "added" && nrow(added_posts()) == 0) {
      return(highchart() %>%
               hc_title(text = "Terms Distinctive of Added Posts") %>%
               hc_subtitle(text = "No added posts to analyse"))
    }
    
    if(input$keyness_tabs_addition == "added") {
      keyness_data <- keyness_results_addition()$overuse %>%
        mutate(color = "#4CAF50", y = ell)
      title_text <- "Terms Distinctive of Added Posts (by Effect Size)"
      
    } else if(input$keyness_tabs_addition == "original") {
      keyness_data <- keyness_results_addition()$underuse %>%
        mutate(color = "#2196F3", y = ell)
      title_text <- "Terms Distinctive of Original Posts (by Effect Size)"
      
    } else {  # Combined
      overuse_top <- keyness_results_addition()$overuse %>% 
        slice(1:5) %>% mutate(color = "#4CAF50", y = ell)
      underuse_top <- keyness_results_addition()$underuse %>% 
        slice(1:5) %>% mutate(color = "#2196F3", y = ell)
      keyness_data <- bind_rows(overuse_top, underuse_top) %>%
        arrange(desc(abs(y)))
      title_text <- "Keyness Analysis: Effect Size Comparison"
    }
    
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
        text = paste0("Comparing ", nrow(added_posts()), " added posts to ", 
                      nrow(original_posts()), " original posts")
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
        labels = list(format = "{value:.6f}"),
        plotLines = if(input$keyness_tabs_addition == "combined") 
          list(list(value = 0, color = "#666", width = 1, zIndex = 5)) else NULL
      ) %>%
      hc_tooltip(
        formatter = JS("function() {
        var corpus = this.point.y > 0 ? 'Added' : 'Original';
        var ell = (this.point.y).toFixed(6);
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
  
  # Render keyness interpretation
  output$keyness_interpretation_addition <- renderUI({
    req(keyness_results_addition(), input$keyness_tabs_addition)
    
    if (nrow(keyness_results_addition()$all) == 0) {
      return(div(class = "alert alert-info", 
                 icon("info-circle"),
                 "Keyness interpretation is only available when posts were added."))
    }
    
    if(input$keyness_tabs_addition == "added") {
      top_terms <- keyness_results_addition()$overuse %>%
        slice(1:5) %>%
        mutate(info = paste0(word, " (LL: ", round(log_likelihood, 1), ", ELL: ", sprintf("%.6f", ell), ")"))
      
      HTML(paste0(
        "<div style='margin-top: 20px; background: #f8f9fa; padding: 15px; border-radius: 5px;'>",
        "<h5>Understanding Key Terms in Added Posts</h5>",
        "<div style='color: #4CAF50;'>",
        paste("- ", top_terms$info, collapse = "<br>"),
        "</div>",
        "<p style='margin-top: 10px; font-size: 0.9em; color: #666;'>",
        "These words appear much more often in added posts than in the original ones.<br>",
        "<strong>Example:</strong> If the word 'new' appears a lot in added posts but not in original ones, it will show up here.<br>",
        "<strong>LL (Log-likelihood)</strong> tells us how statistically significant the difference is (a value above 3.84 means it's important) <a href='https://ucrel.lancs.ac.uk/llwizard.html' target='_blank'>[1]</a> <a href='https://www.lancaster.ac.uk/fss/courses/ling/corpus/blue/l08_4.htm' target='_blank'>[2]</a>.<br>",
        "<strong>ELL (Effect Size)</strong> shows how strong that difference is (closer to 1 = bigger difference).",
        "</p>",
        "</div>"
      ))
      
    } else if(input$keyness_tabs_addition == "original") {
      top_terms <- keyness_results_addition()$underuse %>%
        slice(1:5) %>%
        mutate(info = paste0(word, " (LL: ", round(log_likelihood, 1), ", ELL: ", sprintf("%.6f", ell), ")"))
      
      HTML(paste0(
        "<div style='margin-top: 20px; background: #f8f9fa; padding: 15px; border-radius: 5px;'>",
        "<h5>Understanding Key Terms in Original Posts</h5>",
        "<div style='color: #2196F3;'>",
        paste("- ", top_terms$info, collapse = "<br>"),
        "</div>",
        "<p style='margin-top: 10px; font-size: 0.9em; color: #666;'>",
        "These words show up more often in original posts compared to added ones.<br>",
        "<strong>Example:</strong> If the word 'old' is common in original posts but not in added ones, it will appear here.<br>",
        "LL (Log-likelihood) and ELL (Effect Size) explain how important and how strong the difference is.",
        "</p>",
        "</div>"
      ))
      
    } else {
      top_added <- keyness_results_addition()$overuse %>%
        slice(1:5) %>%
        mutate(info = paste0(word, " (LL: ", round(log_likelihood, 1), ", ELL: ", sprintf("%.6f", ell), ")"))
      
      top_original <- keyness_results_addition()$underuse %>%
        slice(1:5) %>%
        mutate(info = paste0(word, " (LL: ", round(log_likelihood, 1), ", ELL: ", sprintf("%.6f", ell), ")"))
      
      HTML(paste0(
        "<div style='margin-top: 20px; background: #f8f9fa; padding: 15px; border-radius: 5px;'>",
        "<h5>Quick Guide: Comparing Key Terms</h5>",
        "<div style='columns: 2;'>",
        "<div style='color: #4CAF50;'>",
        "<strong>Common in Added Posts:</strong><br>",
        paste("- ", top_added$info, collapse = "<br>"),
        "</div>",
        "<div style='color: #2196F3; margin-left: 30px;'>",
        "<strong>Common in Original Posts:</strong><br>",
        paste("- ", top_original$info, collapse = "<br>"),
        "</div>",
        "</div>",
        "<p style='margin-top: 10px; font-size: 0.9em; color: #666;'>",
        "This helps you understand which words are more typical in each group.<br>",
        "LL tells us if it's a meaningful difference (above 3.84 = likely real).<br>",
        "ELL shows how big the difference is (0 to 1 scale, closer to 1 = bigger).<br>",
        "<strong>Example:</strong> 'new' might appear more in added posts, while 'old' might appear more in original posts.",
        "</p>",
        "</div>"
      ))
    }
  })
  
  # ===== Sentiment Analysis ===== #
  # Calculate sentiment distribution for a text vector
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
  
  # Optimized function to identify most extreme posts
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
  
  # Sentiment distribution plots for added and original posts
  # ── Safe sentiment data for Added Posts (no more warning) ─────────────────────
  sentiment_data_added <- reactive({
    req(comparison_done(), added_posts())
    
    tryCatch({
      text_data <- added_posts()$text
      if (length(text_data) == 0 || all(is.na(text_data) | trimws(text_data) == "")) {
        return(NULL)
      }
      
      # This line eliminates the annoying sentimentr warning forever
      sentences <- get_sentences(text_data)
      
      scores <- sentiment_by(sentences)$ave_sentiment
      
      neg_count <- sum(scores < 0, na.rm = TRUE)
      neu_count <- sum(abs(scores) < 0.01, na.rm = TRUE)   # neutral zone
      pos_count <- sum(scores > 0, na.rm = TRUE)
      total     <- length(scores)
      
      if (total == 0) return(NULL)
      
      list(
        pct   = c(neg_count/total*100, neu_count/total*100, pos_count/total*100),
        total = total
      )
    }, error = function(e) {
      NULL   # graceful fallback
    })
  })
  
  # Updated render for added sentiment plot
  output$sentiment_plot_added <- renderHighchart({
    req(sentiment_data_added())
    
    data <- sentiment_data_added()
    pct <- data$pct
    total <- data$total
    
    # Edge case: very small dataset warning (optional, but good for UX)
    if (total < 10) {
      showNotification("Sentiment analysis on added posts: Small sample size (<10 posts) may not be reliable.", type = "warning")
    }
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Added Posts Sentiment") %>%
      hc_subtitle(text = if (total == 0) "No data available" else NULL) %>%
      hc_xAxis(categories = c("Negative", "Neutral", "Positive"),
               title = list(text = NULL)) %>%
      hc_yAxis(title = list(text = "Percentage"),
               labels = list(format = "{value}%"),
               min = 0, max = 100) %>%
      hc_add_series(name = "Added Posts", data = pct, color = "#4CAF50",
                    showInLegend = FALSE) %>%
      hc_plotOptions(column = list(
        minPointLength = 5,  # Makes zero bars visible as thin lines
        dataLabels = list(enabled = TRUE, format = "{y:.1f}%", inside = FALSE)
      )) %>%
      hc_tooltip(formatter = JS("function() {
      return '<b>' + this.x + '</b>: ' + this.y.toFixed(1) + '%';
    }"))
  })
  
  # Similarly for original (symmetric fix)
  sentiment_data_original <- reactive({
    req(comparison_done(), original_posts())
    
    text_data <- original_posts()$text
    if (length(text_data) == 0 || all(is.na(text_data) | trimws(text_data) == "")) {
      return(NULL)
    }
    
    scores <- sentimentr::sentiment_by(text_data)$ave_sentiment
    
    neg_count <- sum(scores < 0, na.rm = TRUE)
    neu_count <- sum(scores == 0, na.rm = TRUE)
    pos_count <- sum(scores > 0, na.rm = TRUE)
    total <- length(scores)
    
    if (total == 0) return(NULL)
    
    list(
      pct = c(neg_count / total * 100, neu_count / total * 100, pos_count / total * 100),
      total = total
    )
  })
  
  output$sentiment_plot_original <- renderHighchart({
    req(sentiment_data_original())
    
    data <- sentiment_data_original()
    pct <- data$pct
    total <- data$total
    
    if (total < 10) {
      showNotification("Sentiment analysis on original posts: Small sample size (<10 posts) may not be reliable.", type = "warning")
    }
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Original Posts Sentiment") %>%
      hc_subtitle(text = if (total == 0) "No data available" else NULL) %>%
      hc_xAxis(categories = c("Negative", "Neutral", "Positive"),
               title = list(text = NULL)) %>%
      hc_yAxis(title = list(text = "Percentage"),
               labels = list(format = "{value}%"),
               min = 0, max = 100) %>%
      hc_add_series(name = "Original Posts", data = pct, color = "#2196F3",
                    showInLegend = FALSE) %>%
      hc_plotOptions(column = list(
        minPointLength = 5,
        dataLabels = list(enabled = TRUE, format = "{y:.1f}%", inside = FALSE)
      )) %>%
      hc_tooltip(formatter = JS("function() {
      return '<b>' + this.x + '</b>: ' + this.y.toFixed(1) + '%';
    }"))
  })
  
  # # Inside dataAdditionModule, after comparison_done()
  # observe({
  #   req(added_posts())
  #   scores <- sentimentr::get_sentiment(added_posts()$text)
  #   print(summary(scores))
  #   print(table(cut(scores, breaks = c(-Inf, -0.05, 0.05, Inf), 
  #                   labels = c("Negative", "Neutral", "Positive"))))
  # })
  
  # Reactive expressions for most extreme posts
  most_positive_added_text <- reactive({
    req(comparison_done(), added_posts())
    withProgress(message = 'Finding most positive...', value = 0.5, {
      get_extreme_posts(added_posts(), type = "positive")
    })
  })
  
  most_negative_added_text <- reactive({
    req(comparison_done(), added_posts())
    withProgress(message = 'Finding most negative...', value = 0.5, {
      get_extreme_posts(added_posts(), type = "negative")
    })
  })
  
  most_positive_original_text <- reactive({
    req(comparison_done(), original_posts())
    withProgress(message = 'Finding most positive...', value = 0.5, {
      get_extreme_posts(original_posts(), type = "positive")
    })
  })
  
  most_negative_original_text <- reactive({
    req(comparison_done(), original_posts())
    withProgress(message = 'Finding most negative...', value = 0.5, {
      get_extreme_posts(original_posts(), type = "negative")
    })
  })
  
  # Dynamic UI boxes for most extreme posts
  output$most_positive_added_box <- renderUI({
    text <- most_positive_added_text()
    if(is.null(text) || text == "No data") return(div("No data"))
    
    div(style = paste0("background: #f0f8ff; padding: 10px; border-radius: 5px;",
                       "min-height: 50px; max-height: 300px;",
                       "overflow-y: auto; white-space: pre-wrap;"),
        text)
  })
  
  output$most_negative_added_box <- renderUI({
    text <- most_negative_added_text()
    if(is.null(text) || text == "No data") return(div("No data"))
    
    div(style = paste0("background: #fff0f0; padding: 10px; border-radius: 5px;",
                       "min-height: 50px; max-height: 300px;",
                       "overflow-y: auto; white-space: pre-wrap;"),
        text)
  })
  
  output$most_positive_original_box <- renderUI({
    text <- most_positive_original_text()
    if(is.null(text) || text == "No data") return(div("No data"))
    
    div(style = paste0("background: #f0f8ff; padding: 10px; border-radius: 5px;",
                       "min-height: 50px; max-height: 300px;",
                       "overflow-y: auto; white-space: pre-wrap;"),
        text)
  })
  
  output$most_negative_original_box <- renderUI({
    text <- most_negative_original_text()
    if(is.null(text) || text == "No data") return(div("No data"))
    
    div(style = paste0("background: #fff0f0; padding: 10px; border-radius: 5px;",
                       "min-height: 50px; max-height: 300px;",
                       "overflow-y: auto; white-space: pre-wrap;"),
        text)
  })
  
  # ===== Topic Modeling Reactive Values =====
  current_topic_addition <- reactiveVal(0)
  
  observeEvent(input$prev_topic_addition, {
    if(current_topic_addition() > 1) {
      current_topic_addition(current_topic_addition() - 1)
    }
  })
  
  observeEvent(input$next_topic_addition, {
    if(current_topic_addition() < input$num_topics_addition) {
      current_topic_addition(current_topic_addition() + 1)
    }
  })
  
  observeEvent(input$clear_topic_addition, {
    current_topic_addition(0)
  })
  
  # Reset topic when number of topics changes
  observeEvent(input$num_topics_addition, {
    current_topic_addition(0)
  })
  
  # Ensure the topicmodels_json_ldavis function is included (unchanged from your provided code):
  topicmodels_json_ldavis <- function(fitted, text_vector, doc_term) {
    library(dplyr)
    library(stringi)
    
    phi <- posterior(fitted)$terms %>% as.matrix()
    theta <- posterior(fitted)$topics %>% as.matrix()
    vocab <- colnames(phi)
    
    # Get indices of documents that survived in dtm
    valid_rows <- which(rowSums(as.matrix(doc_term)) > 0)
    
    # Subset theta and text_vector to match the filtered dtm
    theta <- theta[valid_rows, , drop = FALSE]
    text_vector_filtered <- text_vector[valid_rows]
    
    # Now calculate doc.length on the filtered texts
    doc_length <- vapply(text_vector_filtered, function(x) stri_count(x, regex = "\\S+"), integer(1))
    
    # Term frequencies from the filtered dtm
    term_freq <- colSums(as.matrix(doc_term))
    
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
    
    return(json)
  }
  
  # ===== LDAvis Output for Addition =====
  output$ldavis_output_addition <- renderUI({
    req(comparison_done(), input$num_topics_addition, input$topic_dataset_addition)
    
    # ── 1. SAFE dataset selection – NEVER pass a closure (this fixes the exact error) ──
    dataset <- switch(input$topic_dataset_addition,
                      "Added Posts"    = { req(added_posts());    added_posts() },
                      "Original Posts" = { req(original_posts()); original_posts() },
                      "Combined View"  = {
                        req(added_posts(), original_posts())
                        bind_rows(
                          added_posts()    %>% mutate(group = "added"),
                          original_posts() %>% mutate(group = "original")
                        )
                      }
    )
    
    # ── 2. Bullet-proof text column check (prevents closure being passed to clean) ──
    if (!is.data.frame(dataset) || nrow(dataset) == 0) {
      return(div(class = "alert alert-info", icon("info-circle"), "No posts available in this view."))
    }
    if (!"text" %in% names(dataset)) {
      return(div(class = "alert alert-danger", "Error: 'text' column is missing."))
    }
    
    text_vec <- dataset$text
    if (!is.character(text_vec)) {
      return(div(class = "alert alert-danger", "Error: 'text' column must be character type."))
    }
    
    # ── Your existing tiny-group check (kept exactly as you had it) ──
    n_valid <- sum(!is.na(dataset$text) & nzchar(trimws(dataset$text)))
    if (n_valid < 2) {
      return(div(class = "alert alert-info",
                 icon("info-circle"),
                 tags$strong("Topic modeling skipped"),
                 tags$p("Only ", n_valid, " valid document(s) in this view. ",
                        "Topic modeling requires at least 2 documents with text."),
                 tags$small("Other analyses (Word Frequency, Keyness, Sentiment) are still available.")))
    }
    
    # ── Your existing diversity check (fixed the "texts" typo → now uses text_vec) ──
    cleaned_sample <- head(dataset$cleaned_text, 500)
    cleaned_sample <- cleaned_sample[nzchar(cleaned_sample)]
    n_unique_clean <- length(unique(cleaned_sample))
    
    diversity_ratio <- n_unique_clean / min(n_valid, 500)
    
    if (diversity_ratio < 0.15 || n_unique_clean < 40) {
      return(div(class = "alert alert-warning",
                 "Topic modeling not meaningful — very low text diversity",
                 tags$br(),
                 sprintf("Documents: %d   •   Unique cleaned (sample): %d (%.0f%% diversity)", 
                         n_valid, n_unique_clean, 100 * diversity_ratio),
                 tags$br(), tags$br(),
                 "Likely caused by near-identical, cyclic or boilerplate content."
      ))
    }
    
    # ── Everything below this line stays EXACTLY as you had it (your withProgress, cleaning, LDA, JSON, etc.) ──
    withProgress(message = 'Checking dataset size...', value = 0.1, {
      
      MAX_DOCS_FOR_TOPIC_MODELING <- 8000
      
      if (nrow(dataset) > MAX_DOCS_FOR_TOPIC_MODELING) {
        view_name <- switch(input$topic_dataset_addition,
                            "Added Posts"    = "Added Posts view",
                            "Original Posts" = "Original Posts view",
                            "Combined View"  = "Combined View (Added + Original)",
                            input$topic_dataset_addition)
        
        return(
          div(class = "alert alert-warning", style = "margin: 20px;",
              icon("exclamation-triangle"),
              tags$strong("Topic modeling not available for this view"),
              tags$p(
                "The ", strong(view_name), " contains ", nrow(dataset), " posts.",
                tags$br(),
                "Topic modeling is limited to ", MAX_DOCS_FOR_TOPIC_MODELING, " documents to avoid long waits and to ensure smooth performance."
              )
          )
        )
      }
      
      withProgress(message = 'Generating topics...', value = 0.5, {
        
        cleaned <- dataset$cleaned_text   # ← pre-cached (works for Added, Original and Combined View)        valid_docs <- which    (cleaned != "" & !is.na(cleaned))
        valid_docs <- which(cleaned != "" & !is.na(cleaned))
        
        if (length(valid_docs) < 5) {
          return(div(class = "alert alert-warning",
                     "After preprocessing, fewer than 5 valid documents remain for topic modeling"))
        }
        
        cleaned <- cleaned[valid_docs]
        corpus <- Corpus(VectorSource(cleaned))
        dtm <- DocumentTermMatrix(corpus)
        dtm <- dtm[rowSums(as.matrix(dtm)) > 0, ]
        
        if (nrow(dtm) < 5 || ncol(dtm) < 5) {
          return(div(class = "alert alert-danger",
                     "Topic modeling failed - insufficient meaningful text patterns after preprocessing"))
        }
        
        n_unique <- length(unique(cleaned))
        k_adaptive <- max(2, min(input$num_topics_addition, round(n_unique / 8)))
        if (n_unique < 150) k_adaptive <- max(2, min(4, round(n_unique / 10)))
        
        lda_model <- tryCatch({
          LDA(dtm, k = input$num_topics_addition, control = list(seed = 1234))
        }, error = function(e) {
          showNotification(paste("Error in topic modeling:", e$message), type = "error")
          return(NULL)
        })
        
        if (is.null(lda_model)) {
          return(div(class = "alert alert-danger",
                     "Topic modeling failed - please check your data"))
        }
        
        json <- topicmodels_json_ldavis(lda_model, cleaned, dtm)
        
        div(
          style = "width: 100%; height: 80vh; min-height: 600px; max-height: 900px; 
                 border: 1px solid #ddd; border-radius: 8px; overflow: hidden; 
                 position: relative; background: white; margin-bottom: 20px;",
          div(
            id = "ldavis-wrapper-addition",
            style = "width: 100%; height: 100%; overflow: auto; position: relative;",
            LDAvis::renderVis(json),
            tags$script(HTML("
  function cleanupLDAvisAddition() {
    const wrapper = document.getElementById('ldavis-wrapper-addition');
    if (!wrapper) return;
    const sliders = wrapper.querySelectorAll('input[type=\"range\"]');
    sliders.forEach((slider, i) => { if (i > 0) { const container = slider.closest('.ldavis-control') || slider.parentElement; if (container) container.remove(); } });
    const labels = wrapper.querySelectorAll('.ldavis-control-label, .ldavis-control');
    labels.forEach((label, i) => { if (i > 0) label.remove(); });
    const radios = wrapper.querySelectorAll('input[type=\"radio\"][name=\"term\"]');
    radios.forEach((radio, i) => { if (i >= 2) { const lbl = radio.closest('label') || radio.parentElement; if (lbl) lbl.remove(); } });
    const vis = wrapper.querySelector('.vis, .ldavis, svg');
    if (vis) { vis.style.width = '100%'; vis.style.height = '100%'; vis.style.display = 'block'; }
  }
  $(document).ready(() => { setTimeout(cleanupLDAvisAddition, 600); setTimeout(cleanupLDAvisAddition, 1800); });
  $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"], .nav-link', function(e) { setTimeout(cleanupLDAvisAddition, 400); });
  $(window).on('resize', () => { setTimeout(cleanupLDAvisAddition, 300); });
"))
          )
        )
        
      })
    })
  })
  
  # Keyness alert
  output$keyness_alert_addition <- renderUI({
    req(keyness_results_addition())
    if (input$keyness_tabs_addition == "added" && nrow(added_posts()) == 0) {
      return(div(class = "alert alert-info", icon("info-circle"),
                 "No added posts → keyness analysis not applicable for this view."))
    }
    msg <- keyness_alert_addition()
    if (!is.null(msg)) {
      div(class = "alert alert-warning", icon("info-circle"), msg)
    }
  })
  
  return(list(
    added_posts = added_posts,
    original_posts = original_posts,
    comparison_done = comparison_done,
    keyness_results = keyness_results_addition,
    current_topic_addition = current_topic_addition
  ))
}