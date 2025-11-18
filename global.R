# ====================== #
# GLOBAL FILE - LIBRARY IMPORTS
# ====================== #
library(shiny)
library(shinyjs)        # For enhanced UI functionality
library(dplyr)          # Data manipulation
library(tm)             # Text mining
library(topicmodels)    # Topic modeling
library(sentimentr)     # Sentiment analysis
library(wordcloud2)     # Word cloud visualization
library(highcharter)    # Interactive charts
library(tidytext)       # Text processing
library(reshape2)       # Data reshaping
library(ggplot2)        # Visualization
library(DT)             # Interactive tables
library(stringdist)     # String distance calculation
library(shinyBS)        # For tooltips
library(quanteda)       # Quantitative text analysis
library(quanteda.textstats) # Text statistics
library(KeynessMeasures)    # Keyness analysis
library(SnowballC)      # For stemming
library(textstem)       # For lemmatization
library(LDAvis)         # LDA visualization
library(diffobj)        # For visual text diffs
library(htmltools)      # HTML tools for Shiny
library(bslib)          # Bootstrap library for Shiny

# ====================== #
# 2. TEXT PROCESSING MODULE
# ====================== #
text_processor = list(
  clean = function(text, use_stem = FALSE, use_lemma = FALSE) {
    corpus = Corpus(VectorSource(text)) %>%
      tm_map(content_transformer(tolower)) %>%
      tm_map(removeNumbers) %>%
      tm_map(removePunctuation) %>%
      tm_map(removeWords, stopwords(source = "smart")) %>%
      tm_map(stripWhitespace)
    
    # Apply stemming if requested
    if(use_stem) {
      corpus <- tm_map(corpus, stemDocument)
    }
    
    # Apply lemmatization if requested
    if(use_lemma) {
      text_vec <- sapply(corpus, as.character)
      text_vec <- lemmatize_strings(text_vec)
      return(text_vec)
    }
    
    sapply(corpus, as.character)
  },
  
  get_freq = function(text, gram_type = "Uni-gram") {
    if (gram_type == "Uni-gram") {
      corpus <- Corpus(VectorSource(text))
      dtm <- DocumentTermMatrix(corpus)
      freq <- colSums(as.matrix(dtm))
      data.frame(word = names(freq), freq = freq) %>% 
        arrange(desc(freq))
    } else {
      tibble(text = text) %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
        count(bigram, sort = TRUE) %>%
        rename(word = bigram, freq = n)
    }
  }
)

# ====================== #
# 3. COMMON FUNCTIONS
# ====================== #

# Initialize shared reactive values
shared_data <- reactiveValues(
  data1 = NULL,
  data2 = NULL,
  edit_distances = NULL,
  comparison_done = FALSE,
  file1_uploaded = FALSE,
  file2_uploaded = FALSE
)

# Function to detect the correct ID column
detect_id_column <- function(df, manual_name = NULL) {
  possible_ids <- c("id", "tweet_id", "comment_id", "post_id", "status_id")
  if (!is.null(manual_name) && manual_name %in% names(df)) {
    return(manual_name)
  }
  found <- intersect(possible_ids, names(df))
  if (length(found) > 0) return(found[1])
  NULL
}

# Common dataset handling logic
common_data_handler <- function(input, output, session) {
  # Disable Dataset 2 upload and Compare button initially
  observe({
    shinyjs::disable("file2")
    shinyjs::disable("compare")
  })
  
  # Handle Dataset 1 upload and ID validation
  observeEvent(input$file1, {
    req(input$file1)
    tryCatch({
      df1 <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
      id_col_1 <- detect_id_column(df1, input$id_col_1)
      
      if (is.null(id_col_1)) {
        output$id_validation_msg_1 <- renderUI({
          tags$p("Warning: ID column not found in Dataset 1 among predefined names ('id', 'tweet_id', 'comment_id', 'post_id', 'status_id'). Please specify explicitly.", style = "color: orange;")
        })
        output$date_validation_msg_1 <- renderUI(NULL)
        shared_data$file1_uploaded <- FALSE
      } else if (!"text" %in% names(df1)) {
        output$id_validation_msg_1 <- renderUI({
          tags$p("Error: 'text' column not found in Dataset 1.", style = "color: red;")
        })
        output$date_validation_msg_1 <- renderUI(NULL)
        shared_data$file1_uploaded <- FALSE
      } else if (shared_data$file2_uploaded && input$date1 >= input$date2) {
        output$id_validation_msg_1 <- renderUI({
          tags$p("Dataset 1 uploaded successfully.", style = "color: green;")
        })
        output$date_validation_msg_1 <- renderUI({
          tags$p("Error: Dataset 1 date must be before Dataset 2 date.", style = "color: red;")
        })
        shared_data$file1_uploaded <- FALSE
      } else {
        output$id_validation_msg_1 <- renderUI({
          tags$p("Dataset 1 uploaded successfully.", style = "color: green;")
        })
        output$date_validation_msg_1 <- renderUI({
          if (shared_data$file2_uploaded) {
            tags$p("Dataset 1 date validated successfully.", style = "color: green;")
          } else {
            NULL
          }
        })
        shared_data$data1 <- df1
        shared_data$file1_uploaded <- TRUE
        shinyjs::enable("file2")
      }
    }, error = function(e) {
      output$id_validation_msg_1 <- renderUI({
        tags$p(paste("Error reading Dataset 1:", e$message), style = "color: red;")
      })
      output$date_validation_msg_1 <- renderUI(NULL)
      shared_data$file1_uploaded <- FALSE
    })
  })
  
  # Handle manual ID column input for Dataset 1
  observeEvent(input$id_col_1, {
    req(input$file1, input$id_col_1)
    tryCatch({
      df1 <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
      if (!input$id_col_1 %in% names(df1) || !"text" %in% names(df1)) {
        output$id_validation_msg_1 <- renderUI({
          tags$p("Error: Specified ID column not found in Dataset 1 or 'text' column missing.", style = "color: red;")
        })
        output$date_validation_msg_1 <- renderUI(NULL)
        shared_data$file1_uploaded <- FALSE
      } else if (shared_data$file2_uploaded && input$date1 >= input$date2) {
        output$id_validation_msg_1 <- renderUI({
          tags$p("Dataset 1 ID column validated successfully.", style = "color: green;")
        })
        output$date_validation_msg_1 <- renderUI({
          tags$p("Error: Dataset 1 date must be before Dataset 2 date.", style = "color: red;")
        })
        shared_data$file1_uploaded <- FALSE
      } else {
        output$id_validation_msg_1 <- renderUI({
          tags$p("Dataset 1 ID column validated successfully.", style = "color: green;")
        })
        output$date_validation_msg_1 <- renderUI({
          if (shared_data$file2_uploaded) {
            tags$p("Dataset 1 date validated successfully.", style = "color: green;")
          } else {
            NULL
          }
        })
        shared_data$data1 <- df1
        shared_data$file1_uploaded <- TRUE
        shinyjs::enable("file2")
      }
    }, error = function(e) {
      output$id_validation_msg_1 <- renderUI({
        tags$p(paste("Error reading Dataset 1:", e$message), style = "color: red;")
      })
      output$date_validation_msg_1 <- renderUI(NULL)
      shared_data$file1_uploaded <- FALSE
    })
  })
  
  # Handle Dataset 2 upload and ID/date validation
  observeEvent(input$file2, {
    req(input$file2, shared_data$file1_uploaded)
    tryCatch({
      df2 <- read.csv(input$file2$datapath, stringsAsFactors = FALSE)
      id_col_2 <- detect_id_column(df2, input$id_col_2)
      
      if (is.null(id_col_2)) {
        output$id_validation_msg_2 <- renderUI({
          tags$p("Warning: ID column not found in Dataset 2 among predefined names ('id', 'tweet_id', 'comment_id', 'post_id', 'status_id'). Please specify explicitly.", style = "color: orange;")
        })
        output$date_validation_msg_2 <- renderUI(NULL)
        shared_data$file2_uploaded <- FALSE
      } else if (!"text" %in% names(df2)) {
        output$id_validation_msg_2 <- renderUI({
          tags$p("Error: 'text' column not found in Dataset 2.", style = "color: red;")
        })
        output$date_validation_msg_2 <- renderUI(NULL)
        shared_data$file2_uploaded <- FALSE
      } else if (input$date2 <= input$date1) {
        output$id_validation_msg_2 <- renderUI({
          tags$p("Dataset 2 ID column validated successfully.", style = "color: green;")
        })
        output$date_validation_msg_2 <- renderUI({
          tags$p("Error: Dataset 2 date must be after Dataset 1 date.", style = "color: red;")
        })
        shared_data$file2_uploaded <- FALSE
      } else {
        output$id_validation_msg_2 <- renderUI({
          tags$p("Dataset 2 uploaded successfully.", style = "color: green;")
        })
        output$date_validation_msg_2 <- renderUI({
          tags$p("Dataset 2 date validated successfully.", style = "color: green;")
        })
        shared_data$data2 <- df2
        shared_data$file2_uploaded <- TRUE
      }
    }, error = function(e) {
      output$id_validation_msg_2 <- renderUI({
        tags$p(paste("Error reading Dataset 2:", e$message), style = "color: red;")
      })
      output$date_validation_msg_2 <- renderUI(NULL)
      shared_data$file2_uploaded <- FALSE
    })
  })
  
  # Handle manual ID column input for Dataset 2
  observeEvent(input$id_col_2, {
    req(input$file2, input$id_col_2, shared_data$file1_uploaded)
    tryCatch({
      df2 <- read.csv(input$file2$datapath, stringsAsFactors = FALSE)
      if (!input$id_col_2 %in% names(df2) || !"text" %in% names(df2)) {
        output$id_validation_msg_2 <- renderUI({
          tags$p("Error: Specified ID column not found in Dataset 2 or 'text' column missing.", style = "color: red;")
        })
        output$date_validation_msg_2 <- renderUI(NULL)
        shared_data$file2_uploaded <- FALSE
      } else if (input$date2 <= input$date1) {
        output$id_validation_msg_2 <- renderUI({
          tags$p("Dataset 2 ID column validated successfully.", style = "color: green;")
        })
        output$date_validation_msg_2 <- renderUI({
          tags$p("Error: Dataset 2 date must be after Dataset 1 date.", style = "color: red;")
        })
        shared_data$file2_uploaded <- FALSE
      } else {
        output$id_validation_msg_2 <- renderUI({
          tags$p("Dataset 2 ID column validated successfully.", style = "color: green;")
        })
        output$date_validation_msg_2 <- renderUI({
          tags$p("Dataset 2 date validated successfully.", style = "color: green;")
        })
        shared_data$data2 <- df2
        shared_data$file2_uploaded <- TRUE
      }
    }, error = function(e) {
      output$id_validation_msg_2 <- renderUI({
        tags$p(paste("Error reading Dataset 2:", e$message), style = "color: red;")
      })
      output$date_validation_msg_2 <- renderUI(NULL)
      shared_data$file2_uploaded <- FALSE
    })
  })
  
  # Handle manual date input for Dataset 2
  observeEvent(input$date2, {
    req(input$file2, shared_data$file1_uploaded)
    tryCatch({
      df2 <- read.csv(input$file2$datapath, stringsAsFactors = FALSE)
      id_col_2 <- detect_id_column(df2, input$id_col_2)
      if (is.null(id_col_2) || !"text" %in% names(df2)) {
        # Do nothing, ID validation handles the message
      } else if (input$date2 <= input$date1) {
        output$date_validation_msg_2 <- renderUI({
          tags$p("Error: Dataset 2 date must be after Dataset 1 date.", style = "color: red;")
        })
        shared_data$file2_uploaded <- FALSE
      } else {
        output$date_validation_msg_2 <- renderUI({
          tags$p("Dataset 2 date validated successfully.", style = "color: green;")
        })
        shared_data$data2 <- df2
        shared_data$file2_uploaded <- TRUE
      }
    }, error = function(e) {
      output$id_validation_msg_2 <- renderUI({
        tags$p(paste("Error reading Dataset 2:", e$message), style = "color: red;")
      })
      output$date_validation_msg_2 <- renderUI(NULL)
      shared_data$file2_uploaded <- FALSE
    })
  })
  
  # Handle manual date input for Dataset 1
  observeEvent(input$date1, {
    req(input$file1, shared_data$file1_uploaded, shared_data$file2_uploaded)
    tryCatch({
      df1 <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
      id_col_1 <- detect_id_column(df1, input$id_col_1)
      if (is.null(id_col_1) || !"text" %in% names(df1)) {
        # Do nothing, ID validation handles the message
      } else if (input$date1 >= input$date2) {
        output$date_validation_msg_1 <- renderUI({
          tags$p("Error: Dataset 1 date must be before Dataset 2 date.", style = "color: red;")
        })
        shared_data$file1_uploaded <- FALSE
      } else {
        output$date_validation_msg_1 <- renderUI({
          tags$p("Dataset 1 date validated successfully.", style = "color: green;")
        })
        shared_data$file1_uploaded <- TRUE
      }
    }, error = function(e) {
      output$id_validation_msg_1 <- renderUI({
        tags$p(paste("Error reading Dataset 1:", e$message), style = "color: red;")
      })
      output$date_validation_msg_1 <- renderUI(NULL)
      shared_data$file1_uploaded <- FALSE
    })
  })
  
  # Enable Compare button when both datasets are valid
  observe({
    if (shared_data$file1_uploaded && shared_data$file2_uploaded) {
      shinyjs::enable("compare")
    } else {
      shinyjs::disable("compare")
    }
  })
  
  return(shared_data)
}