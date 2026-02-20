# ====================== #
# UI DEFINITION
# ====================== #
ui <- navbarPage(
title = div(
  style = "display: flex; align-items: center; gap: 15px; height: 100%;",
  tags$a(
    href = "https://kodaqs-toolbox.gesis.org/",
    target = "_blank",
    tags$img(src = "KODAQS_logo.jpg", height = "40px", style = "height: 36px; width: auto; object-fit: contain;")
  ), 
  tags$span("Datcha", style = "font-size: 26px; font-weight: bold;"),
  tags$a(
    href = "https://github.com/ECCdigital/gesis-datcha",
    target = "_blank",
    icon("github", class = "fa-lg"),
    style = "color: white; margin-left: 20px; vertical-align: middle; text-decoration: none;"
  )
),
# Fixed footer at the bottom of EVERY page
tags$footer(class = "app-footer",
            tags$div(
              HTML("&copy; 2025 Datcha &nbsp; • &nbsp;"),
              tags$a(href = "https://www.gesis.org/en/institute/imprint", 
                     target = "_blank", 
                     style = "color: #a0d8ff; text-decoration: underline;", 
                     "Imprint"),
              HTML(" &nbsp; • &nbsp; "),
              tags$a(href = "https://www.gesis.org/en/institute/data-protection", 
                     target = "_blank", 
                     style = "color: #a0d8ff; text-decoration: underline;", 
                     "Data Protection")
            )
            
),  # end of ui
    # Update navbar color and button styles
                 header = tags$style(HTML("
  .navbar { background-color: #1c4474 !important; }
  .navbar-default .navbar-brand { color: white !important; }
  .navbar-default .navbar-nav > li > a { color: white !important; }
  .navbar-default .navbar-nav > .active > a { background-color: #0f2a4a !important; color: white !important; }
  .btn-primary { background-color: #1c4474; border-color: #1a3e6b; }
  .btn-primary:hover { background-color: #1c4474; border-color: #1a3e6b; }
  
/* only style plain default buttons */
.btn.btn-default:not(.btn-primary) {
  background-color: #a02b93;
  color: white;
  border-color: #8a267b;
}
  .btn-default:hover { background-color: #1c4474; color: white; border-color: #8a267b; }
  .btn-sm { background-color: #1c4474; color: white; border-color: #1a3e6b; }
  .btn-sm:hover { background-color: #1c4474; color: white; border-color: #0d243f; }
  .btn-danger { background-color: #d9534f; border-color: #d43f3a; }
  .btn-danger:hover { background-color: #c9302c; border-color: #ac2925; }
  
  /* keep purple browse button */
.btn.btn-default:not(#compare):hover {
  background-color: #a02b93; 
  color: white; 
  border-color: #8a267b;
}

/* force compare button blue always */
#compare.btn-primary,
#compare.btn-primary:hover,
#compare.btn-primary:focus {
  background-color: #1c4474 !important;
  border-color: #1a3e6b !important;
  color: #fff !important;
}
")),
                 tags$style(HTML("
  html {
    position: relative;
    min-height: 100%;
  }
  body {
    padding-bottom: 180px;  /* Increased padding to push content down */
  }
  .app-footer {
    position: absolute;
    bottom: 0;
    left: 0;
    width: 100%;
    height: 70px;  /* Adjust to fit your content */
    background-color: #1c4474;
    color: white;
    padding: 15px 0;
    text-align: center;
    font-size: 14px;
  }
  .shiny-output-error { visibility: hidden; }
.shiny-output-error:before { content: ' '; visibility: visible; }
"))
,
                 
                 # <<< PRIVACY BANNER – FIXED VERSION >>>
tags$style(HTML("
  #data-privacy-banner {
    position: fixed;
    bottom: 20px;
    right: 20px;
    background: #1c4474;
    color: white;
    padding: 15px 20px;
    border-radius: 8px;
    box-shadow: 0 4px 12px rgba(0,0,0,0.3);
    z-index: 10000;
    max-width: 380px;
    font-size: 14px;
    line-height: 1.5;
  }
  #data-privacy-banner a {
    color: #a0d8ff;
    text-decoration: underline;
  }
  #data-privacy-banner .close-btn {
    background: none;
    border: none;
    color: white;
    font-size: 20px;
    cursor: pointer;
    float: right;
    margin-left: 15px;
    opacity: 0.8;
  }
  #data-privacy-banner .close-btn:hover { opacity: 1; }
")),
                 # <<< PRIVACY BANNER – FIXED VERSION >>>
                 div(id = "data-privacy-banner",
                     # Full banner
                     div(id = "privacy-full",
                         style = "position:fixed; bottom:20px; right:20px; background:#1c4474; color:white;
                padding:20px; border-radius:12px; box-shadow:0 6px 20px rgba(0,0,0,0.4);
                z-index:10000; max-width:380px; font-size:14px; line-height:1.5;",
                         tags$strong("Your uploaded data is NOT stored on our servers"),
                         tags$button("×",
                                     style = "background:none; border:none; color:white; font-size:28px;
                            cursor:pointer; float:right; margin-left:15px; opacity:0.8;",
                                     onclick = "Shiny.setInputValue('minimize_banner', Math.random());"),
                         tags$br(), tags$br(),
                         "All files exist only in your current browser session and are automatically deleted when you close the tab.",
                         tags$br(), tags$br(),
                         tags$a(href = "https://shiny.posit.co/r/articles/build/persistent-data-storage/",
                                target = "_blank", style = "color:#a0d8ff; text-decoration:underline;",
                                "Official Shiny article: No persistent storage →")
                     ),
                     # Minimized floating button
                     div(id = "privacy-minimized",
                         style = "display:none; position:fixed; bottom:25px; right:25px; width:56px; height:56px;
                background:#1c4474; color:white; border-radius:50%; box-shadow:0 4px 15px rgba(0,0,0,0.4);
                z-index:10000; cursor:pointer; text-align:center; line-height:56px; font-size:24px;",
                         onclick = "Shiny.setInputValue('expand_banner', Math.random());",
                         "i")
                 ),

# tags$head(
#   tags$style(HTML("
#   /* LDAvis container styling - prevent footer overlap */
#   #ldavis_output,
#   #ldavis_output_addition,
#   #ldavis_output_editing {
#     overflow: hidden !important;
#     min-height: 500px !important;
#     height: calc(100vh - 200px) !important; /* Adjust based on your layout */
#     width: 100% !important;
#     margin-bottom: 40px !important;
#     border: 1px solid #ddd;
#     border-radius: 8px;
#     position: relative;
#   }
# 
#   /* Ensure LDAvis iframe stays within bounds */
#   #ldavis_output iframe,
#   #ldavis_output_addition iframe,
#   #ldavis_output_editing iframe {
#     width: 100% !important;
#     height: 100% !important;
#     min-height: 500px !important;
#     max-height: calc(100vh - 250px) !important; /* Account for footer */
#     border: none !important;
#     position: relative !important;
#   }
# 
#   /* Container wrappers for topic modeling tabs */
#   #data_deletion_tabs .tab-content,
#   #data_addition .tab-content {
#     position: relative;
#     padding-bottom: 20px;
#   }
#   
#   /* Specific override for Topic Modeling tab panels */
#   #data_deletion_tabs [data-value='Topic Modeling'] .tab-pane,
#   #data_addition [data-value='Topic Modeling'] .tab-pane {
#     padding-bottom: 50px !important; /* Extra padding to avoid footer */
#   }
# "))
# ),
                 # 0. Overview Page
                 tabPanel("Overview",
                          fluidPage(
                            useShinyjs(), 
                            tags$head(
                              tags$style(HTML("
               .form-group, .shiny-input-container { margin-bottom: 8px; }
               .form-group { padding-bottom: 0px; }
               .shiny-date-input { padding-bottom: 0px; margin-bottom: 0px; }
               .tooltip-inner { max-width: 300px; padding: 10px; background-color: #f8f9fa; color: #212529; border: 1px solid #dee2e6; border-radius: 4px; }
               .fa-info-circle { color: #007bff; margin-left: 5px; cursor: pointer; }
               .highlight-topic { stroke: #FF5722 !important; stroke-width: 3px !important; filter: drop-shadow(0 0 5px rgba(255, 87, 34, 0.5)); }
               #ldavis_output { height: 80vh; min-height: 500px; width: 100%; border: 1px solid #ddd; border-radius: 4px; padding: 10px; background: white; }
               .topic-controls { margin-top: 15px; padding: 10px; background: #f8f9fa; border-radius: 5px; }
               .topic-nav-buttons { display: flex; justify-content: space-between; margin-top: 10px; }
               .datatable { margin: 20px 0; }
               .datatable td { max-width: 300px; overflow-wrap: break-word; }
               .dataset-stats { background-color: #e8f1ff; padding: 10px; border-radius: 5px; margin-bottom: 10px; border-left: 4px solid #1c4474; }
               .dataset-stats p { margin: 5px 0; font-weight: bold; }
              .fluid-page { position: relative; min-height: 100vh; }
               ")),   
                  
              tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
                           
                   tags$script(HTML('
               $(window).on("resize", function() {
                 var windowHeight = $(window).height();
                 var headerHeight = $(".navbar").outerHeight();
                 var controlsHeight = $(".topic-controls").outerHeight();
                 var newHeight = windowHeight - headerHeight - controlsHeight - 100;
                 $("#ldavis_output").height(newHeight);
               }).trigger("resize");
             ')),
                              tags$script(HTML('
               $(document).ready(function() {
                 $("[data-toggle=\'tooltip\']").tooltip();
               });
             ')),
                              tags$link(rel = "stylesheet", type = "text/css", href = "diffobj.css")
                            ),
                            
                            h2("Welcome to Datcha"),
                            p("Upload your datasets below to compare social media data collected at different times."),
                            
                            sidebarLayout(
                              sidebarPanel(
                                width = 4,
                                h4("Dataset Comparison"),
                                # === Dataset 1 ===
                                fluidRow(
                                  column(12, 
                                         tags$strong("Dataset 1 (.csv)"),
                                         tags$span(style="color:red;", "*"),
                                         fileInput("file1", NULL, accept = ".csv")
                                  ),
                                  column(12,
                                         checkboxInput("gdpr1",
                                                       tags$span(
                                                         tags$span(style="color:red;", "*"),
                                                         " I confirm Dataset 1 is fully GDPR-compliant ",
                                                         tags$i(class = "fa fa-info-circle", style = "color:#1c4474; cursor:pointer;",
                                                                title = "Click for details",
                                                                onclick = "Shiny.setInputValue('show_gdpr_info', Math.random());")
                                                       ),
                                                       value = FALSE
                                         ),
                                         conditionalPanel(
                                           condition = "!input.gdpr1",
                                           tags$div(style = "color: red; font-size: 90%; margin-top: -10px;",
                                                    "This confirmation is required to proceed.")
                                         )
                                  ),
                                  column(6, 
                                         textInput("id_col_1", "ID Column (optional)", placeholder = "e.g. 'id' or 'tweet_id'")
                                  ),
                                  column(6, 
                                         dateInput("date1", "Collection Date for Dataset 1 *", value = Sys.Date()-30)
                                  )
                                ),
                                uiOutput("id_validation_msg_1"),
                                uiOutput("date_validation_msg_1"),
                                hr(),
                                
                                # === Dataset 2 ===
                                fluidRow(
                                  column(12, 
                                         tags$strong("Dataset 2 (.csv)"),
                                         tags$span(style="color:red;", "*"),
                                         fileInput("file2", NULL, accept = ".csv")
                                  ),
                                  column(12,
                                         checkboxInput("gdpr2",
                                                       tags$span(
                                                         tags$span(style="color:red;", "*"),
                                                         " I confirm Dataset 2 is fully GDPR-compliant ",
                                                         tags$i(class = "fa fa-info-circle", style = "color:#1c4474; cursor:pointer;",
                                                                title = "Click for details",
                                                                onclick = "Shiny.setInputValue('show_gdpr_info', Math.random());")
                                                       ),
                                                       value = FALSE
                                         ),
                                         conditionalPanel(
                                           condition = "input.gdpr1 && !input.gdpr2",
                                           tags$div(style = "color: red; font-size: 90%; margin-top: -10px;",
                                                    "This confirmation is required to enable comparison.")
                                         )
                                  ),
                                  column(6, 
                                         textInput("id_col_2", "ID Column (optional)", placeholder = "e.g. 'id' or 'tweet_id'")
                                  ),
                                  column(6, 
                                         dateInput("date2", "Collection Date for Dataset 2 *", value = Sys.Date())
                                  )
                                ),
                                uiOutput("id_validation_msg_2"),
                                uiOutput("date_validation_msg_2"),
                                hr(),
                                actionButton("compare", "Compare Datasets", class = "btn-primary", width = "100%")
                              ),
                              
                              mainPanel(
                                width = 8,
                                fluidRow(
                                  column(6,
                                         h4("How to Use Datcha:"),
                                         tags$ol(
                                           tags$li("Upload your two datasets below and set their collection dates"),
                                           tags$li("Confirm that your datasets are GDPR-compliant. Click the ", tags$i(class = "fa fa-info-circle", style = "color:#1c4474;"), " button next to each checkbox to learn more."),
                                           tags$li("If your ID column is not one of 'id', 'tweet_id', 'comment_id', 'post_id', or 'status_id', enter the correct name manually in the 'ID Column' field."),
                                           tags$li("Upload Dataset 1 first, then Dataset 2 (it becomes enabled after Dataset 1 is valid)."),
                                           tags$li("Click 'Compare Datasets' to analyze the data"),
                                           tags$li("Navigate to the different tabs to explore specific analyses:"),
                                           tags$ul(
                                             tags$li("Data Deletion: Analyze removed posts"),
                                             tags$li("Data Addition: Identify newly added content"),
                                             tags$li("Data Editing: View edited posts and text changes")
                                           ),
tags$li(tags$strong("Your uploaded data is NOT stored on our servers. All files exist only in your current browser session and are automatically deleted when you close the tab."), " Click the ", tags$i("i"), " button in the bottom-right corner for more details.")
),
tags$li(tags$strong("Note on performance:"), "Some analyses (e.g., topic modeling, keyness, or sentiment) may take several minutes to complete, depending on data size. The tool works fastest on small or medium sized data sets. For very large datasets, we recommend calculating the indicators in your local environment."),
                                         hr(),
                                         h4("Key Features:"),
                                         tags$ul(
                                           tags$li("Identifies removed, added, and edited posts"),
                                           tags$li("Analyzes word frequency and key terms"),
                                           tags$li("Performs sentiment and topic analysis"),
                                           tags$li("Calculates data quality metrics"),
                                           tags$li("Visualizes text differences")
                                         ),
                                         #hr(),

                                  ),
                                  column(6,
                                         h4("Dataset Overview"),
                                         div(class = "dataset-stats",
                                             textOutput("dataset1_count"),
                                             textOutput("dataset2_count"),
                                             textOutput("removed_count"),
                                             textOutput("overview_added_count"),
                                             textOutput("edited_post_count")
                                         ),
                                         hr(),
                                         h4("Data Requirements:"),
                                         p("Please upload CSV files with:"),
                                         tags$ul(
                                           tags$li("A column containing unique post IDs"),
                                           tags$li("A column named 'text' containing the post content")
                                         )
                                  )
                                )
                              )
                            
                   )
                 )
     ),
  # Data Deletion Tab
  tabPanel("Data Deletion",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 h4("Summary Statistics"),
                 uiOutput("deletion_quality_indicators"),
                 
                 # Topic Modeling Controls (only visible when Topic Modeling tab is active)
                 conditionalPanel(
                   condition = "input.data_deletion_tabs == 'Topic Modeling'",
                   div(class = "topic-controls",
                       h4("Topic Controls"),
                       sliderInput("num_topics", "Number of Topics:", 
                                   min = 2, max = 10, value = 5, step = 1),
                       radioButtons("topic_dataset", "Show Topics For:",
                                    choices = c("Removed Posts", "Remaining Posts", "Combined View"),
                                    selected = "Removed Posts"),
                       hr(),
                       div(class = "topic-nav-buttons",
                           actionButton("prev_topic", "Previous Topic", class = "btn-sm"),
                           actionButton("next_topic", "Next Topic", class = "btn-sm"),
                           actionButton("clear_topic", "Clear", class = "btn-sm btn-danger")
                       )
                   )
                 )
               ),
               
               mainPanel(
                 width = 9,
                 tabsetPanel(
                   id = "data_deletion_tabs",
                   tabPanel("Word Frequency", 
                            fluidRow(
                              column(6, h4("Removed Posts"), highchartOutput("word_freq_plot_removed")),
                              column(6, h4("Remaining Posts"), highchartOutput("word_freq_plot_remaining"))
                            )
                   ),
                   tabPanel("Keyness Analysis",
                            fluidRow(
                              column(12,
                                     uiOutput("keyness_controls"),
                                     highchartOutput("keyness_plot"),
                                     uiOutput("keyness_interpretation")
                              )
                            )
                   ),
                   tabPanel("Topic Modeling",
                            div(style = "display: flex; flex-direction: column; height: calc(100vh - 150px);",
                                div(style = "flex: 1; min-height: 0;",
                                    uiOutput("ldavis_output")
                                )
                            )
                   ),
                   tabPanel("Sentiment Analysis", 
                            fluidRow(
                              column(6, h4("Removed Posts Sentiment"), 
                                     highchartOutput("sentiment_plot_removed")),
                              column(6, h4("Remaining Posts Sentiment"), 
                                     highchartOutput("sentiment_plot_remaining"))
                            ),
                            hr(),
                            h4("Most Extreme Posts"),
                            fluidRow(
                              column(6, 
                                     h5("Most Positive (Removed)"),
                                     uiOutput("most_positive_removed_box")
                              ),
                              column(6, 
                                     h5("Most Negative (Removed)"),
                                     uiOutput("most_negative_removed_box")
                              )
                            ),
                            fluidRow(
                              column(6, 
                                     h5("Most Positive (Remaining)"),
                                     uiOutput("most_positive_remaining_box")
                              ),
                              column(6, 
                                     h5("Most Negative (Remaining)"),
                                     uiOutput("most_negative_remaining_box")
                              )
                            )
                   )
                 )
               )
             )
           )
  ),
  
  # Data Addition Tab
  tabPanel("Data Addition",
           fluidPage(
             useShinyjs(),
             sidebarPanel(
               width = 3,
               h4("Summary Statistics"),
               uiOutput("addition_quality_indicators"),
               #textOutput("dataset1_count_addition"),
               #textOutput("dataset2_count_addition"),
               #textOutput("added_count"),
               textOutput("growth_rate"),
               textOutput("daily_addition_rate"),
               
               conditionalPanel(
                 condition = "input.data_addition == 'Topic Modeling'",
                 div(class = "topic-controls",
                     h4("Topic Controls"),
                     sliderInput("num_topics_addition", "Number of Topics:", 
                                 min = 2, max = 10, value = 5, step = 1),
                     radioButtons("topic_dataset_addition", "Show Topics For:",
                                  choices = c("Added Posts", "Original Posts", "Combined View"),
                                  selected = "Added Posts"),
                     hr(),
                     div(class = "topic-nav-buttons",
                         actionButton("prev_topic_addition", "Previous Topic", class = "btn-sm"),
                         actionButton("next_topic_addition", "Next Topic", class = "btn-sm"),
                         actionButton("clear_topic_addition", "Clear", class = "btn-sm btn-danger")
                     )
                 )
               )
             ),
             mainPanel(
               width = 9,
               tabsetPanel(
                 id = "data_addition",
                 tabPanel("Word Frequency", 
                          fluidRow(
                            column(6, h4("Added Posts"), highchartOutput("word_freq_plot_added")),
                            column(6, h4("Original Posts"), highchartOutput("word_freq_plot_original"))
                          )
                 ),
                 tabPanel("Keyness Analysis",
                          fluidRow(
                            column(12,
                                   tabsetPanel(
                                     id = "keyness_tabs_addition",  # Unique ID for Data Addition
                                     tabPanel("Added Posts", value = "added"),
                                     tabPanel("Original Posts", value = "original"),
                                     tabPanel("Combined View", value = "combined")
                                   ),
                                   highchartOutput("keyness_plot_addition"),
                                   uiOutput("keyness_interpretation_addition")
                            )
                          )
                 ),
                 tabPanel("Topic Modeling",
                          div(style = "display: flex; flex-direction: column; height: calc(100vh - 150px);",
                              div(style = "flex: 1; min-height: 0;",
                                  uiOutput("ldavis_output_addition")
                              )
                          )
                 ),
                 tabPanel("Sentiment Analysis",
                          fluidRow(
                            column(6, h4("Added Posts Sentiment"), 
                                   highchartOutput("sentiment_plot_added")),
                            column(6, h4("Original Posts Sentiment"), 
                                   highchartOutput("sentiment_plot_original"))
                          ),
                          hr(),
                          h4("Most Extreme Posts"),
                          fluidRow(
                            column(6, 
                                   h5("Most Positive (Added)"),
                                   uiOutput("most_positive_added_box")
                            ),
                            column(6, 
                                   h5("Most Negative (Added)"),
                                   uiOutput("most_negative_added_box")
                            )
                          ),
                          fluidRow(
                            column(6, 
                                   h5("Most Positive (Original)"),
                                   uiOutput("most_positive_original_box")
                            ),
                            column(6, 
                                   h5("Most Negative (Original)"),
                                   uiOutput("most_negative_original_box")
                            )
                          )
                 )
               )
             )
           )
  ),
  
  # Data Edition Tab (updated with sidebar)
  tabPanel("Data Editing",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 h4("Editing Statistics"),
                 div(style = "margin-bottom: 20px;",
                     uiOutput("editing_stats_ui")
                 ),
                 hr(),
                 h4("Most Edited Posts"),
                 p("View the posts with the most significant changes between datasets."),
                 numericInput("num_edited_posts", "Number of posts to show:", 
                              value = 20, min = 5, max = 100, step = 5)
               ),
               
               mainPanel(
                 width = 9,
                 tabsetPanel(
                   id = "data_editing_tabs",
                   tabPanel("Text Changes",
                            fluidRow(
                              column(12,
                                     h4("Most Edited Posts"),
                                     dataTableOutput("most_edited_posts")
                              )
                            )
                   )
                 )
               )
             )
           )
  ),
  
  # Documentation Tab
  tabPanel("Documentation",
           fluidPage(
             h2("Datcha Application Documentation"),
             h3("Overview"),
             p("Datcha is a Shiny-based web application designed to compare two social media datasets collected at different times. It analyzes changes in posts, including deletions, additions, and edits, and provides insights through visualizations and metrics such as word frequency, keyness analysis, topic modeling, and sentiment analysis. The app is built using R and leverages packages like quanteda, sentimentr, topicmodels, diffobj, and highcharter."),
             hr(),
             h3("Getting Started"),
             h4("Prerequisites"),
             tags$ul(
               tags$li("R and Shiny: The app requires R and the Shiny framework."),
               tags$li("Required Packages: Listed in global.R, including shiny, shinyjs, dplyr, tm, topicmodels, sentimentr, wordcloud2, highcharter, tidytext, DT, stringdist, shinyBS, quanteda, KeynessMeasures, SnowballC, textstem, LDAvis, diffobj, htmltools, and bslib."),
               tags$li("Directory Setup: Ensure a www/ directory exists for storing diffobj.css.")
             ),
             h4("Running the App"),
             p("The app is launched via app.R, which sources:"),
             tags$ul(
               tags$li("ui.R: Defines the user interface."),
               tags$li("server.R: Contains server logic."),
               tags$li("global.R: Includes global configurations, libraries, and shared functions."),
               tags$li("Module files (dataDeletion.R, dataAddition.R, dataEditing.R): Handle specific analyses.")
             ),
             p("Run the app with:"),
             tags$pre(tags$code("source(\"app.R\")")),
             hr(),
             h3("Data Requirements"),
             p("Datcha requires two CSV files, each containing:"),
             tags$ul(
               tags$li("A unique ID column (e.g., id, tweet_id, post_id, comment_id, status_id) to identify posts."),
               tags$li("A text column named 'text' containing post content.")
             ),
             p("Users can manually specify ID column names if they differ from the predefined set. The datasets represent social media posts collected at two different times, with the first dataset's collection date earlier than the second."),
             hr(),
             h3("User Interface (ui.R)"),
             h4("Layout"),
             p("The app uses a navbarPage with a custom dark blue theme (#1c4474) and includes five tabs:"),
             tags$ul(
               tags$li("Overview: Introduces the app and allows dataset uploads."),
               tags$li("Data Deletion: Analyzes posts removed between datasets."),
               tags$li("Data Addition: Analyzes posts added in the second dataset."),
               tags$li("Data Editing: Analyzes changes in matching posts."),
               tags$li("Documentation: Provides app details and usage instructions.")
             ),
             h4("Styling"),
             tags$ul(
               tags$li("Navbar: Dark blue background with white text."),
               tags$li("Buttons: Primary buttons are blue (#1c4474), default buttons are purple (#a02b93), and the 'Compare' button is always blue."),
               tags$li("Tooltips: Provide explanations for metrics and controls."),
               tags$li("Responsive Design: Adjusts visualizations (e.g., LDAvis output) based on window size.")
             ),
             hr(),
             h3("Server Logic (server.R)"),
             h4("Structure"),
             p("The server logic integrates three modules:"),
             tags$ul(
               tags$li("dataDeletionModule: Handles deletion analysis."),
               tags$li("dataAdditionModule: Handles addition analysis."),
               tags$li("dataEditingModule: Handles edit analysis.")
             ),
             p("These modules share data via a shared_data reactive object defined in global.R. The server updates shared_data based on module outputs and triggers UI updates."),
             h4("Shared Data"),
             p("The shared_data reactive object stores:"),
             tags$ul(
               tags$li("data1 and data2: The uploaded datasets."),
               tags$li("edit_distances: Levenshtein distances for edited posts."),
               tags$li("comparison_done: Boolean indicating if comparison is complete."),
               tags$li("file1_uploaded and file2_uploaded: Booleans tracking valid uploads.")
             ),
             hr(),
             h3("Global Functions (global.R)"),
             h4("Text Processing"),
             p("The text_processor object provides:"),
             tags$ul(
               tags$li("clean: Converts text to lowercase, removes numbers, punctuation, stopwords, and applies stemming or lemmatization."),
               tags$li("get_freq: Calculates word or bigram frequencies using tm or tidytext.")
             ),
             h4("ID Detection"),
             p("The detect_id_column function identifies the ID column by checking for common names (id, tweet_id, etc.) or using a user-specified name."),
             h4("Data Handling"),
             p("The common_data_handler function:"),
             tags$ul(
               tags$li("Disables the 'Dataset 2' upload and 'Compare' button until Dataset 1 is valid."),
               tags$li("Validates uploads for ID and text columns, and ensures date1 < date2."),
               tags$li("Displays validation messages (green for success, orange for warnings, red for errors).")
             ),
             hr(),
             h3("Dataset Upload and Validation"),
             h4("Upload Process"),
             tags$ul(
               tags$li("Dataset 1:"),
               tags$ul(
                 tags$li("User uploads a CSV file via fileInput('file1')."),
                 tags$li("Optional: Specify ID column name in textInput('id_col_1')."),
                 tags$li("Set collection date via dateInput('date1').")
               ),
               tags$li("Dataset 2:"),
               tags$ul(
                 tags$li("Enabled only after a valid Dataset 1 upload."),
                 tags$li("Similar inputs: fileInput('file2'), textInput('id_col_2'), dateInput('date2').")
               )
             ),
             h4("Validation"),
             tags$ul(
               tags$li("ID Column: Must exist in the dataset (auto-detected or manually specified)."),
               tags$li("Text Column: Must be named 'text'."),
               tags$li("Date: date1 must be before date2."),
               tags$li("Messages:"),
               tags$ul(
                 tags$li("Success: Green message (e.g., 'Dataset 1 uploaded successfully')."),
                 tags$li("Warning: Orange message if ID column not found among predefined names."),
                 tags$li("Error: Red message for missing text column, invalid ID, or incorrect dates.")
               )
             ),
             p("The 'Compare' button is enabled only when both datasets are valid (file1_uploaded and file2_uploaded are TRUE)."),
             hr(),
             h3("Tabs and Features"),
             h4("1. Overview Tab"),
             tags$ul(
               tags$li("Purpose: Allows dataset uploads and provides a summary of dataset statistics."),
               tags$li("Features:"),
               tags$ul(
                 tags$li("Upload Interface: Users upload two CSV files, specify ID columns (optional), and set collection dates."),
                 tags$li("Validation Messages: Displayed below each file input to indicate upload status."),
                 tags$li("Dataset Stats:"),
                 tags$ul(
                   tags$li("Number of posts in Dataset 1 and Dataset 2."),
                   tags$li("Number of deleted posts."),
                   tags$li("Number of added posts."),
                   tags$li("Number of edited posts.")
                 ),
                 tags$li("Instructions: Lists steps to use the app and data requirements."),
                 tags$li("Key Features: Highlights capabilities like deletion/addition analysis, sentiment analysis, and visualizations.")
               ),
               tags$li("Logic:"),
               tags$ul(
                 tags$li("The common_data_handler in global.R validates uploads and updates shared_data."),
                 tags$li("Outputs like dataset1_count, dataset2_count, removed_count, overview_added_count, and edited_post_count are rendered in server.R using module results.")
               )
             ),
             h4("2. Data Deletion Tab (dataDeletion.R)"),
             tags$ul(
               tags$li("Purpose: Analyzes posts present in Dataset 1 but absent in Dataset 2."),
               tags$li("Features:"),
               tags$ul(
                 tags$li("Summary Statistics:"),
                 tags$ul(
                   tags$li("Completeness: Percentage of posts retained in Dataset 2 (nrow(data2) / nrow(data1) * 100)."),
                   tags$li("Data Loss: Percentage of posts removed (nrow(removed_posts) / nrow(data1) * 100)."),
                   tags$li("Daily Removed: Average posts removed per day (nrow(removed_posts) / days_diff)."),
                   tags$li("Removal Rate: Daily removal as a percentage of total posts.")
                 ),
                 tags$li("Sub-tabs:"),
                 tags$ul(
                   tags$li("Word Frequency: Bar charts comparing word frequencies in removed vs. remaining posts using highcharter."),
                   tags$li("Keyness Analysis: Identifies distinctive terms in removed vs. remaining posts using quanteda (Log-likelihood and Effect Size)."),
                   tags$li("Topic Modeling: Visualizes topics in removed posts using LDAvis with controls for topic number and navigation."),
                   tags$li("Sentiment Analysis: Compares sentiment distributions in removed vs. remaining posts and highlights most positive/negative posts.")
                 )
               ),
               tags$li("Logic:"),
               tags$ul(
                 tags$li("Removed Posts: Calculated as data1 %>% filter(!(id1 %in% data2[[id2]]))."),
                 tags$li("Remaining Posts: Calculated as data1 %>% filter(id1 %in% data2[[id2]])."),
                 tags$li("Word Frequency: Uses text_processor$clean and get_freq to process text and generate bar charts."),
                 tags$li("Keyness: Uses quanteda to compute Log-likelihood and Effect Size (ELL) for term differences."),
                 tags$li("Topic Modeling: Applies LDA with topicmodels and visualizes with LDAvis."),
                 tags$li("Sentiment: Uses sentimentr to compute sentiment scores and plot distributions.")
               )
             ),
             h4("3. Data Addition Tab (dataAddition.R)"),
             tags$ul(
               tags$li("Purpose: Analyzes posts present in Dataset 2 but absent in Dataset 1."),
               tags$li("Features:"),
               tags$ul(
                 tags$li("Summary Statistics:"),
                 tags$ul(
                   tags$li("Number of added posts."),
                   tags$li("Data Addition: Percentage increase (nrow(added_posts) / nrow(data1) * 100)."),
                   tags$li("Daily Addition Rate: Posts added per day (nrow(added_posts) / days_diff)."),
                   tags$li("Daily Added: Daily addition as a percentage.")
                 ),
                 tags$li("Sub-tabs:"),
                 tags$ul(
                   tags$li("Word Frequency: Bar charts for added vs. original posts."),
                   tags$li("Keyness Analysis: Identifies distinctive terms in added vs. original posts."),
                   tags$li("Topic Modeling: Visualizes topics in added, original, or combined posts with controls for topic number and dataset selection."),
                   tags$li("Sentiment Analysis: Compares sentiment in added vs. original posts and highlights extreme posts.")
                 )
               ),
               tags$li("Logic:"),
               tags$ul(
                 tags$li("Added Posts: Calculated as data2 %>% filter(!(id2 %in% data1[[id1]]))."),
                 tags$li("Original Posts: Calculated as data2 %>% filter(id2 %in% data1[[id1]])."),
                 tags$li("Analysis: Similar to Data Deletion, using text_processor, quanteda, topicmodels, and sentimentr.")
               )
             ),
             h4("4. Data Editing Tab (dataEditing.R)"),
             tags$ul(
               tags$li("Purpose: Analyzes changes in posts present in both datasets."),
               tags$li("Features:"),
               tags$ul(
                 tags$li("Editing Statistics:"),
                 tags$ul(
                   tags$li("Mean Edit Distance: Average Levenshtein distance between matching posts."),
                   tags$li("Mean Normalized Distance: Edit distance divided by the longer text length (0-1 scale)."),
                   tags$li("Edited Post Ratio: Percentage of matched posts with changes.")
                 ),
                 tags$li("Most Edited Posts: Displays a table of posts with the highest edit distances, showing text differences using diffobj."),
                 tags$li("Control: Numeric input (num_edited_posts) to set the number of posts displayed (5–100).")
               ),
               tags$li("Logic:"),
               tags$ul(
                 tags$li("Edit Distance: Uses stringdist::stringdist to compute Levenshtein distances for matching posts (inner_join by ID)."),
                 tags$li("Text Differences: Visualized with diffChr from diffobj in a side-by-side format."),
                 tags$li("Table: Rendered with DT for interactive display, including ID, differences, edit distance, and normalized distance.")
               )
             ),
             h4("5. Documentation Tab"),
             tags$ul(
               tags$li("Purpose: Provides static information about the app."),
               tags$li("Content:"),
               tags$ul(
                 tags$li("About: Describes Datcha’s purpose."),
                 tags$li("How to Use: Lists steps for uploading and analyzing data."),
                 tags$li("Credits: Acknowledges developers (Dr. Yannik and Kunjan) and packages used."),
                 tags$li("Contact: Directs users to contact their team lead for support.")
               )
             ),
             hr(),
             h3("Key Features and Logic"),
             tags$ol(
               tags$li("Dataset Comparison:"),
               tags$ul(
                 tags$li("Logic: The 'Compare' button triggers analysis in all modules when both datasets are valid. It uses eventReactive to compute removed, added, and edited posts based on ID column matches."),
                 tags$li("Validation: Ensures ID and text columns exist and dates are in the correct order.")
               ),
               tags$li("Word Frequency Analysis:"),
               tags$ul(
                 tags$li("Logic: Uses text_processor$clean to preprocess text (lowercase, remove stopwords, etc.) and get_freq to compute frequencies. Top 100 words are plotted using highcharter.")
               ),
               tags$li("Keyness Analysis:"),
               tags$ul(
                 tags$li("Logic: Uses quanteda to compute Log-likelihood and Effect Size (ELL) for terms distinctive to one group (e.g., removed vs. remaining). Visualized as bar charts with tooltips explaining metrics.")
               ),
               tags$li("Topic Modeling:"),
               tags$ul(
                 tags$li("Logic: Applies LDA with topicmodels to identify topics. Visualized with LDAvis for interactive exploration. Users can adjust the number of topics (2–10) and navigate topics with buttons.")
               ),
               tags$li("Sentiment Analysis:"),
               tags$ul(
                 tags$li("Logic: Uses sentimentr to compute sentiment scores. Plots distributions and highlights the most positive/negative posts in styled boxes.")
               ),
               tags$li("Text Differences:"),
               tags$ul(
                 tags$li("Logic: Computes Levenshtein distances for matching posts and visualizes changes with diffobj. The DT table allows users to explore edited posts interactively.")
               ),
               tags$li("Data Quality Metrics:"),
               tags$ul(
                 tags$li("Logic: Calculates metrics like completeness, data loss, growth, and daily rates based on dataset sizes and date differences.")
               )
             ),
             hr(),
             h3("Error Handling"),
             tags$ul(
               tags$li("Invalid Data: Notifications are shown for missing ID/text columns or invalid dates."),
               tags$li("Empty Data: Checks for empty or invalid text data prevent crashes in analyses."),
               tags$li("Topic Modeling: Requires at least 5 non-empty documents to proceed.")
             ),
             hr(),
             h3("Conclusion"),
             p("Datcha provides a comprehensive tool for analyzing social media dataset changes over time. Its modular design and interactive visualizations make it suitable for researchers and analysts studying post dynamics. Future improvements could include support for additional file formats, enhanced performance for large datasets, and more advanced text analysis techniques.")
           )
  ),
  # # <<< REPLACE YOUR CURRENT BANNER WITH THIS ENTIRE BLOCK >>>
  # div(id = "data-privacy-banner",
  #     style = "display:none;",
  #     
  #     # Full banner (shown when expanded)
  #     div(id = "privacy-full",
  #         style = "position:fixed; bottom:20px; right:20px; background:#1c4474; color:white; 
  #               padding:15px 20px; border-radius:12px; box-shadow:0 6px 20px rgba(0,0,0,0.3); 
  #               z-index:10000; max-width:380px; font-size:14px; line-height:1.5; transition:all 0.3s ease;",
  #         tags$strong("Your uploaded data is NOT stored"),
  #         tags$button("×", 
  #                     style = "background:none; border:none; color:white; font-size:24px; 
  #                            cursor:pointer; float:right; margin-left:15px; opacity:0.8;",
  #                     onclick = "Shiny.setInputValue('minimize_banner', Math.random())"),
  #         tags$br(), tags$br(),
  #         "All files exist only in your current browser session and are deleted when you close the tab.",
  #         tags$br(), tags$br(),
  #         tags$a(href = "https://shiny.posit.co/r/articles/build/persistent-data-storage/", 
  #                target = "_blank", style = "color:#a0d8ff; text-decoration:underline;",
  #                "Official Shiny article: No persistent storage")
  #     ),
  #     
  #     # Minimized floating button (shown when collapsed)
  #     div(id = "privacy-minimized",
  #         style = "display:none; position:fixed; bottom:25px; right:25px; width:50px; height:50px; 
  #               background:#1c4474; color:white; border-radius:50%; box-shadow:0 4px 15px rgba(0,0,0,0.3); 
  #               z-index:10000; cursor:pointer; text-align:center; line-height:50px; font-size:20px;
  #               transition:all 0.3s ease;",
  #         onclick = "Shiny.setInputValue('expand_banner', Math.random())",
  #         "i"
  #     )
  # ),


)  # this closes the ui object

# End of navbarPage