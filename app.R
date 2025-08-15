library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(tools)
library(jsonlite)
library(shinyjs)
library(cookies)
library(promises)
library(future)
library(progressr)
library(data.table)
library(tidyr)
library(stringr)
library(Matrix)
# library(scales)
library(FlowSOM)
library(flowCore)
library(ConsensusClusterPlus)
library(Rtsne)
library(umap)
library(RColorBrewer)
library(Biobase)
library(reshape2)
library(uuid)
# Configure future to use multisession
plan(multisession)

# Set max file upload size to 30MB
options(shiny.maxRequestSize = 30*1024^2)

# Source analysis function
source("generate_evisualizer_json.R")
source("evAnalysis.R")
source("sessionManager.R")
source("getServerLoad.R")

# UI Definition
ui <- dashboardPage(
  dashboardHeader(
    title = "EV Data Analysis",
    dropdownMenu(type = "messages",
      messageItem(
        from = "System",
        message = "Welcome to EV Data Analysis Application",
        icon = icon("info")
      )
    ),
    dropdownMenuOutput("taskProgressMenu"),  # Make sure this is before notificationMenu
    dropdownMenuOutput("notificationMenu")
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Source", tabName = "data", icon = icon("upload")),
      menuItem("Analysis & Visualization", tabName = "analysis", icon = icon("chart-bar")),
      menuItem("Task History", tabName = "tasks", icon = icon("history")),
      menuItem("Preferences", tabName = "prefs", icon = icon("cog")),
      tags$br(),
      tags$div(
        class = "sidebar-menu",
        style = "position: fixed; bottom: 0; padding: 10px; font-size: 12px; color: #b8c7ce; width: 230px; border-top: 1px solid #374850;",
        textOutput("copyright")
      )
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$script(src = "js/cookies.js"),
      tags$style(HTML("
        .progress-description {
          margin: 0;
          padding: 0;
          font-size: 12px;
        }
        .task-menu .progress {
          margin: 0;
          padding: 0;
          height: 15px;
        }
      "))
    ),
    tabItems(
      # Data Upload Tab
      tabItem(tabName = "data",
        fluidRow(
          box(
            title = "Upload Multiple CSV Files",
            width = 12,
            helpText(
              tags$span(
                style = "color: #f39c12;", # Warning color
                icon("warning"),
                "Each data file must be in CSV format with five columns named: ",
                tags$b("symbol"), ", ", 
                tags$b("ev"), ", ",
                tags$b("variable"), ", ",
                tags$b("value"),", and ",
                tags$b("group"), ".",
                "(Column names are case-sensitive.)",
              )
            ),
            fileInput("files", "Choose CSV Files",
                      multiple = TRUE,
                      accept = c("text/csv", "text/comma-separated-values,text/plain")),
          ),
          box(
            title = "Uploaded Files",
            width = 6,
            actionButton("removeAll", "Remove All Files",
                        class = "btn-danger",
                        style = "margin-bottom: 10px;"),
            DTOutput("fileList")
          ),
          box(
            title = "Selected File Data",
            width = 6,
            DTOutput("selectedData")
          )
        )
      ),
      
      # Analysis Tab
      tabItem(tabName = "analysis",
        fluidRow(
          column(width = 4,
            box(
              title = "Plot Controls",
              width = 12,
              numericInput("sampling_ratio", 
                          "Sampling Ratio:",
                          value = 4,
                          min = 1,
                          max = 100,
                          step = 0.5),
              numericInput("min_proteins",
                          "Minimum Proteins per EV:",
                          value = 2,
                          min = 1,
                          max = 100,
                          step = 1),
              numericInput("min_count",
                          "Minimum Count per Protein:",
                          value = 2,
                          min = 1,
                          max = 100,
                          step = 1),
              actionButton("runAnalysis", "Run Analysis", 
                          class = "btn-primary",
                          style = "margin-top: 25px;")
            )
          ),
          column(width = 8,
            box(
              title = "Visualization",
              width = 12,
              plotOutput("plot"),
              downloadButton("downloadPlot", "Download Plot", 
                             class = "btn-success",
                             style = "margin-top: 10px;")
            ),
            box(
              title = "Analysis Results",
              width = 12,
              downloadButton("downloadText", "Download JSON File", 
                          class = "btn-info",
                          style = "margin-bottom: 15px;"),  # Changed from margin-top to margin-bottom
              uiOutput("exploreResults"),
              verbatimTextOutput("analysisResults")
              )
          )
        )
      ),
      
      # Task Management Tab
      tabItem(tabName = "tasks",
        box(
          title = "Task History",
          width = 12,
          DTOutput("taskTable")
        )
      ),

       # Preferences Tab
      tabItem(tabName = "prefs",
        box(
          title = "Server Information",
          width = 12,
          DTOutput("serverInfoTable")
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  # Initialize reactive values first
  values <- reactiveValues(
    data_files = list(),
    selected_file = NULL,
    tasks = data.frame(
      TaskTime = character(),
      PlotFile = character(),
      TextFile = character(),
      stringsAsFactors = FALSE
    ),
    notifications = data.frame(  # Add this new reactive value
      time = character(),
      message = character(),
      type = character(),
      stringsAsFactors = FALSE
    )
  )

 # Initialize session management
  session_id <- reactiveVal(NULL)

  all_cookies <- extract_cookies(session$request)
  # Get a specific cookie by name
  sessionId <- all_cookies[["sessionId"]]

# Initialize session on startup
  observe({
    if (is.null(sessionId)) {
      new_id <- generateSessionId()
      session_id(new_id)
      runjs(sprintf('setCookie("sessionId", "%s", 1);', new_id))
      print(paste("New session created:", new_id))
    }
    else {
      session_id(sessionId)
      print(paste("Loaded session:", sessionId))
      values$tasks <- loadSessionData(sessionId)
    }
   
  })


  # Update session data when tasks change
  observe({
    req(session_id())
    req(values$tasks)
    
    print(paste("Saving session data for session ID:", session_id()))
    
    if (is.data.frame(values$tasks)) {
      tryCatch({
        saveSessionData(session_id(), values$tasks)
        showNotification("Session data saved", type = "message", duration = 2)
      }, error = function(e) {
        showNotification(
          sprintf("Error saving session data: %s", e$message),
          type = "error",
          duration = 5
        )
      })
    }
  })
  
  # Reactive values for data storage
  values <- reactiveValues(
    data_files = list(),
    selected_file = NULL,
    tasks = data.frame(
      TaskTime = character(),
      PlotFile = character(),
      TextFile = character(),
      stringsAsFactors = FALSE
    )
  )
  
  # File Upload Handler
  observeEvent(input$files, {
    req(input$files)
    
    # Required columns
    required_cols <- c("symbol", "ev", "variable", "value")
    
    # Process each uploaded file
    for(i in 1:nrow(input$files)) {
      file_info <- input$files[i, ]
      
      # Try to read the CSV file
      tryCatch({
        data <- read.csv(file_info$datapath)
        
        # Check if all required columns exist
        missing_cols <- setdiff(required_cols, names(data))
        
        if(length(missing_cols) > 0) {
          # Show warning for missing columns
          addNotification(
            sprintf("File '%s' is missing required columns: %s", 
                   file_info$name,
                   paste(missing_cols, collapse = ", ")),
            type = "warning",
            duration = 10
          )
          next  # Skip this file
        }
        
        # If validation passes, store the file
        file_data <- list(
          name = file_info$name,
          size = paste0(round(file_info$size / 1024, 2), " KB"),
          type = file_ext(file_info$name),
          date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          data = data,
          datapath = file_info$datapath
        )
        values$data_files[[file_info$name]] <- file_data
        
        addNotification(
          sprintf("File '%s' uploaded successfully!", file_info$name),
          type = "message"
        )
        
      }, error = function(e) {
        # Show error notification if file reading fails
        addNotification(
          sprintf("Error reading file '%s': %s", file_info$name, e$message),
          type = "error",
          duration = 10
        )
      })
    }
    
  })
  
  # Display file list
  output$fileList <- renderDT({
    req(length(values$data_files) > 0)
    
    file_info_df <- do.call(rbind, lapply(values$data_files, function(f) {
      data.frame(
        Filename = f$name,
        Size = f$size,
        Type = f$type,
        "Upload Date" = f$date,
        Delete = as.character(
          actionButton(
            paste0("delete_", f$name),
            label = "Delete",
            onclick = sprintf("Shiny.setInputValue('delete_file', '%s', {priority: 'event'})", f$name),
            class = "btn-danger btn-sm"
          )
        ),
        stringsAsFactors = FALSE
      )
    }))
    
    datatable(file_info_df, 
              selection = 'single',
              options = list(
                dom = 't',
                scrollY = "300px",
                scrollCollapse = TRUE,
                fixedColumns = list(rightColumns = 1), # Make delete column sticky
                autoWidth = TRUE
              ),
              escape = FALSE) %>%  # Allow HTML in the table
      formatStyle(
        'Delete',
        backgroundColor = 'white', # Ensure button background is visible
        position = 'sticky',
        right = 0,
        zIndex = 1
      )
  })
  
  # Display selected file data
  observeEvent(input$fileList_rows_selected, {
    req(input$fileList_rows_selected)
    selected_row <- input$fileList_rows_selected
    file_names <- names(values$data_files)
    values$selected_file <- values$data_files[[file_names[selected_row]]]
  })
  
  output$selectedData <- renderDT({
    req(values$selected_file)
    datatable(values$selected_file$data, 
              editable = TRUE,
              options = list(scrollX = TRUE,
                           scrollY = "300px",
                           scrollCollapse = TRUE))
  })

  # Download handler for plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      # Generate filename with timestamp
      paste("plot-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".png", sep = "")
    },
    content = function(file) {
      # Save the plot
      ggsave(file, 
             plot = last_plot(), 
             device = "png",
             width = 10, 
             height = 7, 
             dpi = 300)
    }
  )
  
  # Add download handler for text file
  output$downloadText <- downloadHandler(
    filename = function() {
      # Generate filename with timestamp
      paste("analysis-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".json", sep = "")
    },
    content = function(file) {
      # Check if we have text file results
      if (!is.null(values$latest_results) && !is.null(values$latest_results$json)) {
        # Copy the content to the new file
        writeLines(values$latest_results$json$content, file)
      } else {
        # Write placeholder if no results available
        writeLines("No analysis results available", file)
      }
    }
  )

  # Render explore results UI
  output$exploreResults <- renderUI({
    shiny::a(
              h4(span(class = "fa fa-external-link", style = "margin-right: 5px;"),"Explore the tSNE result", class = "btn btn-primary action-button", style = "font-weight:600;"),
              target = "_blank",
              href = paste0("https://www.svatlas.org/evisualizer/index.html?DATAURL=proxy.php?url=https://ev.svatlas.org/output/analysis-", session_id(),".json")
            )
  })
  
  # Display task table with download buttons
  output$taskTable <- renderDT({
    req(nrow(values$tasks) > 0)
    
    # Create table with download buttons
    file_info_df <- data.frame(
      TaskTime = values$tasks$TaskTime,
      "Plot File" = sapply(seq_len(nrow(values$tasks)), function(i) {
        sprintf(
          '<button onclick="Shiny.setInputValue(\'download_plot\', \'%s\')" class="btn btn-success btn-sm">Download Plot</button>',
          values$tasks$PlotFile[i]
        )
      }),
      "JSON File" = sapply(seq_len(nrow(values$tasks)), function(i) {
        sprintf(
          '<button onclick="Shiny.setInputValue(\'download_text\', \'%s\')" class="btn btn-info btn-sm">Download JSON File</button>',
          values$tasks$JSONFile[i]
        )
      }),
      stringsAsFactors = FALSE
    )
    
    datatable(file_info_df,
              options = list(
                order = list(list(0, 'desc')),
                pageLength = 10,
                scrollX = TRUE
              ),
              escape = FALSE)
  })

  # Add download handlers for plot and text files
  observeEvent(input$download_plot, {
    req(input$download_plot)
    filename <- basename(input$download_plot)
    showModal(modalDialog(
      title = "Download Plot",
      downloadButton("downloadTaskPlot", "Download"),
      easyClose = TRUE
    ))
  })

  observeEvent(input$download_text, {
    req(input$download_text)
    filename <- basename(input$download_text)
    showModal(modalDialog(
      title = "Download JSON File",
      downloadButton("downloadTaskText", "Download"),
      easyClose = TRUE
    ))
  })

  # Download handlers
  output$downloadTaskPlot <- downloadHandler(
    filename = function() {
      basename(input$download_plot)
    },
    content = function(file) {
      file.copy(input$download_plot, file)
    }
  )

  output$downloadTaskText <- downloadHandler(
    filename = function() {
      basename(input$download_text)
    },
    content = function(file) {
      file.copy(input$download_text, file)
    }
  )
  
  # Initialize task progress as a reactive value
  taskProgress <- reactiveVal(list(
    active = FALSE,
    step = "",
    progress = 0
  ))

  # Update the menu output
  output$taskProgressMenu <- renderMenu({
    progress <- taskProgress()
    print(paste("Task progress:", progress$step, "at", progress$progress * 100, "%"))
    dropdownMenu(
      type = "tasks",
      badgeStatus = if(progress$active) "success" else NULL,
      headerText = "Analysis Progress",
      .list = if(progress$active) {
        list(
          taskItem(
            value = progress$progress * 100,
            color = "aqua",
            text = progress$step
          )
        )
      } else {
        list()  # Empty list when not active
      }
    )
  })

  # Update the analysis handler progress updates
  observeEvent(input$runAnalysis, {
    if (length(values$data_files) == 0) {
      showModal(modalDialog(
        title = "Warning",
        "Please upload at least one data file to Data Source before starting an analysis task.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }

    # Check server load. if too high, dont run analysis; if medium, show warning
    server_load <- getServerLoad()
    message(sprintf("Current server load: %.2f", server_load))
    if (server_load > 0.8) {
      showModal(modalDialog(
        title = "Server Load Warning",
        "The server is currently under high load. Please try again later.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }

    if (server_load > 0.5) {
      showModal(modalDialog(
        title = "Server Load Warning",
        "The server is currently under high load. Analysis may take longer than usual.",
        easyClose = TRUE,
        footer = NULL
      ))
    }

    # Disable the runAnalysis button to prevent multiple clicks
    shinyjs::disable("runAnalysis")

    # Disable downloadText button during analysis
    shinyjs::disable("downloadText")

    # Run analysis with progress updates
    evAnalysis(values$data_files, tempdir(), input$sampling_ratio, input$min_proteins, input$min_count) %>%
      then(function(results) {
        
        # Generate filenames for plot and text files
        timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
        plot_file <- file.path("www/output", paste0("plot-", timestamp, ".png"))
        json_file <- file.path("www/output", paste0("analysis-", timestamp, ".json"))
        last_json_file <- file.path("www/output", paste0("analysis-", session_id(), ".json"))
        
        # Save the files
        if(!is.null(results$plot)) {
          ggsave(plot_file, plot = results$plot, width = 10, height = 7, dpi = 300)
        }
        if(!is.null(results$json)) {
          writeLines(results$json$content, json_file)
          writeLines(results$json$content, last_json_file)
        }
        
        # Add new task to history
        new_task <- data.frame(
          TaskTime = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          PlotFile = plot_file,
          JSONFile = json_file,
          LastJSONFile = last_json_file,
          stringsAsFactors = FALSE
        )
        values$tasks <- rbind(new_task, values$tasks)
        
        # Store results
        values$latest_results <- results
        
        # Update plot
        output$plot <- renderPlot({
          results$plot
        })
        
        # Show completion notification
        addNotification("Analysis completed successfully!", type = "message")
      }) %>%
      catch(function(error) {
        message("Analysis failed:", error$message)
        addNotification(
          sprintf("Analysis failed: %s", error$message),
          type = "error",
          duration = 10
        )
      })

    # Re-enable the runAnalysis button
    shinyjs::enable("runAnalysis")
    # Re-enable downloadText button
    shinyjs::enable("downloadText")
  })
  
  # Add handler for individual file deletion
  observeEvent(input$delete_file, {
    req(input$delete_file)
    
    # Remove the file from data_files
    values$data_files[[input$delete_file]] <- NULL
    
    # Show notification
    showNotification(
      sprintf("File '%s' removed successfully", input$delete_file),
      type = "message"
    )
    
  })
  
  # Add handler for removing all files
  observeEvent(input$removeAll, {
    req(length(values$data_files) > 0)
    
    # Clear all files
    values$data_files <- list()
    values$selected_file <- NULL

    
    # Show notification
    showNotification("All files removed successfully", type = "message")
  })
  
    # Replace or add to the existing serverInfo output
  output$serverInfoTable <- renderDT({
    info <- sessionInfo()
    sys_info <- Sys.info()
    
    # Create data frame for table
    data.frame(
      Parameter = c(
        "R Version",
        "Platform",
        "System",
        "Release",
        "Version",
        "Machine",
        "Hostname",
        "User",
        "Date",
        "Working Directory"
      ),
      Value = c(
        info$R.version$version.string,
        info$platform,
        sys_info["sysname"],
        sys_info["release"],
        sys_info["version"],
        sys_info["machine"],
        sys_info["nodename"],
        sys_info["user"],
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        getwd()
      )
    ) %>%
    datatable(
      options = list(
        dom = 't',
        ordering = FALSE,
        pageLength = -1
      ),
      rownames = FALSE,
      class = 'cell-border stripe',
      selection = 'none'
    )
  })

  output$copyright <- renderText({
    paste("© 2025 SVAtlas.org. All rights reserved.")
  })

  notifications <- reactiveVal(list())

  # Add notification function
  addNotification <- function(message, type = "message", duration = 5) {
    # Show popup notification
    showNotification(message, type = type, duration = duration)

    new_notification <- notificationItem(
            text = message,
            icon = icon("bell"),
            status = sample(c("success", "warning", "danger"), 1)
          )
    current_notifications <- notifications()
    notifications(c(current_notifications, list(new_notification)))
  }

  output$notificationMenu <- renderMenu({
        dropdownMenu(type = "notifications", .list = notifications())
    })
}



# Run the application
shinyApp(ui = ui, server = server)