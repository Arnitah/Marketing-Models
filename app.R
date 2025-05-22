library(shiny)
library(bslib)
library(stats)
library(dplyr)
library(DT)
library(plotly)
library(openxlsx)

# Custom function for Cronbach's alpha
calculate_cronbach_alpha <- function(data) {
    k <- ncol(data)
    var_sum <- sum(apply(data, 2, var, na.rm = TRUE))
    var_total <- var(rowSums(data, na.rm = TRUE))
    alpha <- (k/(k-1)) * (1 - var_sum/var_total)
    return(alpha)
}

# Define UI
ui <- page_fluid(
    theme = bs_theme(version = 5, bootswatch = "morph"),
    
    # Dashboard Title
    h1("Factor & Reliability Analysis Dashboard", class = "text-center mb-4"),
    
    # File Input Section
    card(
        card_header("Upload Data"),
        fileInput("file", "Upload Excel/CSV File",
                  accept = c(".xlsx", ".xls", ".csv")),
        uiOutput("sheet_ui")
    ),
    
    # Navigation Tabs
    navset_card_tab(
        # Tab: Data Preview
        nav_panel("Data Preview", 
                  card(DTOutput("data_table"))),
        
        # Tab: Exploratory Analysis
        nav_panel("Exploratory Analysis",
                  layout_columns(
                      col_widths = c(6, 6),
                      card(
                          card_header("Correlation Matrix"),
                          plotlyOutput("correlation_plot")
                      ),
                      card(
                          card_header("Variable Distribution"),
                          selectInput("var_select", "Select Variable", choices = NULL),
                          plotlyOutput("distribution_plot")
                      )
                  )
        ),
        
        # Tab: Factor Analysis
        nav_panel("Factor Analysis",
                  layout_columns(
                      col_widths = c(4, 8),
                      card(
                          card_header("Factor Analysis Settings"),
                          numericInput("n_factors", "Number of Factors", value = 3, min = 1, max = 10),
                          selectInput("rotation", "Rotation Method",
                                      choices = c("varimax", "promax")),
                          actionButton("run_fa", "Run Analysis", class = "btn-primary")
                      ),
                      card(
                          card_header("Scree Plot"), 
                          plotlyOutput("scree_plot")
                      )
                  ),
                  layout_columns(
                      col_widths = c(6, 6),
                      card(
                          card_header("Factor Loadings"), 
                          DTOutput("loadings_table")
                      ),
                      card(
                          card_header("Factor Diagram"),
                          plotOutput("factor_diagram", height = "400px")
                      )
                  )
        ),
        
        # Tab: Reliability Analysis
        nav_panel("Reliability Analysis",
                  layout_columns(
                      col_widths = c(6, 6),
                      card(
                          card_header("Cronbach's Alpha"), 
                          verbatimTextOutput("reliability_stats")
                      ),
                      card(
                          card_header("Item Analysis"), 
                          DTOutput("item_stats")
                      )
                  )
        ),
        
        # Tab: Download Results
        nav_panel("Download Results",
                  card(
                      card_header("Export Data"),
                      downloadButton("download_results", "Download Results")
                  )
        )
    )
)

# Define Server
server <- function(input, output, session) {
    # Reactive Values
    rv <- reactiveValues(data = NULL, fa_results = NULL)
    
    # File Upload Logic
    observeEvent(input$file, {
        req(input$file)
        ext <- tools::file_ext(input$file$datapath)
        if (ext == "csv") {
            rv$data <- read.csv(input$file$datapath)
        } else {
            sheets <- getSheetNames(input$file$datapath)
            updateSelectInput(session, "sheet", choices = sheets)
            rv$data <- read.xlsx(input$file$datapath, sheet = 1)
        }
        updateSelectInput(session, "var_select", choices = names(rv$data))
    })
    
    # Sheet Selection Logic
    output$sheet_ui <- renderUI({
        req(input$file)
        if (tools::file_ext(input$file$datapath) %in% c("xlsx", "xls")) {
            selectInput("sheet", "Select Sheet", choices = NULL)
        }
    })
    
    observeEvent(input$sheet, {
        req(input$file)
        rv$data <- read.xlsx(input$file$datapath, sheet = input$sheet)
        updateSelectInput(session, "var_select", choices = names(rv$data))
    })
    
    # Data Preview
    output$data_table <- renderDT({
        req(rv$data)
        datatable(rv$data, options = list(scrollX = TRUE))
    })
    
    # Correlation Matrix
    output$correlation_plot <- renderPlotly({
        req(rv$data)
        numeric_data <- rv$data[sapply(rv$data, is.numeric)]
        corr <- cor(numeric_data, use = "pairwise.complete.obs")
        plot_ly(x = colnames(corr), y = colnames(corr), z = corr, type = "heatmap")
    })
    
    # Variable Distribution
    output$distribution_plot <- renderPlotly({
        req(rv$data, input$var_select)
        plot_ly(x = rv$data[[input$var_select]], type = "histogram")
    })
    
    # Factor Analysis
    observeEvent(input$run_fa, {
        req(rv$data)
        tryCatch({
            numeric_data <- rv$data[sapply(rv$data, is.numeric)]
            data_scaled <- scale(numeric_data)
            pca_result <- princomp(data_scaled)
            
            loadings <- pca_result$loadings[, 1:input$n_factors]
            if(input$rotation == "varimax") {
                loadings <- varimax(loadings)$loadings
            }
            
            rv$fa_results <- list(
                loadings = loadings,
                values = pca_result$sdev^2,
                communality = rowSums(loadings^2),
                uniquenesses = 1 - rowSums(loadings^2)
            )
        }, error = function(e) {
            showNotification(paste("Error in factor analysis:", e$message), type = "error")
        })
    })
    
    output$loadings_table <- renderDT({
        req(rv$fa_results)
        loadings <- as.data.frame(unclass(rv$fa_results$loadings))
        colnames(loadings) <- paste0("Factor", 1:ncol(loadings))
        
        loadings$Communality <- rv$fa_results$communality
        loadings$Uniqueness <- rv$fa_results$uniquenesses
        
        datatable(round(loadings, 3),
                  options = list(scrollX = TRUE)) %>%
            formatStyle(
                names(loadings),
                backgroundColor = styleInterval(
                    c(-0.3, 0.3),
                    c("#FFF3F3", "white", "#F3FFF3")
                )
            )
    })
    
    output$scree_plot <- renderPlotly({
        req(rv$fa_results)
        eigenvalues <- rv$fa_results$values
        data <- data.frame(
            Factor = 1:length(eigenvalues),
            Eigenvalue = eigenvalues
        )
        
        plot_ly(data, x = ~Factor, y = ~Eigenvalue, type = "scatter", mode = "lines+markers") %>%
            layout(
                title = "Scree Plot",
                xaxis = list(title = "Factor Number"),
                yaxis = list(title = "Eigenvalue")
            )
    })
    
    output$factor_diagram <- renderPlot({
        req(rv$fa_results)
        loadings_matrix <- as.matrix(rv$fa_results$loadings)
        colors <- colorRampPalette(c("#FF9999", "white", "#99FF99"))(100)
        
        par(mar = c(8, 8, 4, 2))
        image(
            1:ncol(loadings_matrix),
            1:nrow(loadings_matrix),
            t(loadings_matrix),
            col = colors,
            xlab = "Factors",
            ylab = "Variables",
            main = "Factor Loadings Heatmap",
            axes = FALSE
        )
        
        axis(1, at = 1:ncol(loadings_matrix), 
             labels = paste0("F", 1:ncol(loadings_matrix)))
        axis(2, at = 1:nrow(loadings_matrix), 
             labels = rownames(loadings_matrix), las = 2)
        
        legend("right", 
               legend = round(seq(-1, 1, length.out = 5), 2),
               fill = colorRampPalette(c("#FF9999", "white", "#99FF99"))(5),
               title = "Loading",
               cex = 0.8)
    })
    
    # Reliability Analysis
    output$reliability_stats <- renderPrint({
        req(rv$data)
        numeric_data <- rv$data[sapply(rv$data, is.numeric)]
        alpha <- calculate_cronbach_alpha(numeric_data)
        cat("Reliability Analysis Results\n")
        cat("==========================\n")
        cat("Cronbach's Alpha:", round(alpha, 3), "\n")
        cat("Number of items:", ncol(numeric_data), "\n")
    })
    
    output$item_stats <- renderDT({
        req(rv$data)
        # Check if there are any numeric columns
        numeric_cols <- sapply(rv$data, is.numeric)
        
        if(sum(numeric_cols) == 0) {
            # If no numeric columns found
            return(datatable(data.frame(Message = "No numeric variables found in the dataset"),
                             options = list(dom = 't')))
        }
        
        numeric_data <- rv$data[, numeric_cols, drop = FALSE]
        
        tryCatch({
            item_stats <- data.frame(
                Variable = names(numeric_data),
                N = apply(numeric_data, 2, function(x) sum(!is.na(x))),
                Mean = round(colMeans(numeric_data, na.rm = TRUE), 3),
                SD = round(apply(numeric_data, 2, sd, na.rm = TRUE), 3),
                Median = round(apply(numeric_data, 2, median, na.rm = TRUE), 3),
                Min = round(apply(numeric_data, 2, min, na.rm = TRUE), 3),
                Max = round(apply(numeric_data, 2, max, na.rm = TRUE), 3),
                Skewness = round(apply(numeric_data, 2, function(x) mean((x - mean(x, na.rm = TRUE))^3, na.rm = TRUE) / 
                                           sd(x, na.rm = TRUE)^3), 3),
                Kurtosis = round(apply(numeric_data, 2, function(x) mean((x - mean(x, na.rm = TRUE))^4, na.rm = TRUE) / 
                                           sd(x, na.rm = TRUE)^4 - 3), 3)
            )
            
            datatable(item_stats, 
                      options = list(scrollX = TRUE, 
                                     pageLength = 15,
                                     dom = 'Bfrtip'),
                      extensions = 'Buttons') %>%
                formatStyle(
                    columns = c('Mean', 'SD', 'Median'),
                    backgroundColor = 'rgba(0, 0, 0, 0.05)'
                )
        }, error = function(e) {
            showNotification(paste("Error in item analysis:", e$message), type = "error")
            return(datatable(data.frame(Error = "Error computing item statistics"),
                             options = list(dom = 't')))
        })
    })
    
    # Download Results
    output$download_results <- downloadHandler(
        filename = function() {
            paste0("analysis_results_", Sys.Date(), ".xlsx")
        },
        content = function(file) {
            wb <- createWorkbook()
            addWorksheet(wb, "Loadings")
            writeData(wb, "Loadings", as.data.frame(rv$fa_results$loadings))
            saveWorkbook(wb, file)
        }
    )
}

shinyApp(ui, server)
