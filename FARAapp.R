library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(openxlsx)
library(plotly)
library(stats)
library(psych)
library(corrplot)
library(factoextra)
library(GPArotation)
library(randomForest)
if (!require(cluster)) install.packages("cluster")

# Custom function for Cronbach's alpha
calculate_cronbach_alpha <- function(data) {
    data <- data[sapply(data, is.numeric)]
    n_items <- ncol(data)
    if (n_items < 2) return(NA)
    cor_matrix <- cor(data, use = "pairwise.complete.obs")
    avg_cor <- (sum(cor_matrix) - n_items) / (n_items * (n_items - 1))
    alpha <- (n_items * avg_cor) / (1 + (n_items - 1) * avg_cor)
    return(alpha)
}

ui <- page_navbar(
    title = "Comprehensive Health & Wellness Analysis Dashboard",
    theme = bs_theme(version = 5, bootswatch = "minty"),
    
    nav_panel("Data Upload & Preview",
              layout_sidebar(
                  sidebar = sidebar(
                      fileInput("file", "Upload CSV or Excel File", accept = c(".xlsx", ".xls", ".csv")),
                      uiOutput("sheet_ui"),
                      hr(),
                      actionButton("refresh", "Refresh Data", class = "btn-primary")
                  ),
                  card(card_header("Data Preview"), DTOutput("data_table"))
              )
    ),
    
    nav_panel("Statistical Summaries",
              card(
                  card_header("Summary Statistics"),
                  DTOutput("summary_stats")
              )
    ),
    
    nav_panel("Exploratory Analysis",
              layout_columns(
                  col_widths = c(6, 6),
                  card(
                      card_header("Correlation Matrix"),
                      plotlyOutput("correlation_plot")
                  ),
                  card(
                      card_header("Variable Distribution"),
                      selectInput("dist_vars", "Select Distribution Variable", choices = NULL, multiple = TRUE),
                      radioButtons("plot_type", "Plot Type", choices = c("Histogram", "Density Plot")),
                      plotlyOutput("distribution_plot")
                  )
              )
    ),
    
    nav_panel("Relationships",
              layout_sidebar(
                  sidebar = sidebar(
                      selectInput("x_var", "Select X Variable", choices = NULL),
                      selectInput("y_var", "Select Y Variable", choices = NULL),
                      checkboxInput("add_reg_line", "Add Regression Line", value = FALSE)
                  ),
                  card(card_header("Scatterplot"), plotlyOutput("scatter_plot"))
              )
    ),
    
    nav_panel("Clustering",
              layout_sidebar(
                  sidebar = sidebar(
                      numericInput("n_clusters", "Number of Clusters", value = 3, min = 2, max = 10),
                      selectInput("clust_method", "Clustering Method", choices = c("kmeans", "hierarchical")),
                      selectInput("cluster_vars", "Variables for Clustering", choices = NULL, multiple = TRUE),
                      actionButton("run_cluster", "Run Clustering", class = "btn-primary")
                  ),
                  layout_columns(
                      col_widths = c(6, 6),
                      card(card_header("Cluster Plot"), plotlyOutput("cluster_plot")),
                      card(card_header("Silhouette Plot"), plotlyOutput("silhouette_plot"))
                  ),
                  card(card_header("Cluster Summary"), DTOutput("cluster_summary"))
              )
    ),
    
    nav_panel("Principal Components",
              layout_sidebar(
                  sidebar = sidebar(
                      selectInput("pca_vars", "Variables for PCA", choices = NULL, multiple = TRUE),
                      numericInput("n_components", "Number of Components", value = 2, min = 1, max = 10),
                      checkboxInput("scale", "Scale Data", value = TRUE),
                      actionButton("run_pca", "Run PCA", class = "btn-primary")
                  ),
                  layout_columns(
                      col_widths = c(6, 6),
                      card(card_header("Scree Plot"), plotlyOutput("pca_scree")),
                      card(card_header("Variance Explained"), DTOutput("pca_summary"))
                  ),
                  layout_columns(
                      col_widths = c(6, 6),
                      card(card_header("Variables Factor Map"), plotlyOutput("pca_var_plot")),
                      card(card_header("Individuals Factor Map"), plotlyOutput("pca_ind_plot"))
                  ),
                  card(card_header("Component Loadings"), DTOutput("pca_loadings"))
              )
    ),
    
    nav_panel("Variable Importance",
              layout_sidebar(
                  sidebar = sidebar(
                      selectInput("target_var", "Select Target Variable", choices = NULL),
                      selectInput("predictor_vars", "Predictor Variables", choices = NULL, multiple = TRUE),
                      actionButton("run_importance", "Calculate Importance", class = "btn-primary")
                  ),
                  card(card_header("Variable Importance Plot"), plotlyOutput("var_importance_plot"))
              )
    ),
    
    nav_panel("Factor Analysis",
              layout_sidebar(
                  sidebar = sidebar(
                      selectInput("fa_vars", "Variables for FA", choices = NULL, multiple = TRUE),
                      numericInput("n_factors", "Number of Factors", value = 3, min = 1, max = 10),
                      selectInput("rotation", "Rotation Method", choices = c("varimax", "promax", "oblimin")),
                      actionButton("run_fa", "Run Analysis", class = "btn-primary")
                  ),
                  layout_columns(
                      col_widths = c(6, 6),
                      card(card_header("Scree Plot"), plotlyOutput("scree_plot")),
                      card(card_header("Factor Loadings"), DTOutput("loadings_table"))
                  ),
                  layout_columns(
                      col_widths = c(6, 6),
                      card(card_header("Factor Diagram"), plotOutput("factor_diagram", height = "350px")),
                      card(card_header("Factor Loadings Heatmap"), plotOutput("fa_heatmap", height = "350px"))
                  )
              )
    ),
    
    nav_panel("VSS Analysis",
              layout_sidebar(
                  sidebar = sidebar(
                      selectInput("vss_vars", "Variables for VSS", choices = NULL, multiple = TRUE),
                      actionButton("run_vss", "Run VSS", class = "btn-primary")
                  ),
                  layout_columns(
                      col_widths = c(6, 6),
                      card(card_header("VSS Plot"), plotOutput("vss_plot")),
                      card(card_header("VSS Output"), verbatimTextOutput("vss_output"))
                  )
              )
    ),
    
    nav_panel("Reliability Analysis",
              layout_columns(
                  col_widths = c(6, 6),
                  card(
                      card_header("Select Variables for Reliability"),
                      selectInput("scale_vars", "Scale Variables", choices = NULL, multiple = TRUE),
                      actionButton("run_reliability", "Run Reliability Analysis", class = "btn-primary")
                  ),
                  card(
                      card_header("Cronbach's Alpha"),
                      verbatimTextOutput("reliability_stats")
                  ),
                  card(
                      card_header("Item Analysis"),
                      DTOutput("item_stats")
                  ),
                  card(
                      card_header("Inter-item Correlations"),
                      plotOutput("item_cors")
                  )
              )
    ),
    
    nav_panel("Download Results",
              card(
                  card_header("Export Data"),
                  downloadButton("download_results", "Download Results")
              )
    )
)

server <- function(input, output, session) {
    rv <- reactiveValues(
        data = NULL,
        fa_results = NULL,
        pca_result = NULL,
        clustering = NULL,
        vss = NULL
    )
    
    # File Upload Logic
    observeEvent(input$file, {
        req(input$file)
        ext <- tools::file_ext(input$file$datapath)
        if (ext == "csv") {
            rv$data <- read.csv(input$file$datapath)
            updateSelectInput(session, "sheet", choices = NULL)
        } else if (ext %in% c("xlsx", "xls")) {
            sheets <- getSheetNames(input$file$datapath)
            updateSelectInput(session, "sheet", choices = sheets)
            rv$data <- read.xlsx(input$file$datapath, sheet = 1)
        }
        updateSelectInput(session, "summary_vars", choices = names(rv$data))
        updateSelectInput(session, "var_select", choices = names(rv$data))
        updateSelectInput(session, "x_var", choices = names(rv$data))
        updateSelectInput(session, "y_var", choices = names(rv$data))
        updateSelectInput(session, "cluster_vars", choices = names(rv$data))
        updateSelectInput(session, "pca_vars", choices = names(rv$data))
        updateSelectInput(session, "fa_vars", choices = names(rv$data))
        updateSelectInput(session, "scale_vars", choices = names(rv$data))
        updateSelectInput(session, "target_var", choices = names(rv$data))
        updateSelectInput(session, "predictor_vars", choices = names(rv$data))
        updateSelectInput(session, "vss_vars", choices = names(rv$data))
    })
    
    # Sheet Selection Logic
    output$sheet_ui <- renderUI({
        req(input$file)
        if (tools::file_ext(input$file$datapath) %in% c("xlsx", "xls")) {
            selectInput("sheet", "Select Sheet", choices = NULL)
        }
    })
    
    observeEvent(input$sheet, {
        req(input$file, input$sheet)
        rv$data <- read.xlsx(input$file$datapath, sheet = input$sheet)
        updateSelectInput(session, "summary_vars", choices = names(rv$data))
        updateSelectInput(session, "var_select", choices = names(rv$data))
        updateSelectInput(session, "x_var", choices = names(rv$data))
        updateSelectInput(session, "y_var", choices = names(rv$data))
        updateSelectInput(session, "cluster_vars", choices = names(rv$data))
        updateSelectInput(session, "pca_vars", choices = names(rv$data))
        updateSelectInput(session, "fa_vars", choices = names(rv$data))
        updateSelectInput(session, "scale_vars", choices = names(rv$data))
        updateSelectInput(session, "target_var", choices = names(rv$data))
        updateSelectInput(session, "predictor_vars", choices = names(rv$data))
        updateSelectInput(session, "vss_vars", choices = names(rv$data))
    })
    
    observeEvent(input$refresh, {
        # Re-trigger file loading
        if (!is.null(input$file)) {
            ext <- tools::file_ext(input$file$datapath)
            if (ext == "csv") {
                rv$data <- read.csv(input$file$datapath)
            } else {
                rv$data <- read.xlsx(input$file$datapath, sheet = if (!is.null(input$sheet)) input$sheet else 1)
            }
            updateSelectInput(session, "summary_vars", choices = names(rv$data))
            updateSelectInput(session, "var_select", choices = names(rv$data))
            updateSelectInput(session, "x_var", choices = names(rv$data))
            updateSelectInput(session, "y_var", choices = names(rv$data))
            updateSelectInput(session, "cluster_vars", choices = names(rv$data))
            updateSelectInput(session, "pca_vars", choices = names(rv$data))
            updateSelectInput(session, "fa_vars", choices = names(rv$data))
            updateSelectInput(session, "scale_vars", choices = names(rv$data))
            updateSelectInput(session, "target_var", choices = names(rv$data))
            updateSelectInput(session, "predictor_vars", choices = names(rv$data))
            updateSelectInput(session, "vss_vars", choices = names(rv$data))
        }
    })
    
    observe({
        req(rv$data)
        num_vars <- names(rv$data)[sapply(rv$data, is.numeric)]
        #updateSelectInput(session, "cor_vars", choices = num_vars, selected = num_vars)
        updateSelectInput(session, "dist_vars", choices = num_vars)
    })
    
    # Data Preview
    output$data_table <- renderDT({
        req(rv$data)
        datatable(rv$data, options = list(scrollX = TRUE))
    })
    
    # Statistical Summaries
    output$summary_stats <- renderDT({
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
    
    # Correlation Matrix
    output$correlation_plot <- renderPlotly({
        req(rv$data)
        numeric_data <- rv$data[sapply(rv$data, is.numeric)]
        
        # Defensive: need at least 2 numeric columns for a correlation matrix
        if (ncol(numeric_data) < 2) {
            return(plotly_empty(type = "heatmap") %>%
                       layout(title = "Need at least two numeric variables for correlation matrix"))
        }
        
        corr <- cor(numeric_data, use = "pairwise.complete.obs")
        plot_ly(
            z = corr,
            x = colnames(corr),
            y = colnames(corr),
            type = "heatmap",
            colorscale = "Viridis"
        ) %>%
            layout(
                title = "Correlation Matrix",
                xaxis = list(title = ""),
                yaxis = list(title = "")
            )
    })
    
    # Variable Distribution
    output$distribution_plot <- renderPlotly({
        req(rv$data, input$dist_vars)
        selected <- input$dist_vars
        if (length(selected) == 0) {
            return(plotly_empty(type = "scatter") %>% layout(title = "Select at least one variable"))
        }
        df <- rv$data
        # Filter to selected numeric columns and drop NA rows
        plot_data <- df[, selected, drop = FALSE]
        plot_data <- na.omit(plot_data)
        plt <- plot_ly()
        for (var in selected) {
            if (input$plot_type == "Histogram") {
                plt <- plt %>% add_histogram(x = plot_data[[var]], name = var, opacity = 0.5)
            } else {
                dens <- density(plot_data[[var]], na.rm = TRUE)
                plt <- plt %>% add_lines(x = dens$x, y = dens$y, name = var)
            }
        }
        plt <- plt %>% layout(barmode = "overlay", title = "Variable Distribution")
        plt
    })
    
    # Relationships - Scatterplot
    output$scatter_plot <- renderPlotly({
        req(rv$data, input$x_var, input$y_var)
        plot_data <- rv$data
        x <- plot_data[[input$x_var]]
        y <- plot_data[[input$y_var]]
        p <- plot_ly(x = x, y = y, type = "scatter", mode = "markers", name = "Data Points")
        
        if (input$add_reg_line) {
            valid <- complete.cases(x, y)
            x_clean <- x[valid]
            y_clean <- y[valid]
            fit <- lm(y_clean ~ x_clean)
            x_seq <- seq(min(x_clean, na.rm = TRUE), max(x_clean, na.rm = TRUE), length.out = 100)
            y_pred <- predict(fit, newdata = data.frame(x_clean = x_seq))
            p <- p %>%
                add_lines(x = x_seq, y = y_pred, line = list(color = 'red'), name = "Regression Line")
        }
        p %>% layout(title = paste("Scatterplot of", input$y_var, "vs", input$x_var),
                     xaxis = list(title = input$x_var),
                     yaxis = list(title = input$y_var))
    })
    
    # Clustering
    observeEvent(input$run_cluster, {
        req(rv$data, input$cluster_vars)
        
        # Defensive check for at least two variables
        if (length(input$cluster_vars) < 2) {
            showNotification("Please select at least two variables for clustering.", type = "error")
            return()
        }
        
        cluster_data <- rv$data[, input$cluster_vars, drop = FALSE]
        if (!all(sapply(cluster_data, is.numeric))) {
            showNotification("Only numeric variables can be used for clustering.", type = "error")
            return()
        }
        orig_n <- nrow(cluster_data)
        cluster_data <- na.omit(cluster_data)
        if (nrow(cluster_data) < orig_n) {
            showNotification("Rows with missing values were removed.", type = "warning")
        }
        if (nrow(cluster_data) < input$n_clusters) {
            showNotification("Not enough data points for the number of clusters.", type = "error")
            return()
        }
        scaled_data <- scale(cluster_data)
        
        if (input$clust_method == "kmeans") {
            set.seed(123)
            clust <- tryCatch({
                kmeans(scaled_data, centers = input$n_clusters)
            }, error = function(e) {
                showNotification(paste("K-means error:", e$message), type = "error")
                return(NULL)
            })
            if (is.null(clust)) return()
            cluster_assign <- clust$cluster
            cluster_summary <- aggregate(cluster_data, by = list(Cluster = cluster_assign), mean)
            output$cluster_summary <- renderDT({
                datatable(cluster_summary, options = list(scrollX = TRUE))
            })
            output$cluster_plot <- renderPlotly({
                if (ncol(scaled_data) >= 2) {
                    plot_ly(
                        x = scaled_data[, 1],
                        y = scaled_data[, 2],
                        color = as.factor(cluster_assign),
                        type = "scatter", mode = "markers", marker = list(size = 10),
                        text = paste("Cluster:", cluster_assign)
                    ) %>%
                        layout(
                            xaxis = list(title = colnames(scaled_data)[1]),
                            yaxis = list(title = colnames(scaled_data)[2]),
                            title = "K-Means Cluster Plot"
                        )
                } else {
                    plotly_empty(type = "scatter", mode = "markers") %>%
                        layout(title = "Need at least two variables for cluster plot")
                }
            })
            output$silhouette_plot <- renderPlotly({
                if (ncol(scaled_data) >= 2 && length(unique(cluster_assign)) > 1) {
                    sil <- tryCatch(
                        cluster::silhouette(cluster_assign, dist(scaled_data)),
                        error = function(e) NULL
                    )
                    if (!is.null(sil)) {
                        sil_df <- data.frame(cluster = sil[, 1], silhouette = sil[, 3])
                        plot_ly(sil_df, y = ~silhouette, color = ~as.factor(cluster), type = "box")
                    } else {
                        plotly_empty(type = "box") %>%
                            layout(title = "Silhouette plot failed (possibly too few points per cluster)")
                    }
                } else {
                    plotly_empty(type = "box") %>%
                        layout(title = "Silhouette plot requires at least two clusters and two variables")
                }
            })
        } else if (input$clust_method == "hierarchical") {
            dist_mat <- dist(scaled_data)
            hc <- hclust(dist_mat)
            cluster_assign <- cutree(hc, k = input$n_clusters)
            cluster_summary <- aggregate(cluster_data, by = list(Cluster = cluster_assign), mean)
            output$cluster_summary <- renderDT({
                datatable(cluster_summary, options = list(scrollX = TRUE))
            })
            output$cluster_plot <- renderPlotly({
                dend <- factoextra::fviz_dend(hc, k = input$n_clusters)
                ggplotly(dend)
            })
            output$silhouette_plot <- renderPlotly({
                if (ncol(scaled_data) >= 2 && length(unique(cluster_assign)) > 1) {
                    sil <- tryCatch(
                        cluster::silhouette(cluster_assign, dist(scaled_data)),
                        error = function(e) NULL
                    )
                    if (!is.null(sil)) {
                        sil_df <- data.frame(cluster = sil[, 1], silhouette = sil[, 3])
                        plot_ly(sil_df, y = ~silhouette, color = ~as.factor(cluster), type = "box")
                    } else {
                        plotly_empty(type = "box") %>%
                            layout(title = "Silhouette plot failed (possibly too few points per cluster)")
                    }
                } else {
                    plotly_empty(type = "box") %>%
                        layout(title = "Silhouette plot requires at least two clusters and two variables")
                }
            })
        }
    })
    
    
    # PCA
    observeEvent(input$run_pca, {
        req(rv$data, input$pca_vars)
        pca_data <- rv$data[, input$pca_vars, drop = FALSE]
        if (input$scale) pca_data <- scale(pca_data)
        pca_result <- prcomp(pca_data, scale. = input$scale)
        rv$pca_result <- pca_result
        
        output$pca_scree <- renderPlotly({
            eig <- pca_result$sdev^2
            plot_ly(x = 1:length(eig), y = eig, type = "scatter", mode = "lines+markers",
                    name = "Eigenvalues") %>%
                layout(title = "Scree Plot", xaxis = list(title = "Component"), yaxis = list(title = "Eigenvalue"))
        })
        
        output$pca_summary <- renderDT({
            var_explained <- summary(pca_result)$importance[2, 1:input$n_components]
            cum_var <- summary(pca_result)$importance[3, 1:input$n_components]
            df <- data.frame(
                Component = paste0("PC", 1:input$n_components),
                Variance_Explained = round(var_explained, 3),
                Cumulative_Variance = round(cum_var, 3)
            )
            datatable(df, options = list(scrollX = TRUE))
        })
        
        output$pca_var_plot <- renderPlotly({
            fviz <- fviz_pca_var(pca_result, col.var = "contrib", repel = TRUE, gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
            ggplotly(fviz)
        })
        
        output$pca_ind_plot <- renderPlotly({
            fviz <- fviz_pca_ind(pca_result, col.ind = "cos2", repel = TRUE, gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
            ggplotly(fviz)
        })
        
        output$pca_loadings <- renderDT({
            loadings <- pca_result$rotation[, 1:input$n_components, drop = FALSE]
            datatable(round(loadings, 3), options = list(scrollX = TRUE))
        })
    })
    
    
    # Variable Importance
    observeEvent(input$run_importance, {
        req(rv$data, input$target_var, input$predictor_vars)
        target <- input$target_var
        predictors <- input$predictor_vars
        data_rf <- rv$data[, c(target, predictors), drop = FALSE]
        data_rf <- na.omit(data_rf)
        data_rf[[target]] <- as.factor(data_rf[[target]])
        model <- randomForest::randomForest(as.formula(paste(target, "~ .")), data = data_rf, importance = TRUE)
        importance <- randomForest::importance(model)
        importance_df <- data.frame(
            Variable = rownames(importance),
            MeanDecreaseAccuracy = importance[, "MeanDecreaseAccuracy"]
        )
        output$var_importance_plot <- renderPlotly({
            plot_ly(data = importance_df, x = ~reorder(Variable, MeanDecreaseAccuracy), y = ~MeanDecreaseAccuracy,
                    type = "bar", orientation = "h") %>%
                layout(title = "Variable Importance", xaxis = list(title = "Variable"), yaxis = list(title = "Mean Decrease in Accuracy"))
        })
    })
    
    # Factor Analysis
    observeEvent(input$run_fa, {
        req(rv$data, input$fa_vars)
        fa_data <- rv$data[, input$fa_vars, drop = FALSE]
        fa_data <- scale(fa_data)
        num_vars <- ncol(fa_data)
        
        if (num_vars < 2) {
            showNotification("Select at least two variables for factor analysis.", type = "error")
            return()
        }
        if (input$n_factors > num_vars) {
            showNotification("Number of factors cannot be greater than number of variables.", type = "error")
            return()
        }
        
        rv$vss <- VSS(fa_data)
        fa_result <- fa(fa_data, nfactors = input$n_factors, rotate = input$rotation, fm = "ml")
        rv$fa_results <- fa_result
        
        output$scree_plot <- renderPlotly({
            req(rv$fa_results)
            eigenvalues <- rv$fa_results$values
            if (is.null(eigenvalues) || length(eigenvalues) == 0) {
                showNotification("Scree plot cannot be displayed: no eigenvalues calculated. Try reducing the number of factors or check your data.", type = "error")
                return(plotly_empty() %>% layout(title = "Unable to calculate eigenvalues for scree plot"))
            }
            data <- data.frame(
                Factor = 1:length(eigenvalues),
                Eigenvalue = eigenvalues,
                Kaiser = rep(1, length(eigenvalues))
            )
            total_var <- sum(eigenvalues)
            cum_var <- cumsum(eigenvalues / total_var) * 100
            plot_ly() %>%
                add_trace(data = data, x = ~Factor, y = ~Eigenvalue, 
                          type = "scatter", mode = "lines+markers",
                          name = "Eigenvalues",
                          line = list(color = "blue"),
                          marker = list(color = "blue", size = 8)) %>%
                add_trace(data = data, x = ~Factor, y = ~Kaiser,
                          type = "scatter", mode = "lines",
                          name = "Kaiser criterion (>1)",
                          line = list(color = "red", dash = "dash")) %>%
                add_trace(x = ~Factor, y = cum_var, 
                          type = "scatter", mode = "lines+markers",
                          name = "Cumulative variance %",
                          yaxis = "y2",
                          line = list(color = "green"),
                          marker = list(color = "green", size = 8)) %>%
                layout(
                    title = "Scree Plot with Kaiser Criterion",
                    xaxis = list(title = "Factor Number"),
                    yaxis = list(title = "Eigenvalue", side = "left"),
                    yaxis2 = list(title = "Cumulative Variance Explained (%)",
                                  side = "right", overlaying = "y", range = c(0, 100)),
                    showlegend = TRUE,
                    legend = list(x = 0.7, y = 0.9),
                    hovermode = "x"
                )
        })
        
        output$loadings_table <- renderDT({
            req(rv$fa_results)
            loadings <- as.data.frame(unclass(rv$fa_results$loadings))
            # Defensive: check for empty loadings
            if (is.null(loadings) || nrow(loadings) == 0 || ncol(loadings) == 0) {
                return(datatable(data.frame(Message = "No loadings available to display. Check variable selection and number of factors."),
                                 options = list(dom = 't')))
            }
            colnames(loadings) <- paste0("Factor", 1:ncol(loadings))
            # Only add if length matches
            if (!is.null(rv$fa_results$communality) && length(rv$fa_results$communality) == nrow(loadings)) {
                loadings$Communality <- rv$fa_results$communality
            }
            if (!is.null(rv$fa_results$uniquenesses) && length(rv$fa_results$uniquenesses) == nrow(loadings)) {
                loadings$Uniqueness <- rv$fa_results$uniquenesses
            }
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
        
        output$factor_diagram <- renderPlot({
            fa.diagram(fa_result)
        })
        
        output$fa_heatmap <- renderPlot({
            loadings_matrix <- as.matrix(rv$fa_results$loadings)
            colors <- colorRampPalette(c("#FF9999", "white", "#99FF99"))(100)
            par(mar = c(4, 4, 2, 2))
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
    })
    
    # VSS Analysis
    observeEvent(input$run_vss, {
        req(rv$data, input$vss_vars)
        vss_data <- rv$data[, input$vss_vars, drop = FALSE]
        vss_data <- scale(vss_data)
        if (ncol(vss_data) < 2) {
            showNotification("Select at least two variables for VSS analysis.", type = "error")
            rv$vss <- NULL
            return()
        }
        rv$vss <- VSS(vss_data)
        output$vss_plot <- renderPlot({
            req(rv$vss)
            plot(rv$vss)
        })
        output$vss_output <- renderPrint({
            req(rv$vss)
            print(rv$vss)
        })
    })
    
    # Reliability Analysis
    observeEvent(input$run_reliability, {
        req(rv$data, input$scale_vars)
        scale_data <- rv$data[, input$scale_vars, drop = FALSE]
        output$reliability_stats <- renderPrint({
            if (ncol(scale_data) < 2) {
                cat("Error: Need at least 2 numeric variables for reliability analysis\n")
                return()
            }
            alpha <- calculate_cronbach_alpha(scale_data)
            n_items <- ncol(scale_data)
            n_cases <- nrow(scale_data)
            cat("Reliability Analysis Results\n")
            cat("==========================\n")
            cat("Number of items:", n_items, "\n")
            cat("Number of cases:", n_cases, "\n")
            cat("\nReliability Statistics:\n")
            cat("Cronbach's Alpha:", round(alpha, 3), "\n")
            cat("Standardized Alpha:", round(alpha, 3), "\n\n")
            scale_mean <- mean(rowSums(scale_data, na.rm = TRUE), na.rm = TRUE)
            scale_sd <- sd(rowSums(scale_data, na.rm = TRUE), na.rm = TRUE)
            cat("Scale Statistics:\n")
            cat("Mean:", round(scale_mean, 2), "\n")
            cat("Standard Deviation:", round(scale_sd, 2), "\n")
            cat("Variance:", round(scale_sd^2, 2), "\n")
        })
        
        output$item_stats <- renderDT({
            numeric_data <- scale_data
            item_stats <- data.frame(
                Variable = names(numeric_data),
                N = apply(numeric_data, 2, function(x) sum(!is.na(x))),
                Mean = round(colMeans(numeric_data, na.rm = TRUE), 3),
                SD = round(apply(numeric_data, 2, sd, na.rm = TRUE), 3),
                Median = round(apply(numeric_data, 2, median, na.rm = TRUE), 3),
                Min = round(apply(numeric_data, 2, min, na.rm = TRUE), 3),
                Max = round(apply(numeric_data, 2, max, na.rm = TRUE), 3),
                Skewness = round(apply(numeric_data, 2, function(x) mean((x - mean(x, na.rm = TRUE))^3, na.rm = TRUE) / sd(x, na.rm = TRUE)^3), 3),
                Kurtosis = round(apply(numeric_data, 2, function(x) mean((x - mean(x, na.rm = TRUE))^4, na.rm = TRUE) / sd(x, na.rm = TRUE)^4 - 3), 3)
            )
            datatable(item_stats, options = list(scrollX = TRUE, pageLength = 15, dom = 'Bfrtip'), extensions = 'Buttons')
        })
        
        output$item_cors <- renderPlot({
            item_cors <- cor(scale_data, use = "pairwise.complete.obs")
            corrplot(item_cors, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
        })
    })
    
    # Download Results
    output$download_results <- downloadHandler(
        filename = function() {
            paste0("analysis_results_", Sys.Date(), ".xlsx")
        },
        content = function(file) {
            wb <- createWorkbook()
            if (!is.null(rv$fa_results)) {
                addWorksheet(wb, "FA_Loadings")
                writeData(wb, "FA_Loadings", as.data.frame(unclass(rv$fa_results$loadings)))
            }
            if (!is.null(rv$pca_result)) {
                addWorksheet(wb, "PCA_Loadings")
                writeData(wb, "PCA_Loadings", as.data.frame(rv$pca_result$rotation))
            }
            if (!is.null(rv$data)) {
                addWorksheet(wb, "Data")
                writeData(wb, "Data", rv$data)
            }
            saveWorkbook(wb, file, overwrite = TRUE)
        }
    )
}

shinyApp(ui, server)
