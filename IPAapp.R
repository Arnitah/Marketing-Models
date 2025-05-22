# ------------------------------------------------------
# Load Required Packages
# ------------------------------------------------------

library(shiny)
library(readxl)
library(ggplot2)
library(tidyverse)
library(ggrepel)
library(RColorBrewer)
library(DT)
library(extrafont)
library(scales)


# ------------------------------------------------------
# Define UI
# ------------------------------------------------------

# UI for the Shiny app
ui <- fluidPage(
  
  # App title
  titlePanel("Importance-Performance Analysis (IPA)"),
  
  # Sidebar layout with input and output
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      fileInput("file1", "Choose First Excel File", accept = ".xlsx"),
      fileInput("file2", "Choose Second Excel File (Optional for Comparison)", accept = ".xlsx"),
      actionButton("processData", "Process Data"), # Process Data button
      hr(),
      helpText("Upload your IPA data and click 'Process Data' to display plots and tables."),
      sliderInput("zoom", "Zoom Level:", min = 0.5, max = 2, value = 1, step = 0.1),  # Zoom slider
      sliderInput("fontSize", "Font Size:", min = 10, max = 30, value = 14),  # Font size slider
      sliderInput("labelSize", "Label Size:", min = 3, max = 10, value = 5),  # Slider for label size
      selectInput("xvar", "Select X Variable:", choices = c("Importance", "Performance")),
      selectInput("colorVar", "Select Color Variable:", choices = NULL),  # Populated dynamically
      radioButtons("plot_type", "Choose Plot Type:",
                   choices = list("Line Plot" = "line", "Area Plot" = "area"),
                   selected = "line"),
      numericInput("res", "Resolution (DPI):", value = 300, min = 100, max = 600, step = 50),
      actionButton("update", "Update"),
      downloadButton('downloadPlot', 'Download Plot'),
      downloadButton("report", "Download Summary Report"),
      downloadButton("downloadPDF", "Download PDF")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      tabsetPanel(
        tabPanel("Introduction", 
                 htmlOutput("introductionText")),
        tabPanel("Survey Questions", 
                 DTOutput("surveyTable")),
        tabPanel("Importance-Performance Analysis Graph",
                 fluidRow(
                   column(12,h3("Steps for Importance-Performance Analysis"),
                 p("1. Mean Calculation: Frequency analysis includes the calculation of means (average values), which indicate central tendencies in the data."),
                 p("2. Normalization: After obtaining means, normalization is used to scale values for comparison across different categories, ensuring that each variable is measured on the same scale."),
                 p("3. Standard Deviation (STD): This measures the spread of data points. A high STD suggests wide variability, while a low STD indicates that the data points are clustered closely around the mean."),
                 p("4. Normalized Scores: When normalized, positive values suggest a higher-than-average score, while negative values indicate lower-than-average performance or importance."),
                 p("5. Importance-Performance Relationship: By comparing importance and performance, decision-makers can identify areas needing improvement or where the university is excelling."),
                 p("6. Importance-Performance Analysis Plot: This graph visualises the normalized scores of importance and performance for us to carry out further analysis.")),
                   column(12, plotOutput("scatterPlotDiagonal", height = "700px", width = "100%")))),
        
        tabPanel("Quadrant Explanation and Analysis", 
                 fluidRow(
                   column(12, htmlOutput("quadrantExplanation")), 
                   column(12, plotOutput("boxPlotQuadrant")),
                   column(12, DTOutput("quadrantTable")))),
        
        tabPanel("45-Degree Line Explanation and Analysis", 
                 fluidRow(
                   column(12, htmlOutput("lineOfEqualityExplanation")), 
                   column(12, plotOutput("fixingPlot")), 
                   column(12, DTOutput("fixingTable")))),
                 tabPanel("Satisfaction Explanation and Analysis", 
                          fluidRow(
                            column(12, htmlOutput("satisfactionExplanation")), 
                            column(12, plotOutput("weightedSatisfactionPlot", height = "700px", width = "1000px")), 
                            column(12, plotOutput("satisfactionGapPlot", height = "700px", width = "1000px")),
                            )),
        
                          tabPanel("Regional Comparison Analysis",
                          fluidRow(
                            column(12, htmlOutput("regionalComparisonAnalysis")),
                            column(12, plotOutput("comparisonPlot", height = "700px", width = "1000px")), 
                            column(12, plotOutput("ipaPlot", height = "700px", width = "1000px")))),
                            
                          tabPanel("Conclusion", 
                                   fluidRow(
                                     column(12, htmlOutput("conclusion"))),
                                     column(12, DTOutput("comparison_table")))
                          )
                 )
        )
      )
    
   


# ------------------------------------------------------
# Define Server Logic
# ------------------------------------------------------

# Server logic for the Shiny app
server <- function(input, output) {
  
  output$introductionText <- renderUI({
    HTML(
      "<h3>Introduction to Importance-Performance Analysis (IPA)</h3>
      <p>Importance-Performance Analysis (IPA) is a powerful strategic tool that helps organizations evaluate how well they are meeting the needs and expectations of their stakeholders. In the context of higher education, IPA is particularly valuable for assessing the importance of various institutional services—such as academic programs, campus facilities, and student support services—against their actual performance.</p>
      
      <p>The main goal of IPA is to identify areas of strength and weakness within an institution's offerings. By plotting services on a two-dimensional grid, with importance on the x-axis and performance on the y-axis, educators and administrators can easily visualize where their efforts should be prioritized to enhance stakeholder satisfaction.</p>
      
      <h4>Analysis Overview</h4>
      <p>This particular analysis is based on feedback from parents of Columbus State University students. It provides valuable insights into how well the university is meeting parental expectations and highlights areas where improvements can be made. By identifying key priorities through parent feedback, Columbus State University can better align its strategies to serve both students and their families, ultimately improving satisfaction and fostering stronger engagement.</p>
      
      <h4>Data Collection Method</h4>
      <p>For this analysis, a structured survey was distributed to a targeted group of parents to collect their perceptions, opinions, and experiences regarding CSU's services. The survey included a combination of closed-ended and Likert scale questions, enabling both quantitative data (e.g., satisfaction ratings) and qualitative feedback (e.g., suggestions for improvement). The responses were analyzed using statistical tools to uncover trends and patterns that will inform future institutional strategies.</p>
      
      <h4>Frequency Analysis</h4>
      <p>Frequency Analysis was conducted to understand how often specific responses or categories appeared in the dataset. Tools such as IBM SPSS were used to generate counts and percentages for each response, helping to identify data distribution, spot trends, and detect patterns within the feedback. This process helps guide CSU in making informed decisions based on parent insights."
    )
  })
  
  # Quadrant Explanation Write-up
  output$quadrantExplanation <- renderUI({
    HTML(
      "<h3>Understanding the Importance-Performance Quadrants</h3>
      <p>In the Importance-Performance Analysis (IPA), services or products are categorized into four quadrants based on how important they are to customers and how well they perform.</p>
      
      <ul>
        <li><strong>Keep Up the Good Work (Top-Left Quadrant)</strong>: These are the services that are both important and performing well. You should continue focusing on these areas.</li>
        <li><strong>Concentrate Here (Top-Right Quadrant)</strong>: These services are important but are currently underperforming. These areas need the most attention and improvement.</li>
        <li><strong>Low Priority (Bottom-Left Quadrant)</strong>: These services are less important and have lower performance. They don't require immediate attention and can be improved over time.</li>
        <li><strong>Possible Overkill (Bottom-Right Quadrant)</strong>: These services perform well but are not as important to customers. You may be putting too much effort here.</li>
      </ul>
      <p>This analysis helps identify where resources should be focused to improve customer satisfaction effectively.</p>"
    )
  })
  
  # Satisfaction Explanation Write-up
  output$satisfactionExplanation <- renderUI({
    HTML(
      "<h3>Understanding Satisfaction in Importance-Performance Analysis</h3>
      <p>Satisfaction is a critical measure that evaluates how well an organization meets the expectations of its stakeholders. In Importance-Performance Analysis (IPA), we calculate satisfaction based on two main factors: Importance and Performance.</p>
      
      <ul>
        <li><strong>Importance</strong>: This indicates how crucial a service or attribute is to customers. It is assessed through surveys where respondents rate the significance of different factors.</li>
        <li><strong>Performance</strong>: This measures how effectively an organization delivers those services, evaluated through customer ratings of their satisfaction.</li>
      </ul>
      
      <p>The Satisfaction variable is calculated using the following formulas:</p>
      
      <ul>
        <li><strong>Weighted Satisfaction</strong>: This represents overall satisfaction, weighted by the importance of each service.</li>
        <li><strong>Satisfaction Gap</strong>: This reflects the difference between performance and importance, normalized to show how well services meet expectations.</li>
      </ul>
      
      <p>By analyzing these satisfaction metrics, organizations can identify strengths and areas for improvement, ultimately enhancing customer satisfaction and loyalty.</p>"
    )
  })
  
  
  # 45-Degree Line Explanation Write-up
  output$lineOfEqualityExplanation <- renderUI({
    HTML(
      "<h3>The 45-Degree Line of Equality in Importance-Performance Analysis</h3>
      <p>The 45-degree line of equality is a pivotal reference in Importance-Performance Analysis (IPA). It represents a balance between the importance of a service and its performance.</p>
      
      <h4>Understanding the Line</h4>
      <p>This line divides the IPA grid into two sections. Services above this line are performing better than expected, while those below are underperforming.</p>
      
      <h4>Interpreting the Quadrants</h4>
      <ul>
        <li><strong>Quadrant I (High Importance, High Performance)</strong>: Services that exceed expectations and are critical to stakeholders.</li>
        <li><strong>Quadrant II (High Importance, Low Performance)</strong>: Essential services that need improvement.</li>
        <li><strong>Quadrant III (Low Importance, Low Performance)</strong>: Lesser critical services that are also underperforming; they may require reevaluation.</li>
        <li><strong>Quadrant IV (Low Importance, High Performance)</strong>: Services that perform well but are less important; these can be deprioritized.</li>
      </ul>
      
      <p>By analyzing the placement of services relative to the line of equality, organizations can identify areas that require focus and allocate resources effectively to enhance stakeholder satisfaction.</p>"
    )
  })
  
  
  output$regionalComparisonAnalysis <- renderUI({
    HTML(
      "<h3>Regional IPA Comparison: Columbus State University vs. Other Georgia Universities</h3>
    <p>The Regional Comparison Analysis aims to identify variations in satisfaction levels between Columbus State University (CSU) and other prominent universities within the Georgia's higher education landscape. By analyzing and benchmarking performance and satisfaction metrics across institutions, we aim to identify areas where CSU can enhance its offerings and overall satisfaction levels, particularly as perceived by students and parents.</p>
   
    <p>This analysis is crucial for understanding how CSU performs relative to regional competitors and allows us to pinpoint strengths and weaknesses in its educational and service offerings. For example, if certain universities in Georgia are excelling in areas where CSU shows lower satisfaction or performance, these success strategies can be assessed and potentially adapted to CSU’s context.</p>
       
    <p>By drawing comparisons in critical areas such as academic support, facilities, student engagement, and career readiness, CSU can develop targeted initiatives that address gaps in student and parent satisfaction. The findings from this analysis will assist the institution in refining its competitive positioning and aligning more closely with the expectations and needs of its key stakeholders—students and parents alike.</p>
  
  <p>
    Finally, these insights provide actionable data to inform decision-making on how to improve student experiences and satisfaction. Furthermore, the regional comparison allows CSU to explore areas where peer institutions are outperforming and determine whether similar approaches could be adopted to enhance its standing and reputation within the higher education landscape of Georgia. </p>"
      )
   })
  
  
  output$conclusion <- renderUI({
    HTML(
      "<h3>Conclusion</h3>
    <p>In conclusion, the Importance-Performance Analysis (IPA) and Regional Comparison Analysis provide a comprehensive framework for understanding parents' satisfaction within Columbus State Univeristy (CSU). The IPA graph serves as a strategic tool to evaluate how well services meet parents' expectations, identifying strengths to maintain and weaknesses to improve.</p>

    <p>The Regional Comparison Analysis complements this by highlighting geographic disparities in satisfaction levels, guiding universities to tailor their strategies based on specific needs. By leveraging these insights, universities can prioritize their efforts effectively, allocate resources strategically, and enhance overall parents engagement.</p>

    <p>Together, these analyses not only inform decision-making but also foster a culture of continuous improvement and responsiveness to parents' feedback. By addressing both importance-performance relationships and regional variations, univerisities can work toward achieving higher levels of satisfaction, ensuring they meet the diverse needs of parents effectively. This holistic approach positions them to enhance their reputation, retain parents' loyalty, and ultimately drive better university outcomes.</p>"
    )
  })
  
  survey_data <- data.frame(
    Question = c(
      "The college has a very good academic reputation",
      "The college's graduates get good jobs",
      "The college graduates a high percentage of students",
      "The cost of attending the college is a good value",
      "The college has a good reputation for its social activities",
      "The college is the right size for me",
      "The college is in a good location for me",
      " My parents are okay with me attending the college",
      "The college is highly regarded by my teachers/guidance counselor",
      "The college has the type of major I want",
      "I have friends attending the college",
      "The best academic students in my high school attend the college"
      
      # Add more questions as needed
    ),
    Response_Type = c("Likert Scale", "Likert Scale", "Likert Scale", "Likert Scale", "Likert Scale", "Likert Scale", "Likert Scale", "Likert Scale", "Likert Scale", "Likert Scale", "Likert Scale", "Likert Scale")
  )
  
  # ------------------------------------------------------
  #  Processing Logic
  # ------------------------------------------------------ 
  # Reactive value to store whether data processing has been triggered
  processed <- reactiveVal(FALSE)
  
  # Reactive expression to read in the data based on user input
  df1 <- reactive({
    req(input$file1)
    read_excel(input$file1$datapath)
  })
  
  # Reactive expression to read in the second file
  df2 <- reactive({
    if (is.null(input$file2)) return(NULL)  # Ensure df2 is optional
    read_excel(input$file2$datapath)
  })
  
 
  # ------------------------------------------------------
  # Data Processing
  # ------------------------------------------------------ 
  # Process and calculate normalized scores and quadrants, only when button is clicked
  observeEvent(input$processData, {
    processed(TRUE)  # Set processed to TRUE when the button is clicked
  })
  
  df1_normalized <- reactive({
    req(df1(), processed())  # Only process data if the button is clicked (processed is TRUE)
    
    df1() %>%
      mutate(
        Imp_Normalized_Score = round((Importance - mean(Importance, na.rm = TRUE)) / sd(Importance, na.rm = TRUE),2),
        Perf_Normalized_Score = round((Performance - mean(Performance, na.rm = TRUE)) / sd(Performance, na.rm = TRUE),2)
      ) %>%
      mutate(Quadrant = case_when(
        Imp_Normalized_Score >= 0 & Perf_Normalized_Score >= 0 ~ "High Importance / High Performance",
        Imp_Normalized_Score >= 0 & Perf_Normalized_Score < 0 ~ "High Importance / Low Performance",
        Imp_Normalized_Score < 0 & Perf_Normalized_Score >= 0 ~ "Low Importance / High Performance",
        TRUE ~ "Low Importance / Low Performance"
      )) %>%
      mutate(
        Fix_Needed = ifelse(Imp_Normalized_Score < Perf_Normalized_Score, "No Fixing Needed", "Needs Fixing"),
        Abbreviated_Quadrant = factor(Quadrant,
                                      levels = c("High Importance / High Performance",
                                                 "High Importance / Low Performance",
                                                 "Low Importance / High Performance",
                                                 "Low Importance / Low Performance"),
                                      labels = c("HI/HP", "HI/LP", "LI/HP", "LI/LP")),
        Weighted_Satisfaction = Performance * Importance,  # New column for Weighted Satisfaction
        Satisfaction_Gap = (Performance - Importance) / Importance  # New column for Satisfaction Gap
      ) %>%
      mutate(Abbreviated_Variable_Names = str_wrap(Abbreviated_Variable_Names, width = 10))
  })
  

    
  # Cleaning df2 and creating additional plots
  df2_cleaned <- reactive({
    req(df2())  # Ensure df2 is available
    df2() %>%
          mutate(
            Imp_Normalized_Score = (Importance - mean(Importance, na.rm = TRUE)) / sd(Importance, na.rm = TRUE),
            Perf_Normalized_Score = (Performance - mean(Performance, na.rm = TRUE)) / sd(Performance, na.rm = TRUE)
          )
    
  })

    
  # Combined data for comparison
  combined_df <- reactive({
    req(df1_normalized())  # Ensure df1 is processed
    if (is.null(df2_cleaned())) return(NULL)  # Only combine if df2 is available
    
    bind_rows(
      df1_normalized() %>% mutate(University = "Columbus State University"),  # liable to change
      df2_cleaned() %>% mutate(University = "Georgia Southern University") # liable to change
    ) %>%
      mutate(Abbrev_Labels = abbreviate(Abbreviated_Variable_Names)
             )
    
  })
 
 
  # Prepare a reactive expression for facet data
  facet_data <- reactive({
    req(combined_df())  # Ensure combined_df is available
    
    combined_df() %>%  # Ensure combined_df() is called as a reactive function
      select(University, Imp_Normalized_Score, Perf_Normalized_Score, Abbreviated_Variable_Names) %>%  # Use 'Abbrev_Labels'
      gather(key = "Metric", value = "Score", Imp_Normalized_Score, Perf_Normalized_Score)
    
  })
 
  
  # Create a reactive expression for the comparison data
  df_comparison <- reactive({
    req(combined_df())  # Ensure combined_df is available
    
    combined_df() %>%
      mutate(
        Performance_Category = case_when(
          Perf_Normalized_Score > 0.67 ~ "High",     
          Perf_Normalized_Score >= 0.34 & Perf_Normalized_Score <= 0.67 ~ "Medium",  
          Perf_Normalized_Score < 0.34 ~ "Low"
        ),
        Importance_Category = case_when(
          Imp_Normalized_Score > 0.67 ~ "High",     
          Imp_Normalized_Score >= 0.34 & Imp_Normalized_Score <= 0.67 ~ "Medium",  
          Imp_Normalized_Score < 0.34 ~ "Low"
        )
      ) %>%
      group_by(University) %>%
      summarize(
        Good_Jobs = first(Performance_Category),
        Cost_of_Attendance = first(Performance_Category),
        Academic_Reputation = first(Performance_Category),
        Has_Major = first(Importance_Category),
        .groups = "drop"
      ) %>%
      mutate(
        Cost_of_Attendance = case_when(
          Cost_of_Attendance == "High" ~ "Expensive",
          Cost_of_Attendance == "Medium" ~ "Moderate",
          Cost_of_Attendance == "Low" ~ "Affordable",
          TRUE ~ Cost_of_Attendance
        ),
        Academic_Reputation = case_when(
          Academic_Reputation == "High" ~ "Established",
          Academic_Reputation == "Medium" ~ "Emerging",
          Academic_Reputation == "Low" ~ "Developing",
          TRUE ~ Academic_Reputation
        ),
        Good_Jobs = case_when(
          Good_Jobs == "High" ~ "Exceptional",
          Good_Jobs == "Medium" ~ "Moderate",
          Good_Jobs == "Low" ~ "Average",
          TRUE ~ Good_Jobs
        ),
        Has_Major = case_when(
          Has_Major == "High" ~ "Strong",
          Has_Major == "Medium" ~ "Medium",
          Has_Major == "Low" ~ "Limited",
          TRUE ~ Has_Major
        )
      ) %>%
      arrange(University) %>%  # Sort by University name
      distinct()               # Ensure no duplicates
  })
   
  # ------------------------------------------------------
  # Render Outputs
  # ------------------------------------------------------
  
  # Render the survey questions table
  output$surveyTable <- renderDT({
    datatable(survey_data, options = list(pageLength = 5, autoWidth = TRUE))
  })
  
  # Box Plot by Quadrant
  output$boxPlotQuadrant <- renderPlot({
    req(processed())  # Only show plot if data has been processed
    
    zoom_level <- input$zoom  # Use the zoom level for other elements
    font_size <- input$fontSize  # Use the general font size for axes or titles
    label_size <- input$labelSize  # Label size controlled by user
    
    ggplot(df1_normalized(), aes(x = Abbreviated_Quadrant, y = Importance, fill = Abbreviated_Quadrant)) +
      geom_boxplot(width = 0.5) +  # Adjust box width
      geom_point(position = position_jitter(width = 0.2), size = 2) +  # Jitter points to avoid overlap
      labs(title = "Boxplot of Importance by Quadrant", x = "Quadrant", y = "Importance") +
      theme_minimal(base_size = input$fontSize) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels
        axis.text.y = element_text(size = 10),
        plot.margin = margin(10, 10, 10, 10)  # Increase margin
      ) +
      scale_fill_brewer(palette = "Set3")
  })
  
  
  # Quadrant Table
  output$quadrantTable <- renderDT({
    req(processed())  # Only show table if data has been processed
    quadrant_table <- df1_normalized() %>%
      select(Variable = Abbreviated_Variable_Names, Quadrant)
    
    datatable(quadrant_table, 
              options = list(pageLength = 10, autoWidth = TRUE), 
              class = 'cell-border stripe')
  })
  

  # Scatter Plot with Diagonal
  output$scatterPlotDiagonal <- renderPlot({
    req(processed())  # Only show plot if data has been processed
    
    zoom_level <- input$zoom  # Use the zoom level for other elements
    font_size <- input$fontSize  # Use the general font size for axes or titles
    label_size <- input$labelSize  # Label size controlled by user
    
    ggplot(df1_normalized(), aes(x = Imp_Normalized_Score, y = Perf_Normalized_Score)) +
      geom_point(aes(color = Quadrant), size = 3) +  
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  
      geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  
      geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "blue") +  
      geom_text_repel(aes(label = paste(Abbreviated_Variable_Names, '(', 
                                        round(Imp_Normalized_Score, 2), ',', 
                                        round(Perf_Normalized_Score, 2), ')')), 
                      size = label_size) +  
      labs(title = "Scatter Plot with 45-degree Line",
           x = "Normalized Importance", 
           y = "Normalized Performance", 
           color = "Quadrant") +  
      xlim(-2, 2) +  # Set x-axis limits
      ylim(-2, 2) +  # Set y-axis limits
      scale_color_brewer(palette = "Set2") +  
      theme_minimal(base_size = input$fontSize) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20 * zoom_level, face = "bold"),
        axis.title = element_text(size = 14 * zoom_level),
        axis.text = element_text(size = font_size),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.minor = element_blank()
      )
  })
  
  # Plot highlighting areas needing fixing
  output$fixingPlot <- renderPlot({
    req(processed())  # Only show plot if data has been processed
    
    zoom_level <- input$zoom  # Use the zoom level for other elements
    font_size <- input$fontSize  # Use the general font size for axes or titles
    label_size <- input$labelSize  # Label size controlled by user
    
    ggplot(df1_normalized(), aes(x = Imp_Normalized_Score, y = Perf_Normalized_Score)) +
      geom_point(aes(color = Fix_Needed, shape = Fix_Needed), size = 3) +  
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  
      geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  
      geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "blue") +  
      geom_text_repel(aes(label = Abbreviated_Variable_Names), size = label_size) +  
      labs(title = "Scatter Plot Highlighting Areas Needing Fixing",
           x = "Normalized Importance", 
           y = "Normalized Performance", 
           color = "Fixing Needed") +
      xlim(-2, 2) +  # Set x-axis limits
      ylim(-2, 2) +  # Set y-axis limits
      scale_color_manual(values = c("No Fixing Needed" = "green", "Needs Fixing" = "red")) +
      theme_minimal(base_size = input$fontSize) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20 * zoom_level, face = "bold"),
        axis.title = element_text(size = 14 * zoom_level),
        axis.text = element_text(size = font_size),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.minor = element_blank()
      )
  })
  
  # Fixing Table
  output$fixingTable <- renderDT({
    req(processed())  # Only show table if data has been processed
    fixing_table <- df1_normalized() %>%
      filter(Fix_Needed == "Needs Fixing") %>%
      select(Variable = Abbreviated_Variable_Names, Imp_Normalized_Score, Perf_Normalized_Score)
    
    datatable(fixing_table, 
              options = list(pageLength = 10, autoWidth = TRUE), 
              class = 'cell-border stripe')
  })
  
  
  # Weighted Satisfaction Plot
  output$weightedSatisfactionPlot <- renderPlot({
    req(processed())  # Only show plot if data has been processed
    
    zoom_level <- input$zoom  # Use the zoom level for other elements
    font_size <- input$fontSize  # Use the general font size for axes or titles
    label_size <- input$labelSize  # Label size controlled by user
    
    ggplot(df1_normalized(), aes(x =Abbreviated_Quadrant, y = Weighted_Satisfaction, fill = Abbreviated_Quadrant)) +
      geom_bar(stat = "identity", width = 0.7) +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 12, color = "black"), # Adjust x-axis text
            axis.text.y = element_text(size = 12, color = "black"), # Adjust y-axis text
            axis.title.x = element_text(size = 14, face = "bold"), # Adjust x-axis title
            axis.title.y = element_text(size = 14, face = "bold"), # Adjust y-axis title
            plot.title = element_text(size = 16, face = "bold"), # Adjust plot title
            legend.text = element_text(size = 12), # Adjust legend text size
            legend.title = element_text(size = 14) # Adjust legend title size
      ) +
      labs(title = "Weighted Average Satisfaction by Quadrant", x = "Abbreviated_Quadrant", y = "Weighted Satisfaction") +
      scale_fill_viridis_d(option = "viridis")
  })

    
  # Satisfaction Gap Plot
  output$satisfactionGapPlot <- renderPlot({
    req(processed())  # Only show plot if data has been processed
    
    zoom_level <- input$zoom  # Use the zoom level for other elements
    font_size <- input$fontSize  # Use the general font size for axes or titles
    label_size <- input$labelSize  # Label size controlled by user
    
    ggplot(df1_normalized(), aes(x = Abbreviated_Quadrant, y = Satisfaction_Gap, fill = Abbreviated_Quadrant)) +
      geom_bar(stat = "identity", width = 0.7) +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 12, color = "black"), # Adjust x-axis text
            axis.text.y = element_text(size = 12, color = "black"), # Adjust y-axis text
            axis.title.x = element_text(size = 14, face = "bold"), # Adjust x-axis title
            axis.title.y = element_text(size = 14, face = "bold"), # Adjust y-axis title
            plot.title = element_text(size = 16, face = "bold"), # Adjust plot title
            legend.text = element_text(size = 12), # Adjust legend text size
            legend.title = element_text(size = 14) # Adjust legend title size
      ) +
      labs(title = "Satisfaction Gap (Normalized) by Quadrant", x = "Quadrant", y = "Satisfaction Gap") +
      scale_fill_viridis_d(option = "inferno")
  })
 
   
  # Create the comparison plot
  output$comparisonPlot <- renderPlot({
    req(facet_data())  # Ensure facet_data is available
    
    zoom_level <- input$zoom  # Use the zoom level for other elements
    font_size <- input$fontSize  # Use the general font size for axes or titles
    label_size <- input$labelSize  # Label size controlled by user
    
    ggplot(facet_data(), aes(x = Metric, y = Score, group = University, color = University)) +
      geom_line(aes(linetype = University), size = 1.5) +  # Thicker lines for better visibility
      geom_point(size = 2.5, alpha = 0.7) +  # Smaller, semi-transparent points
      
      # Use geom_text_repel to automatically repel labels and prevent overlaps
      geom_text_repel(aes(label = Abbreviated_Variable_Names),  # Use 'Abbrev_Labels'
                      size = label_size, color = "black",  # Increased label size for legibility
                      nudge_y = 0.1, 
                      max.overlaps = 10,  # Increase max.overlaps to display more labels
                      box.padding = 0.5,  # Add padding around the text boxes
                      point.padding = 0.5,  # Add padding between points and text
                      segment.size = 0.5) +  # Adjust segment size for clarity
      
      # Facet wrap by University
      facet_wrap(~ University, ncol = 2) +  # Adjust columns for better layout
      
      # Add titles and labels
      labs(title = "Comparison of Universities on Importance and Performance",
           x = "Metric", y = "Normalized Score") +
      
      # Minimal theme and color adjustments
      theme_minimal(base_size = input$fontSize) +
      scale_fill_viridis_d(option = "inferno") +
      
      # Further theme adjustments
      theme(
        plot.title = element_text(hjust = 0.5, size = 22 * zoom_level, face = "bold"),  # Title alignment and styling
        axis.title = element_text(size = 16 * zoom_level),  # Axis title size
        axis.text = element_text(size = font_size),  # Axis text size
        legend.position = "bottom",  # Remove legend
        strip.text = element_text(size = 18, face = "bold"),  # Bold facet labels
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "grey80"),  # Light grid lines
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white")  # Background color
      )
  })
  
  # Comparison Table
  output$comparison_table <- renderDT({
    datatable(
      df_comparison(),  # Use the reactive data
      options = list(
        pageLength = 10,      # Set the number of rows to display per page
        autoWidth = TRUE      # Adjust the column width automatically
      ),
      caption = htmltools::tags$caption(
        style = "font-weight: bold; margin-bottom: 5px;",
        "Regional Comparison Table of Universities"
      ),
      rownames = FALSE        # Remove row numbers
    )
  })
  
  
  # IPA Plot  
  output$ipaPlot <- renderPlot({
  req(df2(), processed())  # Ensure df2 exists and data is processed
  
  # Call combined_df() to get the data
  plot_data <- combined_df()
  
  zoom_level <- input$zoom  # Use the zoom level for other elements
  font_size <- input$fontSize  # Use the general font size for axes or titles
  label_size <- input$labelSize  # Label size controlled by user
  
  # Assuming combined_df is your data frame with necessary columns
  ggplot(combined_df(), aes(x = Imp_Normalized_Score, y = Perf_Normalized_Score, color = University)) +
    geom_point(size = 4, shape = 21, fill = "white", stroke = 1.5) +
    geom_text_repel(aes(label = paste(Abbrev_Labels, '(', round(Imp_Normalized_Score, 2), ',', round(Perf_Normalized_Score, 2), ')')), 
                    size = 4, max.overlaps = 10, box.padding = 0.35, point.padding = 0.5,
                    nudge_y = 0.1, family = "Arial", color = "black") +
    geom_hline(yintercept = 0, linetype = "dotted", color = "grey50", size = 1) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey50", size = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "blue") + # Add 45-degree line
    labs(title = "Importance-Performance Analysis (IPA)",
         subtitle = "Normalized Importance vs. Normalized Performance",
         x = "Normalized Importance", 
         y = "Normalized Performance",
         caption = "Data Source: Your Data") +
    xlim(-2, 2) +  
    ylim(-2, 2) +  
    scale_color_brewer(palette = "Dark2") +  
    theme_minimal(base_family = "Arial") +
    theme(
      plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 16, face = "italic"),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      axis.text.x = element_text(size = font_size),
      axis.text.y = element_text(size = font_size),
      legend.position = "right",
      panel.grid.major = element_line(linewidth=0.8, linetype='solid', colour="grey80"),
      panel.grid.minor=element_blank()
    )

  })  
  
  # ------------------------------------------------------
  #  Downloadable Report
  # ------------------------------------------------------
  
  output$report <- downloadHandler(
    filename = function() { paste("comparison_table", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      write.csv(comparison_table, file, row.names = FALSE)
    }
  )
  
  
  # Provide a download option for the plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("IPA_comparison_plot", ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = output$ipaPlot(), device = "png", dpi = input$res, width = 10, height = 7)
    }
  )
  
  # PDF Download Handler
  output$downloadPDF <- downloadHandler(
    filename = function() { paste("comparison_table", Sys.Date(), ".pdf", sep = "") },
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report_template.Rmd", tempReport, overwrite = TRUE)
      
      rmarkdown::render(tempReport, output_file = file, 
                        params = list(table = comparison_table()))
    }
  )
  
}

# ------------------------------------------------------
# Run the Shiny App
# ------------------------------------------------------
# Run the application 
shinyApp(ui = ui, server = server)
