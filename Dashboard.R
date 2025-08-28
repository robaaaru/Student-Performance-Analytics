# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(randomForest)
library(corrplot)
library(DT)
library(tidyr)

# Load the dataset
data <- read.csv("Normalized_StressLevelToPerformance.csv")

# Train a random forest model
set.seed(123)
model <- randomForest(academic_performance ~ ., data = data, importance = TRUE)
feature_importance <- importance(model)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Welcome"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home Dashboard", tabName = "home", icon = icon("home")),
      menuItem("Variable Explorer", tabName = "explorer", icon = icon("chart-line")),
      menuItem("Feature Importance", tabName = "importance", icon = icon("sliders-h")),
      menuItem("Risk Predictions", tabName = "risk", icon = icon("exclamation-triangle")),
      menuItem("Manual Prediction", tabName = "predict", icon = icon("calculator")),
      menuItem("Summary Tables", tabName = "summary", icon = icon("table")),
      menuItem("Data Explorer", tabName = "dataexplore", icon = icon("database"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                valueBoxOutput("avgPerformanceBox"),
                valueBoxOutput("highRiskCountBox"),
                valueBoxOutput("keyFactorBox")
              ),
              fluidRow(
                valueBoxOutput("reliableResourceCountBox"),
                valueBoxOutput("unhealthyHabitCountBox"),
                valueBoxOutput("avgStudyLoadBox")
              ),
              fluidRow(
                valueBoxOutput("aboveAverageCountBox"),
                valueBoxOutput("belowAverageCountBox"),
                valueBoxOutput("totalStudentBox")
              )
      ),
      
      tabItem(tabName = "explorer",
              fluidRow(
                box(title = "Select Variable to Explore", width = 4,
                    selectInput("xvar", "Select Variable:",
                                choices = names(data)[names(data) != "academic_performance"],
                                selected = "study_load")),
                box(title = "Box Plot", width = 4, plotlyOutput("boxPlot")),
                box(title = "Bar Chart Summary", width = 4, plotlyOutput("barChart"))
              ),
              fluidRow(
                box(title = "Distribution of Academic Performance", width = 12,
                    plotlyOutput("distributionPlot"))
              )
      ),
      
      tabItem(tabName = "importance",
              fluidRow(
                box(title = "Feature Importance from Random Forest", width = 12,
                    plotlyOutput("featurePlot"))
              ),
              fluidRow(
                box(title = "Top Predictive Features Table", width = 12,
                    tableOutput("featureTable"))
              )
      ),
      
      tabItem(tabName = "risk",
              fluidRow(
                box(title = "At-Risk Student Detection (Predicted Low Performance)", width = 12,
                    DT::dataTableOutput("riskTable"))
              )
      ),
      
      tabItem(tabName = "predict",
              fluidRow(
                column(4, uiOutput("inputCol1")),
                column(4, uiOutput("inputCol2")),
                column(4, uiOutput("inputCol3"))
              ),
              fluidRow(
                column(12, align = "center",
                       br(),
                       actionButton("predictBtn", "Predict My Performance", icon = icon("check")),
                       br(), br(),
                       uiOutput("predictedResultStyled")
                )
              )
      ),
      
      tabItem(tabName = "summary",
              fluidRow(
                box(title = "Grouped Summary by Resource Access", width = 6,
                    tableOutput("resourceSummary")),
                box(title = "Grouped Summary by Personal Habits", width = 6,
                    tableOutput("habitSummary"))
              ),
              fluidRow(
                box(title = "Grouped Summary by Motivation Factors", width = 6,
                    tableOutput("motivationSummary")),
                box(title = "Grouped Summary by Living Conditions", width = 6,
                    tableOutput("livingSummary"))
              ),
              fluidRow(
                box(title = "Grouped Summary by Extracurricular Activities", width = 6,
                    tableOutput("extraSummary")),
                box(title = "Grouped Summary by Study Load", width = 6,
                    tableOutput("loadSummary"))
              )
      ),
      
      tabItem(tabName = "dataexplore",
              fluidRow(
                box(title = "Interactive Data Explorer", width = 12,
                    DT::dataTableOutput("dataExplorer"))
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  output$corrPlot <- renderPlot({
    cor_matrix <- cor(data)
    corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", addCoef.col = "black")
  })
  
  output$distributionPlot <- renderPlotly({
    p <- ggplot(data, aes(x = academic_performance)) +
      geom_histogram(fill = "steelblue", bins = 30) +
      labs(title = "Distribution of Academic Performance")
    ggplotly(p)
  })
  
  output$boxPlot <- renderPlotly({
    p <- ggplot(data, aes_string(x = 'academic_performance', y = input$xvar)) +
      geom_boxplot(fill = "orange", alpha = 0.7) +
      theme_minimal()
    ggplotly(p)
  })
  
  output$barChart <- renderPlotly({
    grouped <- data %>%
      group_by(!!sym(input$xvar)) %>%
      summarise(MeanPerformance = mean(academic_performance, na.rm = TRUE)) %>%
      ungroup()
    
    p <- ggplot(grouped, aes_string(x = input$xvar, y = "MeanPerformance")) +
      geom_bar(stat = "identity", fill = "#0073C2FF") +
      labs(x = input$xvar, y = "Average Performance") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>% layout(margin = list(b = 100))
  })
  
  
  output$featurePlot <- renderPlotly({
    feat_df <- data.frame(Feature = rownames(feature_importance), Importance = feature_importance[, 1])
    feat_df <- feat_df[order(feat_df$Importance, decreasing = TRUE), ]
    plot_ly(feat_df, x = ~Feature, y = ~Importance, type = 'bar')
  })
  
  output$featureTable <- renderTable({
    feat_df <- data.frame(Feature = rownames(feature_importance), Importance = round(feature_importance[, 1], 3))
    feat_df[order(-feat_df$Importance), ]
  })
  
  output$featureInsights <- renderUI({
    top_feats <- rownames(feature_importance)[order(feature_importance[, 1], decreasing = TRUE)][1:5]
    HTML(paste0("<ul>", paste0("<li><b>", top_feats, ":</b> Strongly linked to academic performance.</li>"), "</ul>"))
  })
  
  output$riskTable <- DT::renderDataTable({
    preds <- predict(model, data)
    df <- data
    df$predicted_performance <- preds
    df %>% filter(predicted_performance < 0.4) %>% arrange(predicted_performance) %>%
      DT::datatable(options = list(pageLength = 10))
  })
  
  output$resourceSummary <- renderTable({
    data %>%
      mutate(group = ifelse((basic_needs + living_conditions + safety)/3 > 0.5, "Reliable", "Limited")) %>%
      group_by(group) %>%
      summarise(Average = mean(academic_performance), Count = n())
  })
  
  output$habitSummary <- renderTable({
    data %>%
      mutate(group = ifelse((sleep_quality + study_load + extracurricular_activities)/3 > 0.5, "Healthy", "Unhealthy")) %>%
      group_by(group) %>%
      summarise(Average = mean(academic_performance), Count = n())
  })
  
  output$motivationSummary <- renderTable({
    data %>%
      mutate(group = ifelse((self_esteem + future_career_concerns + teacher_student_relationship)/3 > 0.5, "Motivated", "Less Motivated")) %>%
      group_by(group) %>%
      summarise(Average = mean(academic_performance), Count = n())
  })
  
  output$livingSummary <- renderTable({
    data %>%
      mutate(group = ifelse((living_conditions + safety)/2 > 0.5, "Stable", "Unstable")) %>%
      group_by(group) %>%
      summarise(Average = mean(academic_performance), Count = n())
  })
  
  output$extraSummary <- renderTable({
    data %>%
      mutate(group = ifelse(extracurricular_activities > 0.5, "Active", "Inactive")) %>%
      group_by(group) %>%
      summarise(Average = mean(academic_performance), Count = n())
  })
  
  output$loadSummary <- renderTable({
    data %>%
      mutate(group = ifelse(study_load > 0.5, "Heavy Load", "Light Load")) %>%
      group_by(group) %>%
      summarise(Average = mean(academic_performance), Count = n())
  })
  
  output$dataExplorer <- DT::renderDataTable({
    datatable(data, options = list(pageLength = 10))
  })
  
  output$avgPerformanceBox <- renderValueBox({
    valueBox(round(mean(data$academic_performance), 2), "Avg Performance", icon = icon("graduation-cap"))
  })
  
  output$highRiskCountBox <- renderValueBox({
    preds <- predict(model, data)
    valueBox(sum(preds < 0.4), "At-Risk Students", icon = icon("exclamation-triangle"), color = "red")
  })
  
  output$keyFactorBox <- renderValueBox({
    key_factor <- rownames(feature_importance)[which.max(feature_importance)]
    valueBox(key_factor, "Top Factor", icon = icon("lightbulb"), color = "yellow")
  })
  
  output$reliableResourceCountBox <- renderValueBox({
    count <- sum(((data$basic_needs + data$living_conditions + data$safety) / 3) > 0.5)
    valueBox(count, "Students with Reliable Resources", icon = icon("home"), color = "blue")
  })
  
  output$unhealthyHabitCountBox <- renderValueBox({
    count <- sum(((data$sleep_quality + data$study_load + data$extracurricular_activities) / 3) < 0.5)
    valueBox(count, "Students with Unhealthy Habits", icon = icon("bed"), color = "maroon")
  })
  
  output$avgStudyLoadBox <- renderValueBox({
    valueBox(round(mean(data$study_load), 2), "Average Study Load", icon = icon("book"), color = "teal")
  })
  
  output$aboveAverageCountBox <- renderValueBox({
    mean_perf <- mean(data$academic_performance)
    valueBox(sum(data$academic_performance > mean_perf), "Above Average Students", icon = icon("arrow-up"), color = "green")
  })
  
  output$belowAverageCountBox <- renderValueBox({
    mean_perf <- mean(data$academic_performance)
    valueBox(sum(data$academic_performance < mean_perf), "Below Average Students", icon = icon("arrow-down"), color = "orange")
  })
  
  output$totalStudentBox <- renderValueBox({
    valueBox(nrow(data), "Total Students", icon = icon("users"), color = "aqua")
  })
  
  # PREDICTOR UI
  output$inputCol1 <- renderUI({
    lapply(names(data)[1:ceiling(length(names(data))/3)], function(var) {
      sliderInput(var, label = var, min = 0, max = 1, value = mean(data[[var]]), step = 0.01)
    })
  })
  
  output$inputCol2 <- renderUI({
    lapply(names(data)[(ceiling(length(names(data))/3)+1):(2*ceiling(length(names(data))/3))], function(var) {
      if (var != "academic_performance")
        sliderInput(var, label = var, min = 0, max = 1, value = mean(data[[var]]), step = 0.01)
    })
  })
  
  output$inputCol3 <- renderUI({
    lapply(names(data)[(2*ceiling(length(names(data))/3)+1):length(names(data))], function(var) {
      if (var != "academic_performance")
        sliderInput(var, label = var, min = 0, max = 1, value = mean(data[[var]]), step = 0.01)
    })
  })
  
  observeEvent(input$predictBtn, {
    input_vals <- sapply(names(data)[names(data) != "academic_performance"], function(var) input[[var]])
    new_df <- as.data.frame(t(input_vals))
    colnames(new_df) <- names(data)[names(data) != "academic_performance"]
    pred <- predict(model, new_df)
    output$predictedResultStyled <- renderUI({
      div(style = "font-size: 28px; font-weight: bold; color: #0073e6;", 
          paste("Predicted Academic Performance:", round(pred, 2)))
    })
  })
}

shinyApp(ui = ui, server = server)
