# r_final_project
# Load necessary libraries
library(shiny) 
library(MASS)
library(ggplot2)
library(Metrics) 
library(caret)  

# Load the dataset
data("Boston")

# Define UI
ui <- fluidPage(
    titlePanel("Boston Housing Data Regression Analysis"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("plotType", "Select a plot type:",
                        choices = c("Histogram of MEDV", "Correlation Heatmap", 
                                    "Feature Importance", "Residuals Plot")),
            actionButton("update", "Update Plot")
        ),
        
        mainPanel(
            plotOutput("selectedPlot"),
            verbatimTextOutput("modelSummary"),
            verbatimTextOutput("performanceMetrics"),
            verbatimTextOutput("crossValidation")
        )
    )
)

# Define server logic
server <- function(input, output) {
    
    # Perform Linear Regression
    model <- lm(medv ~ ., data = Boston)
    
    # Reactive expression for model summary
    output$modelSummary <- renderPrint({
        summary(model)
    })
    
    # Reactive expression for performance metrics
    output$performanceMetrics <- renderPrint({
        r_squared <- summary(model)$r.squared
        adj_r_squared <- summary(model)$adj.r.squared
        residual_standard_error <- summary(model)$sigma
        
        cat("R-squared: ", r_squared, "\n")
        cat("Adjusted R-squared: ", adj_r_squared, "\n")
        cat("Residual Standard Error: ", residual_standard_error, "\n")
        
        # Model Comparison
        predictions <- predict(model, newdata = Boston)
        mse_linear <- mse(Boston$medv, predictions)
        mae_linear <- mae(Boston$medv, predictions)
        
        cat("Linear Model - MSE: ", mse_linear, "\n")
        cat("Linear Model - MAE: ", mae_linear, "\n")
    })
    
    # Cross-validation
    output$crossValidation <- renderPrint({
        set.seed(123)
        train_control <- trainControl(method = "cv", number = 10)
        cv_model <- train(medv ~ ., data = Boston, method = "lm", trControl = train_control)
        print(cv_model)
    })
    
    # Render selected plot
    output$selectedPlot <- renderPlot({
        input$update  # React to button clicks
        isolate({
            if (input$plotType == "Histogram of MEDV") {
                ggplot(Boston, aes(x = medv)) +
                    geom_histogram(binwidth = 2, fill = "lightblue", color = "black") +
                    labs(title = "Distribution of MEDV", x = "MEDV", y = "Frequency")
                
            } else if (input$plotType == "Correlation Heatmap") {
                correlation_matrix <- cor(Boston)
                heatmap(correlation_matrix, Rowv = NA, Colv = NA, 
                        main = "Correlation Heatmap", col = heat.colors(256), 
                        scale = "column", margins = c(5, 10))
                
            } else if (input$plotType == "Feature Importance") {
                coefficients <- summary(model)$coefficients
                significant_coeffs <- coefficients[order(abs(coefficients[, "Estimate"]), 
                                               decreasing = TRUE), ]
                
                barplot(significant_coeffs[, "Estimate"], names.arg = rownames(significant_coeffs),
                        main = "Feature Importance", las = 2, col = "lightgreen", border = "black")
                
            } else if (input$plotType == "Residuals Plot") {
                par(mfrow = c(2, 2))
                plot(model)
            }
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
