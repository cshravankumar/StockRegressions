#############################
# Thomas Cerbelaud | Shravan Kumar Chandrasekaran | Mathilde Sirbu
# SIEO 4150 
# Homework 3, Group problem
# Due date: 09/15
#############################

# Set the path to our folder
getwd()
dir = "Historical prices/"

source("HMWK#1.R")
library(shiny)

# Define server logic
shinyServer(function(input, output) {
  
  # Output for the main panel (and the side panel for the confidence interval)
  
  # Plot output
  output$distPlot <- renderPlot({
    x <- df_log_return[, input$var]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white', 
         xlab = paste(input$var, "log return", sep = " "), 
         main=paste("Log-return histogram of", input$var, sep = " "),)
    # Draw normal curve when the checkbox is checked
    sd = sd(x,)
    var = var(x,)
    mean = mean(x,)
    df = length(x)-1
    if(input$checkbox) {
      curve(dnorm(x, mean = mean, sd = sd), add = T,)
    }

    # Display interval of confidence when checked
    if(input$checkbox_confidence_mean){
      lb1 <- mean + qnorm((1 - input$confidence_mean/100)/2) * sd/sqrt(length(x)) # lower bound
      ub1 <- mean + qnorm((1 - input$confidence_mean/100)/2, lower.tail = FALSE) * sd/sqrt(length(x)) # upper bound
      # Graphically represents the confidence interval
      abline(v = ub1, col = "palevioletred", lw = 2)
      abline(v = lb1, col = "palevioletred", lw = 2)
      # Display the numeric form of the confidence interval in the side panel, text output
      output$CI_mean <- renderText({paste("The confidence interval for the mean is","[", 
                                          format(lb1, scientific = TRUE, digits = 3), ", ",
                                          format(ub1, scientific = TRUE, digits = 3), "]", sep = " ")})
    }
    
    # Display interval of confidence when checked
    if(input$checkbox_confidence_variance){
      lb2 <- (length(x)-1) * var / qchisq((1 - input$confidence_variance/100)/2, df, lower.tail = FALSE) # lower bound
      ub2 <- (length(x)-1) * var / qchisq((1 - input$confidence_variance/100)/2, df) # upper bound
      # Display the numeric form of the confidence interval in the side panel, text output
      output$CI_variance <- renderText({paste("The confidence interval for the variance is","[", 
                                              format(lb2, scientific = TRUE, digits = 3), ", ", 
                                              format(ub2, scientific = TRUE, digits = 3), "]", 
                                              sep = " ")})
    }
  })
  
  # Brent Regression
  b <- df_log_return$BRENT
  #model <- lm(formula = x ~ b, data = df_log_return)
  #output$regBrent <- renderDataTable(model$)
  output$regPlotBrent <- renderPlot({
    # Plot data points and fit
    x <- df_log_return[, input$var]
    model <- lm(formula = x ~ b, data = df_log_return)
    plot(x = b, y = x, xlab = "Brent log return", 
         ylab = paste(input$var, "log return", sep = " "), 
         main = paste("Linear regression of", input$var, "over the Brent log return", sep = " "))
    lines(x = b, y = model$fit, col = "darkblue", type = "l", lw = 3)
    legend("bottomright", c("Fit Line"), lty=1, lwd = 2, col="darkblue", cex = 0.5)
    
    # Residual
    if (input$checkbox_residuals) {
      lines(x = b, y = model$residuals, col = "red", type = "p", lw = 1)
      legend("bottomright", c("Residual"), lty=1, lwd = 2, col="red", cex = 0.5)
      output$textResBrent <- renderText({"In red is displayed the residuals."})
    }
    
    # Regression table
    df_reg_brent <- data.frame()
    df_reg_brent[1,"Intercept"] <- model$coefficients["(Intercept)"]
    df_reg_brent[1,"Slope"] <- model$coefficients["b"]
    df_reg_brent[1,"R-squared"] <- summary(model)$r.squared
    output$regBrentTable <- renderDataTable({df_reg_brent}, options = list(
      paging = FALSE,
      searching = FALSE,
      searchable = FALSE
    ))
  })
  
  # Regression
  output$regPlotTime <- renderPlot({
    # Plot data points and fit
    x <- df_log_return[, input$var]
    d <- as.double(brent$Date)
    model <- lm(x ~ d)
    plot(x = d, y = x, type = "p",xlab = "Time",
         ylab = paste(input$var, "log return", sep = " "), 
         main = paste("Linear regression of", input$var, "over time (as integer)"))
    lines(x = d, y = model$fit, col = "darkblue", type = "l", lw = 3)
    legend("bottomright", c("Fit Line"), lty=1, lwd = 2, col="darkblue", cex = 0.5)
    
    # Residual
    if (input$checkbox_residuals) {
      lines(x = d, y = model$residuals, col = "red", type = "p", lw = 1)
      legend("bottomright", c("Residual"), lty=1, lwd = 2, col="red", cex = 0.5)
      output$textResTime <- renderText({"In red is displayed the residuals."})
    }
    
    # Regression table
    df_reg_time <- data.frame()
    df_reg_time[1,"Intercept"] <- model$coefficients["(Intercept)"]
    df_reg_time[1,"Slope"] <- model$coefficients["d"]
    df_reg_time[1,"R-squared"] <- summary(model)$r.squared
    output$regTimeTable <- renderDataTable({df_reg_time}, options = list(
      paging = FALSE,
      searching = FALSE,
      searchable = FALSE
    ))
  })
  
  # Two-stocks Regression
  output$regPlotStocks <- renderPlot({
    # Plot data points and fit
    x <- df_log_return[, input$var]
    y <- df_log_return[, input$comp]
    model <- lm(x ~ y)
    plot(x = y, y = x, type = "p",
         xlab = paste(input$comp, "log return", sep = " "),
         ylab = paste(input$var, "log return", sep = " "), 
         main = paste("Linear regression of", input$var, "over", input$var, sep = " "))
    lines(x = y, y = model$fit, col = "darkblue", type = "l", lw = 3)
    legend("bottomright", c("Fit Line"), lty=1, lwd = 2, col="darkblue", cex = 0.5)
    
    # Residual
    if (input$checkbox_residuals) {
      lines(x = y, y = model$residuals, col = "red", type = "p", lw = 1)
      legend("bottomright", c("Residual"), lty=1, lwd = 2, col="red", cex = 0.5)
      output$textResStocks <- renderText({"In red is displayed the residuals."})
    }
    
    # Regression table
    df_reg_stocks <- data.frame()
    df_reg_stocks[1,"Intercept"] <- model$coefficients["(Intercept)"]
    df_reg_stocks[1,"Slope"] <-model$coefficients["y"]
    df_reg_stocks[1,"R-squared"] <- summary(model)$r.squared
    output$regStocksTable <- renderDataTable({df_reg_stocks}, options = list(
      paging = FALSE,
      searching = FALSE,
      searchable = FALSE
    ))
  })
  
  # Test the equality of the two population means
  output$twoMeans <- renderPrint({
    x <- df_log_return[, input$var]
    y <- df_log_return[, input$comp]
    test <- abs(mean(x,) - mean(y,)) / sqrt(sd(x,)^2/(length(x)) + sd(y,)^2/(length(y))) # test estimator
    # Display the test result 
    if(test > qnorm(0.025, lower.tail = FALSE)) {
      output$mean_Comp <- renderText({"At five percent level of significance, the hypothesis that the means of the selected samples are equal is rejected."}) 
    } else {
      output$mean_Comp <- renderText({"At five percent level of significance, the hypothesis that the means of the selected samples are equal is not rejected."})
    }
    
    # Display interval of confidence of the 2 variables to compare
    lb_x <- mean(x) + qnorm(0.05/2) * sd(x)/sqrt(length(x)) # lower bound
    ub_x <- mean(x) + qnorm(0.05/2, lower.tail = FALSE) * sd(x)/sqrt(length(x)) # upper bound
    lb_y <- mean(y) + qnorm(0.05/2) * sd(y)/sqrt(length(y)) # lower bound
    ub_y <- mean(y) + qnorm(0.05/2, lower.tail = FALSE) * sd(y)/sqrt(length(y)) # upper bound
    # Display the numeric form of the confidence interval 
    output$CI_var <- renderText({paste("The confidence interval for the mean of", input$var, 
                                       "is", "[", format(lb_x, scientific = TRUE, digits = 3), 
                                       ", ", format(ub_x, scientific = TRUE, digits = 3), "].", 
                                       sep = " ")})
    output$CI_comp <- renderText({paste("The confidence interval for the mean of", input$comp, 
                                        "is", "[", format(lb_y, scientific = TRUE, digits = 3), 
                                        ", ", format(ub_y, scientific = TRUE, digits = 3), 
                                        "].", sep = " ")})
  })
  
  # Brent plot over time
  output$brentPlotTime <- renderPlot({
    d <- brent$Date
    plot(x = d, y = brent$Close, type = "l", lwd = 3, ylab = "Brent price [$]", xlab = "Time")
  })
})