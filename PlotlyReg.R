library(htmltools)
library(tidyverse)
library(plotly)
RegressionPlots <- function(fit){
  # Extract fitted values from lm() object
  Fitted.Values <-  fitted(fit)
  # Extract residuals from lm() object
  Residuals <-  resid(fit)
  # Extract standardized residuals from lm() object
  Standardized.Residuals <- MASS::stdres(fit)  
  # Extract fitted values for lm() object
  Theoretical.Quantiles <- qqnorm(Residuals, plot.it = F)$x
  # Square root of abs(residuals)
  Root.Residuals <- sqrt(abs(Standardized.Residuals))
  # Calculate Leverage
  Leverage <- lm.influence(fit)$hat
  NamesV <-rownames(fit$model) 
  # Create data frame 
  # Will be used as input to plot_ly
  regMat <- data.frame(Fitted.Values, 
                       Residuals, 
                       Standardized.Residuals, 
                       Theoretical.Quantiles,
                       Root.Residuals,
                       Leverage, 
                       NamesV)
  
  # Plot using Plotly
  # Fitted vs Residuals
  # For scatter plot smoother
  LOESS1 <- loess.smooth(Fitted.Values, Residuals)
  plt1 <- regMat %>% 
    plot_ly(x = ~Fitted.Values, y = ~Residuals, 
            type = "scatter", mode = "markers", showlegend = F, text = ~ NamesV) %>% 
    add_trace(x = LOESS1$x, y = LOESS1$y, type = "scatter", mode = "line", name = "Smooth",
              line = list(width = 2), text = "") %>% 
    layout(xaxis = list(text="Residuals"), yaxis=list(text="Fitted Values"), plot_bgcolor = "#e6e6e6")
  
  # QQ Plot
  plt2 <- regMat %>% 
    plot_ly(x = ~Theoretical.Quantiles, y = ~Standardized.Residuals, text = ~ NamesV,
            type = "scatter", mode = "markers",
            marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
    
    add_trace(x = Theoretical.Quantiles, y = Theoretical.Quantiles, type = "scatter", mode = "line", name = "",
              line = list(width = 2)) %>% 
    layout(xaxis = list(text="Theoretical"), yaxis=list(text="Standardized Residuals"), plot_bgcolor = "#e6e6e6")
  
  # Scale Location
  # For scatter plot smoother
  LOESS2 <- loess.smooth(Fitted.Values, Root.Residuals)
  
  plt3 <- regMat %>% 
    plot_ly(x = ~Fitted.Values, y = ~Root.Residuals, text = ~NamesV,
            type = "scatter", mode = "markers", 
            marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
    add_trace(x = LOESS2$x, y = LOESS2$y, type = "scatter", mode = "line", name = "Smooth",
              line = list(width = 2), text="") %>% layout(xaxis = list(text="Scale"), yaxis = list(text="Location"), plot_bgcolor = "#e6e6e6")
  
  # Residuals vs Leverage
  # For scatter plot smoother
  LOESS3 <- loess.smooth(Leverage, Residuals)
  
  plt4 <- regMat %>% 
    plot_ly(x = ~Leverage, y = ~Residuals, text =  ~NamesV,
            type = "scatter", mode = "markers",
            marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
    add_trace(x = LOESS3$x, y = LOESS3$y, type = "scatter", mode = "line", name = "Smooth",
              line = list(width = 2), text="") %>% layout(xaxis = list(text = "Leverage"), yaxis=list(text="Residuals"), plot_bgcolor = "#e6e6e6")
  plt = subplot(plt1, plt2, plt3, plt4, nrows=2)
  return(plt)
}