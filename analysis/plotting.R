# This file contains plotting functions

library(ggplot2)
library(gridExtra)
library(dplyr)

#' Plot a response vs lags of an explanatory variable
#'
#' @param data A data.frame containing the response and explanatory variables.
#' @param response The response variable.
#' @param explanatory The explanatory variable, which will be lagged.
#' @param max.lags Number of lag plots to create.
#' @param start.lag The number of lags behind the response variable to start.
#'
#' @return Correlation of lags
#' 
#' @example 
#' plot.cross.lags(data, 'response.var', 'explanatory.var')
#'
plot.cross.lags <- function(data, response, explanatory, max.lags, start.lag = 1){
  
  size <- nrow(data)
  plots <- list()
  corr <- rep(0, max.lags - start.lag + 1)
  
  for (i in start.lag:max.lags){
    # calculate lag correlation
    corr[i] <- cor(data[(i + 1) : size, response],
                   dplyr::lag(data[ , explanatory], i)[(i + 1) : size])
    
    # create lagged dataset
    temp.frame <- data.frame(data[(i + 1) : size, response],
                             dplyr::lag(data[ , explanatory], i)[(i + 1) : size])
    names(temp.frame) <- c(response, explanatory)
    
    # create lag plot
    plots[[paste('p', as.character(i))]] <- 
      ggplot(data = temp.frame,
             aes_string(x = explanatory, y = response)) +
      geom_point() +
      geom_smooth(method = 'lm') +
      ggtitle(paste('Cor:', round(corr[i], 5))) +
      xlab(paste(as.character(i),' lags of ', explanatory, sep = '')) 
  }
  # plots in a grid
  do.call("grid.arrange", c(plots, top = paste(response, "vs lags of", explanatory)))
  
  # return the lag correlations
  return(
    data.frame('variable' = rep(explanatory, max.lags - start.lag + 1),
               'lags' = start.lag:max.lags,
               'correlation' = corr,
               stringsAsFactors = F)
  )
}

