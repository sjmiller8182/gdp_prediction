tqw_get_fred = function(ID, from = NA, to = NA){
  ## ID is the series ID from FRED
  
  ## only 'to' date has been specified
  if (is.na(from) & !is.na(to)){  
    data = tq_get(x = ID, get = "economic.data", to = to)  
  }
  ## only 'from' date has been specified
  else if (!is.na(from) & is.na(to)){
    data = tq_get(x = ID, get = "economic.data", from = from)
  }
  ## both 'from' and 'to' dates have been specified
  else if (!is.na(from) & !is.na(to)){
    data = tq_get(x = ID, get = "economic.data", from = from, to = to)
  }
  ## no dates have been specified
  else{ 
    data = tq_get(x = ID, get = "economic.data")
  }
  
  return(data)
}

tq_plot_ts = function(data, y_col = 'price', x_col = 'date', 
                      title = "Time Series Data", y_label = "Value", x_label = "Time"){
  
  p = data %>%
    ggplot2::ggplot(ggplot2::aes_string(x = x_col, y = y_col)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = title, y = y_label, x = x_label) + 
    tidyquant::theme_tq()
  
  print(p)
  
}


get_format_tq = function(ID, from = NA, to = NA, col_rename = NA, plot = TRUE, verbose = 1,
                         return = FALSE, return_period = "quarterly",
                         resample_period = 'quarters', resample_index_at = 'yearqtr'){
  data = tqw_get_fred(ID = ID, from = from, to = to)
  if (verbose == 1){
    cat("\n\nInitial data pull\n\n")
    data %>% glimpse()
  }
  
  if (is.na(col_rename)){
    new_col_name = 'price'
  }
  else{
    col_name = col_rename  
  }
  
  if (return == TRUE){
    data = data %>%    
      tq_transmute(select = price,
                   mutate_fun = periodReturn,  
                   period = return_period, 
                   col_rename = 'price') %>% 
      dplyr::mutate(price = price * 100)
    
    if (verbose == 1){
      cat("\n\nData after calculating period return\n\n")
      data %>% glimpse()
    }
  }
  
  data = data %>% 
    tq_transmute(mutate_fun = to.period,
                 period = resample_period, 
                 indexAt = resample_index_at,
                 col_rename = col_rename)
  
  if (verbose == 1){
    cat("\n\nFinal Data\n\n")
    data %>% glimpse()
  }

  if (plot == TRUE){
    tq_plot_ts(data = data, y_col = col_rename, x_col = 'date', title = paste0(col_rename, " Over Time"), y_label = col_rename)
  }
  
  
  return(data)
  
}

