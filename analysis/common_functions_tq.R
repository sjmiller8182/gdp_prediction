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

tqw_plot_ts = function(data, y_col = 'price', x_col = 'date', mutate = FALSE, old_name = NA,
                       title = "Time Series Data", y_label = "Value", x_label = "Time"){
  # 'data' is a dataframe containing the time and value as 2 of the columns
  # x_col is the "Time' column name
  # y_col is the Time Series Value column name
  
  if (mutate){
    if (is.na(old_name)){
      stop("You have selected to MUTATE the 'y' column (Time Series value), but have not specified the original name in the dataframe that represents this column. Please pass the 'old_name' argument to this function.")
    }
    data = tqw_mutate_column(data = data, old_name = old_name, new_name = y_col)
  }
  
  data %>%
    ggplot(aes_string(x = x_col, y = y_col)) +
    geom_line() +
    labs(title = title, y = y_label, x = x_label) + 
    theme_tq()  
}

tqw_mutate_column = function(data, old_name, new_name){
  data = data %>% 
    mutate(!!new_name := !!as.name(old_name)) %>% 
    select(-!!old_name)
  
  return(data)
}