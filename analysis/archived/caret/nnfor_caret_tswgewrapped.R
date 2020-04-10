library(tswgewrapped)
library(dplyr)

#### Basic Testing ####
# file = system.file("extdata", "USeconomic.csv", package = "tswgewrapped", mustWork = TRUE)
# USeconomic = read.csv(file, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
# names(USeconomic) = gsub("[(|)]", "", colnames(USeconomic))
# data = USeconomic
# 
# # Random Parallel
# model = ModelBuildNNforCaret$new(data = data, var_interest = "logGNP", m = 2,
#                                  search = 'random',
#                                  grid = NA, tuneLength = 2,
#                                  batch_size = 136, h = 2,
#                                  parallel = TRUE,
#                                  seed = 1,
#                                  verbose = 1)
# 
# 
# model$summarize_hyperparam_results()
# model$summarize_best_hyperparams()
# model$plot_hyperparam_results()
# model$summarize_build()


#### 1.0 Project ####
data = read.csv("data/economic_indicators_all_ex_3mo_china.csv")
data %>% glimpse()

#### 1.1 Random Parallel ####
model = ModelBuildNNforCaret$new(data = data %>% select(-date), var_interest = "gdp_change", m = 6,
                                 search = 'random',
                                 tuneLength = 2,
                                 batch_size = 195, h = 2,
                                 parallel = TRUE,
                                 seed = 1,
                                 verbose = 1,
                                 lags = 1:4,
                                 keep = c(TRUE, FALSE, FALSE, TRUE),
                                 difforder = c(1))
                                 #xreg.lags = list(1:2, 1:2, 1:2))


model$summarize_hyperparam_results()
model$summarize_best_hyperparams()
model$plot_hyperparam_results()
model$summarize_build()

nnfor_model = model$get_final_models(subset = 'r')
nnfor_model

#### 1.2 Grid Parallel #### 
model = ModelBuildNNforCaret$new(data = data %>% select(-date), var_interest = "gdp_change", m = 6,
                                 search = 'grid',
                                 grid = NA,
                                 batch_size = 195, h = 2,
                                 parallel = TRUE,
                                 seed = 1,
                                 verbose = 1)


model$summarize_hyperparam_results()
model$summarize_best_hyperparams()
model$plot_hyperparam_results()
model$summarize_build()

nnfor_model = model$get_final_models(subset = 'r')
nnfor_model

#### 1.3 Grid (User Defined) Parallel #### 
nnfor_grid = expand.grid(reps = c(20, 50),
                         hd = c(1:5),
                         allow.det.season = c(FALSE, TRUE))
  
model = ModelBuildNNforCaret$new(data = data %>% select(-date), var_interest = "gdp_change", m = 6,
                                 search = 'grid',
                                 grid = nnfor_grid,
                                 batch_size = 191, h = 2,
                                 parallel = TRUE,
                                 seed = 1,
                                 verbose = 1)

model$summarize_hyperparam_results()
model$summarize_best_hyperparams()
model$plot_hyperparam_results()
model$summarize_build()

