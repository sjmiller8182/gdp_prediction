library(dplyr)
library(caret)
library(tseries)
library(parallel)
library(doParallel)
library(tictoc)

# Common Functions ####

## See https://topepo.github.io/caret/model-training-and-tuning.html#metrics
ASE = function(data, lev = NULL, model = NULL) {
  ASE = mean((data$pred - data$obs)^2)
  RMSE = sqrt(ASE)
  return (c(RMSE = RMSE, ASE = ASE))
}

check_equality_with_manual_model = function(manual_model, caret_model){
  stopifnot(all.equal(caret_model$finalModel$MSE, manual_model$MSE))
  stopifnot(all.equal(caret_model$finalModel$net, manual_model$net))
  stopifnot(all.equal(caret_model$finalModel$hd, manual_model$hd))
  stopifnot(all.equal(caret_model$finalModel$lags, manual_model$lags))
  stopifnot(all.equal(caret_model$finalModel$xreg.lags, manual_model$xreg.lags))
  stopifnot(all.equal(caret_model$finalModel$difforder, manual_model$difforder))
  stopifnot(all.equal(caret_model$finalModel$sdummy, manual_model$sdummy))
  stopifnot(all.equal(caret_model$finalModel$ff.det, manual_model$ff.det))
  stopifnot(all.equal(caret_model$finalModel$det.type, manual_model$det.type))
  stopifnot(all.equal(caret_model$finalModel$minmax, manual_model$minmax))
  stopifnot(all.equal(caret_model$finalModel$xreg.minmax, manual_model$xreg.minmax))
  stopifnot(all.equal(caret_model$finalModel$comb, manual_model$comb))
  stopifnot(all.equal(caret_model$finalModel$fitted, manual_model$fitted))
  stopifnot(all.equal(caret_model$finalModel$MSEH, manual_model$MSEH))
}

# In order to make models fully reproducible when using parallel processing, we need to pass seeds as a parameter
# https://stackoverflow.com/questions/13403427/fully-reproducible-parallel-models-using-caret

get_seeds = function(search, fitControl){
  ## TODO: Fix the nnfor_grid and tuneLength params in the class to get from object variables
  ## Also data needs to be come object variables
  if (search == "grid"){
    if (!all(is.na(nnfor_grid))){
      total.param.permutations = nrow(nnfor_grid)
    }
    else{
      total.param.permutations = nrow(nnfor_caret$grid())
    }
  }
  else if (search == "random"){
    if (!is.na(tuneLength)){
      total.param.permutations = tuneLength
    }
    else{
      total.param.permutations = nrow(nnfor_caret$grid())
    }
  }
  
  folds = (nrow(data) - fitControl$initialWindow)/fitControl$horizon
  
  # length is = (n_repeats*nresampling)+1
  seeds <- vector(mode = "list", length = folds + 1)
  
  set.seed(1)  
  
  for(i in 1:folds) seeds[[i]]<- sample.int(n=1, total.param.permutations, replace = TRUE)
  # for the last model
  seeds[[folds + 1]]<-sample.int(1, 1, replace = TRUE)
  
  return(seeds)
}

get_fit_control = function(initialWindow, h, search = "random", verbose = TRUE, parallel = TRUE){
  
  fitControl = caret::trainControl(method = "timeslice",
                                   horizon = h,
                                   skip = h-1,
                                   fixedWindow = TRUE,
                                   summaryFunction = ASE,
                                   verboseIter = verbose,
                                   returnData = TRUE,
                                   returnResamp = "all",
                                   savePredictions = TRUE,
                                   allowParallel = TRUE)

  fitControl$initialWindow = initialWindow
  fitControl$search = search
  fitControl$horizon = h
  fitControl$skip = h-1
  
  fitControl$seeds = get_seeds(search = search, fitControl = fitControl)
  
  return(fitControl)
}

# Load Data ####
data("USeconomic")
data = as.data.frame(USeconomic)
names(data) = gsub("[(|)]", "", colnames(data))


# Global Settings ####
source("analysis/source_caret.R")
parallel = TRUE

initialWindow = 134
h = 2

# Random Search ####

search = "random"
tuneLength = 5
fitControl = get_fit_control(initialWindow = initialWindow, h = h, search = search, parallel = parallel)

# http://sshaikh.org/2015/05/06/parallelize-machine-learning-in-r-with-multi-core-cpus/

if (parallel == TRUE){
  num_cores = parallel::detectCores()
  cl = parallel::makeCluster(ifelse(num_cores == 1, 1, num_cores -1)) # Leave one core out
  doParallel::registerDoParallel(cl)
}

tic("- Total Time for training: ")

## This seed is for selecting the random grid. 
## It does not impact the model seed if seeds are passed in trainControl
## Not sure what will happen if the seeds are not passed in trainControl. My guess is that
## this seed will then be used to set the seeds for each fold as well (but would need to check)
set.seed(101)  

# Must pass a dataframe to caret::train (does not accept ts object), 
# hence we need to specify the frequency of y separately using 'm'
# tuneLength default = 3 (if not specified)
nnfor_model = caret::train(logGNP ~ ., data = data,
                           method = nnfor_caret,
                           trControl = fitControl,
                           tuneLength = tuneLength,
                           metric = "ASE", maximize = FALSE,
                           # Additional Params to be passed to model
                           ## necessary to pass the frequency 'm' of variable of interest, else lags may not get picked
                           m = 4,
                           ## Rest of the variables passed below are optional
                           lags = 1:4,
                           keep = c(TRUE, FALSE, FALSE, TRUE),
                           difforder = c(1),
                           xreg.lags = list(1:2, 1:2, 1:2)
                           )

set.seed(101)
nnfor_model2 = caret::train(logGNP ~ ., data = data,
                           method = nnfor_caret,
                           trControl = fitControl,
                           tuneLength = tuneLength,
                           metric = "ASE", maximize = FALSE,
                           # Additional Params to be passed to model
                           ## necessary to pass the frequency 'm' of variable of interest, else lags may not get picked
                           m = 4,
                           ## Rest of the variables passed below are optional
                           lags = 1:4,
                           keep = c(TRUE, FALSE, FALSE, TRUE),
                           difforder = c(1),
                           xreg.lags = list(1:2, 1:2, 1:2)
)

toc()

if(parallel == TRUE){
  stopCluster(cl)
  registerDoSEQ()
}

## Check for reproducibility of model
## Caret gives final model on the full data
set.seed(1)
manual_model = nnfor::mlp(y = ts(data %>% dplyr::select(logGNP) %>% purrr::pluck()),
                          xreg = data %>% dplyr::select(-logGNP),
                          reps = nnfor_model$bestTune$reps,
                          hd = nnfor_model$bestTune$hd,
                          allow.det.season = nnfor_model$bestTune$allow.det.season,
                          # Additional Params to be passed to model
                          m = 4,
                          ## Rest of the variables passed below are optional
                          lags = 1:4,
                          keep = c(TRUE, FALSE, FALSE, TRUE),
                          difforder = c(1),
                          xreg.lags = list(1:2, 1:2, 1:2)
                          )

# Check equality of 2 separate caret runs
all.equal(predict(nnfor_model), predict(nnfor_model2))

# Check equality of caret run with manual run
print(manual_model)
print(nnfor_model$finalModel)
check_equality_with_manual_model(manual_model = manual_model, caret_model = nnfor_model)
check_equality_with_manual_model(manual_model = manual_model, caret_model = nnfor_model2)

## self$get_tabular_hypertuning_results()
cat("\n\n------------------------------")
cat("\nHyperparameter Tuning Results:")
cat("\n------------------------------\n\n")
print(nnfor_model$results)

## self$plot_perf_across_hyperparameters()
## Plots the metric vs. hyperparameters

# Opt 1
print(ggplot2::ggplot(nnfor_model))

# Opt 2
trellis.par.set(caretTheme())
plot(nnfor_model)

# Opt 3 (Useful for grid searches not for random)
trellis.par.set(caretTheme())
plot(nnfor_model, metric = "ASE", plotType = "level",
     scales = list(x = list(rot = 90)))

## self$get_best_hyper_parameters()
cat("\n\n---------------------")
cat("\nBest Hyperparameters:")
cat("\n---------------------\n\n")
print(nnfor_model$bestTune)

## self$get_final_model()  - also needs another one called self$get_caret_model
cat("\n\n--------------")
cat("\nFinal Model:")
cat("\n--------------\n\n")
print(nnfor_model$finalModel)


# Per Batch Results

cat("\n\n------------------------")
cat("\nMetrics for each batch:")
cat("\n------------------------\n\n")

print(nnfor_model$resample)

cat("\n\n----------------------------")
cat("\nPredictions for each batch:")
cat("\n----------------------------\n\n")
print(nnfor_model$pred)

## get_tabular_results(ases = TRUE)

ases = nnfor_model$resample 

batches = ases  %>% 
  dplyr::distinct(Resample) %>% 
  dplyr::arrange(Resample) %>% 
  dplyr::mutate(Batch = dplyr::row_number())

ases = ases %>% 
  dplyr::left_join(batches, by = "Resample") %>% 
  dplyr::select(-RMSE)

print(ases %>% 
        dplyr::mutate(combined = paste0(reps, "_", hd, "_", allow.det.season)) %>% 
        ggplot2::ggplot(aes(y = ASE, x = combined, color = combined)) + geom_boxplot() + 
        stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="red", fill="red") +
        coord_flip())


## get_tabular_results(ases = FALSE)

preds = nnfor_model$pred

batches = preds  %>% 
  dplyr::distinct(Resample) %>% 
  dplyr::arrange(Resample) %>% 
  dplyr::mutate(Batch = dplyr::row_number())

preds = preds %>% 
  dplyr::left_join(batches, by = "Resample") %>% 
  dplyr::select(-Resample) %>% 
  dplyr::rename(Time = rowIndex)

## TODO: Rename pred, obs, rowIndex appropriately



#### Manual Grid Search ####

search = "grid"
nnfor_grid = expand.grid(reps = c(20, 30),
                         hd = 1:2,
                         allow.det.season = c(TRUE, FALSE))

fitControl = get_fit_control(initialWindow = initialWindow, h = h, search = search, parallel = parallel)

# http://sshaikh.org/2015/05/06/parallelize-machine-learning-in-r-with-multi-core-cpus/
if (parallel == TRUE){
  num_cores = parallel::detectCores()
  cl = parallel::makeCluster(ifelse(num_cores == 1, 1, num_cores -1)) # Leave one core out
  doParallel::registerDoParallel(cl)
}

tic("- Total Time for training: ")

set.seed(101)
# Must pass a dataframe to caret::train, hence we need to specify the frequency of y separately using 'm'
nnfor_model = caret::train(logGNP ~ ., data = data,
                           method = nnfor_caret,
                           trControl = fitControl,
                           tuneGrid = nnfor_grid,
                           metric = "ASE", maximize = FALSE,
                           # Additional Params to be passed to model
                           ## necessary to pass the frequency 'm' of variable of interest, else lags may not get picked
                           m = 4,
                           ## Rest of the variables passed below are optional
                           lags = 1:4,
                           keep = c(TRUE, FALSE, FALSE, TRUE),
                           difforder = c(1),
                           xreg.lags = list(1:2, 1:2, 1:2)
)

set.seed(101)
# Must pass a dataframe to caret::train, hence we need to specify the frequency of y separately using 'm'
nnfor_model2 = caret::train(logGNP ~ ., data = data,
                           method = nnfor_caret,
                           trControl = fitControl,
                           tuneGrid = nnfor_grid,
                           metric = "ASE", maximize = FALSE,
                           # Additional Params to be passed to model
                           ## necessary to pass the frequency 'm' of variable of interest, else lags may not get picked
                           m = 4,
                           ## Rest of the variables passed below are optional
                           lags = 1:4,
                           keep = c(TRUE, FALSE, FALSE, TRUE),
                           difforder = c(1),
                           xreg.lags = list(1:2, 1:2, 1:2)
)


toc()

if(parallel == TRUE){
  stopCluster(cl)
  registerDoSEQ()
}


## Check for reproducibility of model
## Caret gives final model on the full data
set.seed(1)

manual_model = nnfor::mlp(y = stats::ts(data %>% dplyr::select(logGNP) %>% purrr::pluck()),
                          xreg = data %>% dplyr::select(-logGNP),
                          reps = nnfor_model$bestTune$reps,
                          hd = nnfor_model$bestTune$hd,
                          allow.det.season = nnfor_model$bestTune$allow.det.season,
                          # Additional Params to be passed to model
                          m = 4,
                          ## Rest of the variables passed below are optional
                          lags = 1:4,
                          keep = c(TRUE, FALSE, FALSE, TRUE),
                          difforder = c(1),
                          xreg.lags = list(1:2, 1:2, 1:2)
                          )

# Check equality of 2 separate caret runs
all.equal(predict(nnfor_model), predict(nnfor_model2))

# Check equality of caret run with manual run
print(manual_model)
print(nnfor_model$finalModel)
check_equality_with_manual_model(manual_model = manual_model, caret_model = nnfor_model)
check_equality_with_manual_model(manual_model = manual_model, caret_model = nnfor_model2)


cat("\n\n------------------------------")
cat("\nHyperparameter Tuning Results:")
cat("\n------------------------------\n\n")
print(nnfor_model$results)

## Plots the metric vs. hyperparameters
# Opt 1
print(ggplot2::ggplot(nnfor_model))

# Opt 2
trellis.par.set(caretTheme())
plot(nnfor_model)

# Opt 3 (Useful for grid searches not for random)
trellis.par.set(caretTheme())
plot(nnfor_model, metric = "ASE", plotType = "level",
     scales = list(x = list(rot = 90)))

cat("\n\n---------------------")
cat("\nBest Hyperparameters:")
cat("\n---------------------\n\n")
print(nnfor_model$bestTune)

cat("\n\n--------------")
cat("\nFinal Model:")
cat("\n--------------\n\n")
print(nnfor_model$finalModel)

