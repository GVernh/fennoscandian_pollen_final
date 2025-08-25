crossvalidation = function(LCC, SPD, clim){
  varnames <- c(deparse(substitute(LCCadapt)), deparse(substitute(SPD)), deparse(substitute(clim)))
  lagnames <- paste0(varnames, "_lag", rep(1:maxlag, each = 3))
  # Then we created the lagged variables / data for models
  VAR_data <- stats::embed(as.matrix(cbind(LCC, SPD, clim)), maxlag + 1)
  colnames(VAR_data) <- c(varnames, lagnames)
  VAR_data <- as.data.frame(VAR_data[, -c(2,3)])
  
  dataconN_cv  <- modelr::crossv_kfold(VAR_data, k = nfolds) # just an old structure that we'll fill out differently
  
  chunksize = ceiling(nrow(VAR_data)/(nfolds+1))
  BP = -yearsBP[(maxlag + 1):length(yearsBP)] # so that time goes up with time
  VAR_data_ext = cbind(BP,VAR_data)
  tscv <- timetk::time_series_cv(data = VAR_data_ext, date_var = BP, initial = chunksize, skip = chunksize - 1, assess = chunksize - 1, cumulative = TRUE)
  
  for(i in 1:nfolds){
    dataconN_cv$train[[i]]$idx <- tscv$splits[[i]]$in_id
    dataconN_cv$test[[i]]$idx <- tscv$splits[[i]]$out_id
    dataconN_cv$train[[i]]$idx
    dataconN_cv$test[[i]]$idx
  }
  # the full model with LCC, SPD and clim
  formula7 <- paste(colnames(VAR_data)[-1], collapse = " + ")
  formula7 <- stats::formula(paste0(varnames[1], " ~ ", formula7))
  
  formula6 <- dplyr::select(VAR_data,-contains("7"))
  formula6 <- paste(colnames(formula6)[-1], collapse = " + ")
  formula6 <- stats::formula(paste0(varnames[1], " ~ ", formula6))
  
  formula5 <- dplyr::select(VAR_data,-contains(c("7", "6")))
  formula5 <- paste(colnames(formula5)[-1], collapse = " + ")
  formula5 <- stats::formula(paste0(varnames[1], " ~ ", formula5))
  
  formula4 <- dplyr::select(VAR_data,-contains(c("7", "6", "5")))
  formula4 <- paste(colnames(formula4)[-1], collapse = " + ")
  formula4 <- stats::formula(paste0(varnames[1], " ~ ", formula4))
  
  formula3 <- dplyr::select(VAR_data,-contains(c("7", "6", "5", "4")))
  formula3 <- paste(colnames(formula3)[-1], collapse = " + ")
  formula3 <- stats::formula(paste0(varnames[1], " ~ ", formula3))
  
  formula2 <- dplyr::select(VAR_data,-contains(c("7", "6", "5", "4", "3")))
  formula2 <- paste(colnames(formula2)[-1], collapse = " + ")
  formula2 <- stats::formula(paste0(varnames[1], " ~ ", formula2))
  
  formula1 <- dplyr::select(VAR_data,-contains(c("7", "6", "5", "4", "3", "2")))
  formula1 <- paste(colnames(formula1)[-1], collapse = " + ")
  formula1 <- stats::formula(paste0(varnames[1], " ~ ", formula1))
  
  model7  <- purrr::map(dataconN_cv$train, ~glm2(formula7, data = .))
  model6  <- purrr::map(dataconN_cv$train, ~glm2(formula6, data = .))
  model5  <- purrr::map(dataconN_cv$train, ~glm2(formula5, data = .))
  model4  <- purrr::map(dataconN_cv$train, ~glm2(formula4, data = .))
  model3  <- purrr::map(dataconN_cv$train, ~glm2(formula3, data = .))
  model2  <- purrr::map(dataconN_cv$train, ~glm2(formula2, data = .))
  model1  <- purrr::map(dataconN_cv$train, ~glm2(formula1, data = .))
  
  get_pred  <- function(model, test_data){
    data  <- as.data.frame(test_data)
    pred  <- modelr::add_predictions(data, model)
    return(pred)
  }
  pred7  <- purrr::map2_df(model7, dataconN_cv$test, get_pred, .id = "Run")
  pred6  <- purrr::map2_df(model6, dataconN_cv$test, get_pred, .id = "Run")
  pred5  <- purrr::map2_df(model5, dataconN_cv$test, get_pred, .id = "Run")
  pred4  <- purrr::map2_df(model4, dataconN_cv$test, get_pred, .id = "Run")
  pred3  <- purrr::map2_df(model3, dataconN_cv$test, get_pred, .id = "Run")
  pred2  <- purrr::map2_df(model2, dataconN_cv$test, get_pred, .id = "Run")
  pred1  <- purrr::map2_df(model1, dataconN_cv$test, get_pred, .id = "Run")
  
  MSE7  <- pred7 %>% group_by(Run) %>%
    dplyr::summarise(MSE = mean( (LCCadapt - pred)^2))
  
  MSE6  <- pred6 %>% group_by(Run) %>%
    dplyr::summarise(MSE = mean( (LCCadapt - pred)^2))
  
  MSE5  <- pred5 %>% group_by(Run) %>%
    dplyr::summarise(MSE = mean( (LCCadapt - pred)^2))
  
  MSE4  <- pred4 %>% group_by(Run) %>%
    dplyr::summarise(MSE = mean( (LCCadapt - pred)^2))
  
  MSE3  <- pred3 %>% group_by(Run) %>%
    dplyr::summarise(MSE = mean( (LCCadapt - pred)^2))
  
  MSE2  <- pred2 %>% group_by(Run) %>%
    dplyr::summarise(MSE = mean( (LCCadapt - pred)^2))
  
  MSE1  <- pred1 %>% group_by(Run) %>%
    dplyr::summarise(MSE = mean( (LCCadapt - pred)^2))
  
  v1 = c("lag1", "lag2", "lag3", "lag4", "lag5", "lag6", "lag7")
  v2 = c(MSE1=mean(MSE1$MSE), MSE2=mean(MSE2$MSE), MSE3=mean(MSE3$MSE), MSE4=mean(MSE4$MSE), MSE5=mean(MSE5$MSE), MSE6=mean(MSE6$MSE), MSE7=mean(MSE7$MSE))
  list = data.frame(v1, v2)
  return(list[which.min(list$v2),])
  #return(list(MSE1=mean(MSE1$MSE), MSE2=mean(MSE2$MSE), MSE3=mean(MSE3$MSE), MSE4=mean(MSE4$MSE), MSE5=mean(MSE5$MSE), MSE6=mean(MSE6$MSE), MSE7=mean(MSE7$MSE)))
}