#' Computes multiple statistics describing the model fit
#'
#' @param result fitted inla model
#' @param Data dataframe containing all variables and covariates and time and location of each observation
#' @param time_col name of column containing time point of each observation
#' @param location_col name of column containing location of each observation
#' @param ValidationData dataframe containing the validation data
#' @param response_col names of response column
#' @param prediction_steps how many steps ahead are being predicted, needed for generating naive forecast for comparison
#' @param stack.est stack.est object
#' @export
validate_model_fit <- function(result, Data, ValidationData, response_col, time_col, location_col, 
                                stack.est, prediction_steps=0){
    validation <- list()

    if (prediction_steps==0){ #if not doing any prediction calculates validation stats for fit of training data
        index <- INLA::inla.stack.index(stack.est,"est")$data
    }else{
        index <- as.integer(row.names(ValidationData))

        last_time_points <- tail(sort(unique(Data[[time_col]])), prediction_steps)
        most_recent_time <- tail(sort(unique(Data[[time_col]])), 
                                    prediction_steps+1)[[1]]
      
        for (location in Data[[location_col]]){
            most_recent_response <- 
                        Data[Data[[location_col]] == location &
                            Data[[time_col]] == most_recent_time,][[response_col]]

            Data[Data[[time_col]] %in% last_time_points &
                Data[[location_col]] == location,][[response_col]] <- most_recent_response
            
            naive_prediction <- 
                    Data[Data[[time_col]] %in% last_time_points,][[response_col]]
        }
    }

    tmp.mean <- result$summary.linear.predictor[index,"mean"]
    tmp.sd <- result$summary.linear.predictor[index,"sd"]
    
    validation$res <- ValidationData[[response_col]] - tmp.mean #calculates model residual
    validation$res.std <- validation$res /sqrt(tmp.sd^2 + 
                    1/result$summary.hyperpar[1,"mean"]) 
                    
    validation$p = pnorm(validation$res.std)
    validation$cover = mean((validation$p>0.025) & (validation$p<0.975), 
                    na.rm=TRUE) #actual coverage prob

    validation$rmse = sqrt(mean(validation$res^2, na.rm=TRUE)) #RMSE
    validation$cor = cor(ValidationData[[response_col]], tmp.mean,
                    use="pairwise.complete.obs", method="pearson") #Pearson correlation coefficient between observations and model fit

    if (prediction_steps > 0){
        validation$naive.res <- ValidationData[[response_col]] - naive_prediction
        
        validation$naive.rmse = sqrt(mean(validation$naive.res^2, na.rm=TRUE)) #RMSE
        validation$naive.cor = cor(ValidationData[[response_col]], naive_prediction,
                    use="pairwise.complete.obs", method="pearson") #Pearson correlation c
    }

    return(validation)
}