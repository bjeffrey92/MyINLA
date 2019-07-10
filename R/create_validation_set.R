#' Splits data validation data 
#'
#' @param Data dataframe containing all variables and covariates and time and location of each observation
#' @param time_col name of column containing time point of each observation
#' @param location_col name of column containing location of each observation
#' @param response_col name of column containing response
#' @param covariate_cols name of column containing the value for ab consumption
#' @param prediction_steps number of time steps ahead to predict (assumes intervals are constant)
#' @param covariate_change Default is Constant. Will assume that ab consumption remains same as last observed time point
#' @return A list containing training data and the validation data
#' @export
create_validation_set <- function(data, time_col, location_col, response_col, 
                                covariate_cols, prediction_steps, 
                                covariate_change='Constant'){
    
    #splits validation data 
    last_time_points <- tail(sort(unique(data[[time_col]])), prediction_steps)
    validation_data <- data[data[[time_col]] %in% last_time_points,]

    if (covariate_change == 'Constant'){
        most_recent_time <- tail(sort(unique(data[[time_col]])), 
                                prediction_steps+1)[[1]]

        for (location in unique(data[[location_col]])){

            for (covariate in covariate_cols){

                #data will assume covaraiates stay the same as last recorded time point
                most_recent_covar <- data[data[[location_col]] == location &
                                        data[[time_col]] == most_recent_time,][[covariate]]
                data[data[[time_col]] %in% last_time_points &
                    data[[location_col]] == location,][[covariate]] <- most_recent_covar
            }
            data[data[[time_col]] %in% last_time_points,][[response_col]] <- NA
        }
        return(list(data, validation_data))

    }else if (covariate_change == 'ETS'){
        
        previous_times <- c()
        for (i in unique(data[[time_col]])){
            if (!(i %in% last_time_points)){
                previous_times <- c(previous_times, i)
            }
        }

        for (location in unique(data[[location_col]])){

            for (covariate in covariate_cols){

                previous_covars <- data[data[[location_col]] == location &
                                    data[[time_col]] %in% previous_times,][[covariate]]
                covar_ts <- ts(previous_covars, start = min( previous_times))

                ets_prediction <- forecast::forecast(forecast::ets(covar_ts),
                                            h=prediction_steps)
                ets_prediction <- as.numeric(ets_prediction$mean)

                for (i in 1:length(last_time_points)){
                    data[data[[time_col]] == last_time_points[[i]] &
                        data[[location_col]] == location,][[covariate]] <- ets_prediction[[i]]
                }
            }
        }
        return(list(data, validation_data))
    } else{
        stop('Unknown value of covariate_change, use Constant or ETS')
    }
}