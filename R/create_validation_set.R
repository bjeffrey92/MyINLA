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
    
    if (covariate_change != 'Constant'){
        stop('Only constant covariate model is implemented currently')
    }

    #splits validation data 
    last_time_points <- tail(sort(unique(data[[time_col]])), prediction_steps)
    validation_data <- data[data[[time_col]] %in% last_time_points,]
    
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
}