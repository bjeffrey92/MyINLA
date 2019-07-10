#' Write summary of multiple model fits to a dataframe
#'
#' @param all_data_files list of file names all containing one fited inla model object
#' @param all_model_fits list of fitted inla model objects
#' @return dataframe containing aggregated fit summaries
#' @export

write_summary_of_fits <- function(all_data_files = NULL, 
                                    all_model_fits = NULL){
    
    # if (is.null(all_data_files) and is.null(all_model_fits)){
    #     stop('Please provide either a list of model fits or paths containing rds files of model fits')    
    # }else if(!(is.null(all_data_files)) and !(is.null(all_model_fits)){
    #     stop('Cannot use both model fit objects and data files')
    # }else if(!is.null(all_data_files)){
    #     all_model_fits <- vector(mode = 'list', length=length(all_data_files))
    #     pos <- 1
    #     for (i in all_data_files){
    #         all_model_fits[[pos]] <- readRDS(all_data_files)
    #         pos <- pos + 1
    #     }
    # } 

    all_model_fits <- vector(mode = 'list', length=length(all_data_files))
    pos <- 1
    for (i in all_data_files){
        all_model_fits[[pos]] <- list(readRDS(i), i)
        pos <- pos + 1
    }

    all_rows <- list()
    pos <- 1
    all_validation_rows <- list()
    validation_pos <- 1
    for (item in all_model_fits){
        result <- item[[1]][['result']]
        filename <- item[[2]]
     
        for (row in 1:nrow(result$summary.fixed)){
            new_row <- c(row.names(result$summary.fixed)[[row]],
                        head(as.numeric(result$summary.fixed[row,]),6), 
                        filename)
            all_rows[[pos]] <- new_row
            pos <- pos + 1
        }
        for (row in 1:nrow(result$summary.hyperpar)){
            new_row <- c(row.names(result$summary.hyperpar)[[row]],
                        as.numeric(result$summary.hyperpar[row,]),
                        filename)
            all_rows[[pos]] <- new_row
            pos <- pos + 1
        }

        if ('validation' %in% names(item[[1]])){
            validation <- item[[1]][['validation']]
            for (i in 1:length(validation)){
                if (length(validation[[i]])== 1){
                    new_row <- c(names(validation)[[i]],
                                validation[[i]],
                                filename)
                    all_validation_rows[[validation_pos]] <- new_row
                    validation_pos <- validation_pos + 1
                }
            }
        }        
    }
    out <- do.call('rbind', all_rows)
    out_df  <- as.data.frame(out)
    names(out_df) <- c('Parameter', names(result$summary.hyperpar), 'filename')
    
    if (length(all_validation_rows) > 0){
        validation_df <- as.data.frame(
                                    do.call('rbind', all_validation_rows))
        names(validation_df) <- c('Measure', 'Value', 'filename')
        return(list(out_df, validation_df))
    }else{
        return(out_df)
    }
}