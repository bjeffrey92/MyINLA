#' Computes multiple statistics describing the model fit
#'
#' @param ValidationData dataframe containing the validation data
#' @param response_col names of response column
#' @param stack.est stack.est object
#' @export
validate_model_fit <- function(ValidationData, response_col, stack.est){
    validation <- list()

    index <- inla.stack.index(stack.est,"est")$data
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

    return(validation)
}