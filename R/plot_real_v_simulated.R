#' PLot real versus model simulated data
#'
#' @param result fitted inla model
#' @param stack data stack
#' @param Data data model was fitted to
#' @param response_col name of column in data containing response data
#' @param filename file for plot to be saved to, default FALSE
#' @export
plot_real_v_simulated <- function(result, stack.est, Data, response_col, 
                                filename=NULL){
    if (!(is.null(filename))){
        png(filename)
    
        INLA::inla.stack.index(stack.est,"est")$data
        lp <- result$summary.linear.predictor$mean[index]
        plot(lp[index],Data[[response_col]])

        dev.off()
    }else{
        index <- INLA::inla.stack.index(stack.est,"est")$data
        lp <- result$summary.linear.predictor$mean[index]
        plot(lp[index],Data[[response_col]])
    }
}