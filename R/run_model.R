#' Run INLA model
#'
#' @param stack Data stack 
#' @param formula ar formula
#' @param likelihood default gaussian
#' @return result of inla model
#' @export
run_model <- function(stack, formula, spde, likelihood="gaussian"){
    formula <- as.formula(formula)
    
    result <-
        INLA::inla(formula,   
                data=INLA::inla.stack.data(stack, spde=spde),
                family=likelihood,
                control.predictor=list(A=INLA::inla.stack.A(stack), compute=TRUE),
                control.compute=list(cpo=FALSE),
                control.inla = list(reordering = "metis"),
                keep=FALSE, verbose=TRUE)
    return(result)
}

# coeff < -result$summary.fixed$mean
# field <- result$summary.random$field$mean

# prediction_table = matrix where each row is a time and location and columns are values of covariates

# prediction_table%*%coeff + drop (A_est%*%field)

# length(field)
# mesh$n

# result$summary.linear.predictor$mean[index]