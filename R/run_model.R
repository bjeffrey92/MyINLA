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