#' Build data stack and formula for AR model
#'
#' @param Data dataframe containing all variables and covariates and time and location of each observation
#' @param x_col name of column containing position on x access (longitude) of each location
#' @param y_col name of column containing position on y access (latitude) of each location
#' @param time_col name of column containing time point of each observation
#' @param response_col name of column containing variable of interest
#' @param order order of autoregressive model
#' @param covariate_cols vector containing name of all columns containing covariates
#' @param mesh2d 2d mesh 
#' @param mesh1d 1d mesh
#' @return list containing the data stack and the formula 
#' @export
build_stack_and_formula <- function(Data, x_col, y_col, time_col, response_col, 
								order, covariate_cols, mesh2d, mesh1d){

	A.est <-
	    INLA::inla.spde.make.A(mesh2d,
                        loc=as.matrix(cbind(Data[[x_col]],Data[[y_col]])),
                        group=Data[[time_col]],
                        group.mesh=mesh1d
                        )

	field.indices <-
        	INLA::inla.spde.make.index("field",
                             n.spde=mesh2d$n,n.group=mesh1d$m)

	if (length(covariate_cols == 0)){
		covariates <- TRUE
	}

	if (covariates){
		covariates_matrix <- Data[[covariate_cols[[1]]]]
		if (length(covariate_cols) > 1){
			for (i in 2:length(covariate_cols)){
				covariates_matrix <- cbind(covariates_matrix, Data[[covariate_cols[[i]]]])
			}
		}
		covariates_df <- as.data.frame(covariates_matrix)
		names(covariates_df) <- covariate_cols

		stack.est <-
				INLA::inla.stack(data=list(response=Data[[response_col]]),
						A=list(A.est, 1),
						effects=
						list(c(field.indices,
							list(Intercept=1)),
							list(covariates_df)),
						tag="est", remove.unused = FALSE)

		stack <- INLA::inla.stack(stack.est) #final stack
		
		formula <- paste('(response ~ -1 + Intercept + ', 
							paste(covariate_cols, collapse=' + '))
		a <- paste('+ f(field, model=spde, group=field.group,
						control.group=list(model="ar", order=', as.character(order))
		b <- paste(a,')))')
		formula <- paste(formula, b)

	}else{
		stack.est <-
				INLA::inla.stack(data=list(response=Data[[response_col]]),
						A=list(A.est, 1),
						effects=
						list(c(field.indices,
							list(Intercept=1))
							),
						tag="est", remove.unused = FALSE)

		stack <- INLA::inla.stack(stack.est) #final stack

		formula <- response ~ -1 + Intercept +
					INLA::f(field, model=spde, group=field.group,
						control.group=list(model="ar", order=order))
	}

	output_list <- list()
	output_list[['stack']] <- stack
	output_list[['stack.est']] <- stack.est
	output_list[['formula_string']] <- formula
	return(output_list)
}
