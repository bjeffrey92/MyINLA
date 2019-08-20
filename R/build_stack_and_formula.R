#' Build data stack and formula for AR model
#'
#' @param Data dataframe containing all variables and covariates and time and location of each observation
#' @param x_col name of column containing position on x access (longitude) of each location
#' @param y_col name of column containing position on y access (latitude) of each location
#' @param time_col name of column containing time point of each observation
#' @param response_col name of column containing variable of interest
#' @param order order of autoregressive model
#' @param covariate_cols vector containing name of all columns containing covariates
#' @param ValidationData dataframe containing the data for validating the predictions
#' @param prediction_steps Number of steps ahead to be predicted, validation data must also be provided
#' @param mesh2d 2d mesh 
#' @param mesh1d 1d mesh
#' @return list containing the data stack and the formula 
#' @export
build_stack_and_formula <- function(Data, x_col, y_col, time_col, response_col, 
									order, covariate_cols, ValidationData=NULL, 
									prediction_steps=0, mesh2d, mesh1d){

	if (prediction_steps > 0 & is.null(ValidationData)){
		stop('Please provide validation data if prediction_steps > 0')
	}

	A.est <-
	    INLA::inla.spde.make.A(mesh2d,
                        loc=as.matrix(cbind(Data[[x_col]],Data[[y_col]])),
                        group=Data[[time_col]],
                        group.mesh=mesh1d
                        )
	
	if (prediction_steps > 0){

		A.val <-
			INLA::inla.spde.make.A(mesh2d,
							loc=as.matrix(cbind(ValidationData[[x_col]],
												ValidationData[[y_col]])),
							group=ValidationData[[time_col]],
							group.mesh=mesh1d
							)

		A.pred <-
			INLA::inla.spde.make.A(mesh2d, group=ValidationData[[time_col]], 
							group.mesh=mesh1d, index = 1:nrow(ValidationData))
	}

	field.indices <-
        	INLA::inla.spde.make.index("field",
                             n.spde=mesh2d$n,n.group=mesh1d$m)

	if (length(covariate_cols < 0)){
		covariates <- TRUE
	}else{
		covariates <- FALSE
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
							list(Intercept=0)),
							list(covariates_df)),
						tag="est", remove.unused = FALSE)

		if (prediction_steps > 0){

			validation_covariates_matrix <- ValidationData[[covariate_cols[[1]]]]
			if (length(covariate_cols) > 1){
				for (i in 2:length(covariate_cols)){
					validation_covariates_matrix <- cbind(validation_covariates_matrix, 
											ValidationData[[covariate_cols[[i]]]])
				}
			}
			validation_covariates_df <- as.data.frame(validation_covariates_matrix)
			names(validation_covariates_df) <- covariate_cols

			stack.val <-
					INLA::inla.stack(data=list(response=NA),
							A=list(A.val, 1),
							effects=
							list(c(field.indices,
									list(Intercept=0)),
									list(validation_covariates_df)),
							tag="val")

			stack.pred <-
					INLA::inla.stack(data=list(response=NA),
							A=list(A.pred),
							effects=
							list(c(field.indices,
									list(Intercept=0))),
							tag="pred")

			stack <- INLA::inla.stack(stack.est, stack.val, stack.pred) #final stack
		}else{
			stack <- INLA::inla.stack(stack.est) #final stack
		}
		
		formula <- paste('(response ~ -1 + Intercept + ', 
							paste(covariate_cols, collapse=' + '))
		a <- paste('+ f(field, model=spde, group=field.group,
						control.group=list(model="ar", order=', as.character(order))
		b <- paste(a,')))')
		formula <- paste(formula, b)

	}else{
		stack.est <-
				INLA::inla.stack(data=list(response=Data[[response_col]]),
						A=list(A.est),
						effects=
						list(c(field.indices,
							list(Intercept=0))
							),
						tag="est", remove.unused = FALSE)

		if (prediction_steps > 0){
			stack.val <-
					INLA::inla.stack(data=list(response=NA),
							A=list(A.val),
							effects=
							list(c(field.indices,
									list(Intercept=0))),
							tag="val")

			stack.pred <-
					INLA::inla.stack(data=list(response=NA),
							A=list(A.pred),
							effects=
							list(c(field.indices,
									list(Intercept=0))),
							tag="pred")

			stack <- INLA::inla.stack(stack.est, stack.val, stack.pred) #final stack
		}

		stack <- INLA::inla.stack(stack.est) #final stack

		formula <- paste0('response ~ -1 + Intercept +
						f(field, model=spde, group=field.group,
						control.group=list(model="ar", order=', as.character(order)) 
		formula <- paste0(formula, '))')
	}

	output_list <- list()
	output_list[['formula_string']] <- formula
	output_list[['stack']] <- stack
	output_list[['stack.est']] <- stack.est
	if (prediction_steps > 0){
		output_list[['stack.val']] <- stack.val
		output_list[['stack.pred']] <- stack.pred
	}
	return(output_list)
}
