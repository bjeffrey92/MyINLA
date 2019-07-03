#' Build 1d and 2d mesh for INLA model
#'
#' @param Data dataframe containing all variables and covariates and time and location of each observation
#' @param x_col name of column containing position on x access (longitude) of each location
#' @param y_col name of column containing position on y access (latitude) of each location
#' @param time_col name of column containing time point of each observation
#' @param predict_forwards number of time steps to predict forwards 
#' @param two_dim build 2d mesh (TRUE/FALSE)
#' @param one_dim build 1d mesh (TRUE/FALSE)
#' @return a list containing either the 1d mesh, 2d mesh or both 
#' @export
build_mesh_and_spde <- function(Data, x_col, y_col, time_col, predict_forwards=NULL,
		       two_dim=TRUE, one_dim=TRUE){
	
	if (two_dim){
		locations_matrix <- cbind(unique(Data[[x_col]]),
					  unique(Data[[y_col]]))
		mesh2d <- INLA::inla.mesh.2d(loc=locations_matrix, 
                    		cutoff = 0.2, min.angle=c(25,25),
				max.edge=2) 
	}

	if (is.null(predict_forwards)){
		predict_forwards <- length(unique(Data[[time_col]]))
	}else if (!is.null(predict_forwards) & !(one_dim)){
		stop('Cannot make forward predictions without specifying 1d mesh.
		     Set one_dim=TRUE')
	}

	if (one_dim){
		mesh1d <- INLA::inla.mesh.1d(seq(1,length(unique(Data[[time_col]])),by=1),
		        	interval=c(1,length(unique(Data[[time_col]])) + predict_forwards),
				degree=2, boundary=c('free'))	
	}


	output_list <- list()
	if (exists('mesh2d')){
		spde <- INLA::inla.spde2.matern(mesh=mesh2d)
		output_list[['spde']] <- spde
		output_list[['mesh2d']] <- mesh2d
	}
	if (exists('mesh1d')){
		output_list[['mesh1d']] <- mesh1d
	}

	return(output_list)
}
