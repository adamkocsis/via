#' @rdname st_crs
#' @method st_crs SfArray
#' @export 
st_crs.SfArray <- function(x,...){
	# CRS should be the same in the entire stack
	if(!requireNamespace("sf", quietly=TRUE)) stop("This function requires the 'sf' package to run.")
	crs <- sf::st_crs(x@stack[[1]],...)
	return(crs)

}

#' @rdname st_transform
#' @method st_transform SfArray
#' @export 
st_transform.SfArray <- function(x,...){
	# CRS should be the same in the entire stack
	if(!requireNamespace("sf", quietly=TRUE)) stop("This function requires the 'sf' package to run.")
	for(i in 1:length(x@stack)){
		x@stack[[i]]<- sf::st_transform(x@stack[[i]],...)
	}
	return(x)
}
