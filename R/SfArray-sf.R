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

#' @rdname st_bbox
#' @method st_bbox SfArray
#' @export 
st_bbox.SfArray <- function(x,...){
	# CRS should be the same in the entire stack
	if(!requireNamespace("sf", quietly=TRUE)) stop("This function requires the 'sf' package to run.")
	m <- NULL
	for(i in 1:length(x@stack)){
		m<- rbind(m, sf::st_bbox(x@stack[[i]]))
	}
	final <- sf::st_bbox(x@stack[[1]])
	final["xmin"] <- min(m[,"xmin"])
	final["ymin"] <- min(m[,"ymin"])
	final["xmax"] <- max(m[,"xmax"])
	final["ymax"] <- max(m[,"ymax"])
	
	return(final)
}
