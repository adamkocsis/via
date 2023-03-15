#' @rdname arraylength
#' @exportMethod nlayers
setMethod(
	"nlayers",
	signature="RasterArray",
	function(x){
	 dims <- dim(x@stack)
	 return(dims[3])
	}
)


#' Number of cells in a '\code{\link[via:RasterArray-class]{RasterArray}}'-class object
#' 
#' The method is inherited from the '\code{\link[terra:rast]{SpatRaster}}' class.
#' 
#' @param x a \code{\link[via:RasterArray-class]{RasterArray}} class object.
#' @rdname ncell
#' @return A \code{numeric} value.
#' @examples
#' ex <- rastex()
#' ncell(ex)
#' @exportMethod ncell
setMethod(
	"ncell",
	signature="RasterArray",
	function(x){
		if(!requireNamespace("terra", quietly=TRUE)){
			stop("This function requires the terra package.")
		}	
		terra::ncell(x@stack)
	}
)



#' @rdname nvalues
setMethod(
	"nvalues", 
	signature="RasterArray", 
	function(x){
		# returns the layer names
		prod(dim(x@stack))

	} 
)




#' @rdname dimlayer
setMethod(
	"dimlayer", 
	signature="RasterArray", 
	function(x){
		# depends on subset-method
		dim(x@stack[[1]])[1:2]

	} 
)
