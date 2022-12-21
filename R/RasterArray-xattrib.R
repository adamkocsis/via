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


#' Number of cells in a RasterArray object
#' 
#' The method is inherited from the \code{RasterStack} class.
#' 
#' @param x a \code{RasterArray} class object.
#' @rdname ncell
#' @return A \code{numeric} value.
#' @examples
#' data(dems)
#' ncell(dems)
#' @exportMethod ncell
setMethod(
	"ncell",
	signature="RasterArray",
	function(x) ncell(x@stack)
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
