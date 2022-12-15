#' @rdname arraylength
#' @exportMethod nlayers
setMethod(
	"nlayers",
	signature="RastArray",
	function(x){
	 dims <- dim(x@stack)
	 return(dims[3])
	}
)


