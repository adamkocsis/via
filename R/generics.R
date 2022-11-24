#' Names of \code{\link{RasterArray}} or \code{\link{SpatialArray}} Layers in the stack
#' 
#' @param x A \code{\link{RasterArray}} or \code{\link{SpatialArray}} class object.
#' @param ... additional arguments passed to class-specific methods.
#' @return A \code{character} vector of names.
#' @exportMethod layers
#' 
#' @examples
#' # names of layers in the stack
#' data(dems)
#' layers(dems)
#' @rdname layers
setGeneric("layers", function(x,...) standardGeneric("layers"))





setGeneric(
	name="nlayers",
	def=function(x){
		standardGeneric("nlayers")
	}
)

