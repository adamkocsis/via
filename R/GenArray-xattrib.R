#' @rdname arraylength
#' @exportMethod nlayers
setMethod(
	"nlayers",
	signature="GenArray",
	function(x) length(x@stack)
)


