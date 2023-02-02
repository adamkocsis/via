#' @rdname arraylength
#' @exportMethod nlayers
setMethod(
	"nlayers",
	signature="ViArray",
	function(x) length(x@stack)
)


