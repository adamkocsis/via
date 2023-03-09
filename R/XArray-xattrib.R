#' @rdname arraylength
#' @exportMethod nlayers
setMethod(
	"nlayers",
	signature="XArray",
	function(x) length(x@stack)
)


