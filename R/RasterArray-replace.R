#' @rdname replacementSingle
#' @exportMethod "[<-"
setReplaceMethod(
	"[", 
	signature(x="RasterArray", value="SpatRaster"),
	definition=function(x,i,j,..., value){
		x<- VirtualArrayReplaceLayer(x=x, i=i, j=j, value=value, ...)
		return(x)
	}
)


