#' @rdname replacementSingle
#' @exportMethod "[<-"
setReplaceMethod(
	"[", 
	signature(x="RastArray", value="SpatRaster"),
	definition=function(x,i,j,..., value){
		x<- XArrayReplaceLayer(x=x, i=i, j=j, value=value, ...)
		return(x)
	}
)


