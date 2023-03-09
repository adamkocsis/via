#' @rdname replacementSingle
#' @exportMethod "[<-"
setReplaceMethod(
	"[", 
	signature(x="SfArray", value="sf"),
	definition=function(x,i,j,..., value){
		x<- VirtualArrayReplaceLayer(x=x, i=i, j=j, value=value, ...)
		return(x)
	}
)


