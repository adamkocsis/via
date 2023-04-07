#' @rdname replacementSingle
#' @exportMethod "[<-"
setReplaceMethod(
	"[", 
	signature(x="SfcArray", value="sfc"),
	definition=function(x,i,j,..., value){
		x<- VirtualArrayReplaceLayer(x=x, i=i, j=j, value=value, ...)
		return(x)
	}
)


