#' @rdname replacementSingle
#' @exportMethod "[<-"
setReplaceMethod(
	"[", 
	signature(x="SfArray", value="sf"),
	definition=function(x,i,j,..., value){
		x<- XArrayReplaceLayer(x=x, i=i, j=j, value=value, ...)
		return(x)
	}
)


