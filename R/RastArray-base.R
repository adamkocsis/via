
# build from existing stack with existing index, or dimensions
#' @export RastArray
setMethod("initialize",signature="RastArray",
	definition=function(.Object,stack, index=NULL, dim=NULL){
		# some defense for index
		if(is.null(dim)){ 
			if(class(stack)!="SpatRaster") stop("The 'stack' has to be a 'SpatRaster' - class object.")
			if(!is.numeric(index)) stop("The 'index' has to be a 'numeric' object.")
			

			# where were supposed to be NAs
			bNA <- is.na(index)

			if(any(index[!bNA]%%1!=0) | any(index[!bNA]<1)) stop("The 'index' has to contain positive integer values.")
			
			# the number of valid entries mismatch the number of layers
			if(sum(!bNA)!=nlayers(stack)) stop("You have to provide as many layers as many valid entries in index.")

			# reorder the stack
			noNAInd <- index[!bNA]
			newStack <- stack[[noNAInd]]

			# force index to be monotonous integer sequence
			newIndex <- index
			newIndex[] <- NA
			newIndex[!bNA] <- 1:nlayers(stack)

			# store final object
			.Object@index <- newIndex
			.Object@stack <- newStack
			
		}else{
			if(!is.numeric(dim)) stop("The 'dim' argument has to be a 'numeric' vector.")
			if(nlayers(stack)!=prod(dim, na.rm=TRUE)) warning("The number of layers in the does not equal the product of the 'dim' vector.")
			.Object@stack<- stack
			index <- array(1:nlayers(stack), dim=dim)
			# in case of reuse
			index[duplicated(as.numeric(index))] <- NA
			.Object@index<- index
			
			
		}
		return(.Object)
	}
)








## #' Positions of missing values in a RasterArray object
## #' 
## #' The function behaves similar to the regular \code{is.na()} function applied to the proxy object of a \code{RasterArray}.
## #' 
## #' @param x A \code{RasterArray} class object.
## #' @return A \code{logical} \code{vector}, \code{matrix} or \code{array} matching the structure of the \code{RasterArray}.
## #' 
## #' @examples
## #' data(dems)
## #' dems[2] <- NA
## #' is.na(dems)
## #' 
## #' @export
## is.na.RastArray<-function(x){
## 	is.na(proxy(x))
## }



## #' @rdname apply-methods
## #' @aliases apply,RasterArray-method
## #' @aliases apply,SpatialArray-method
## "apply"

## if("simplify" %in% names(formals(base::apply))){
## 	setMethod("apply", "RasterArray",function(X, MARGIN, FUN,..., simplify=TRUE) 
## 		if(is.null(MARGIN)){
## 			ArrayApplyNULL(X, FUN, ...)
## 		}else{
## 			ArrayApplyReduce(X, MARGIN, FUN, ...)
## 		}
## 	)

## }else{
## 	setMethod("apply", "RasterArray",function(X, MARGIN, FUN,...) 
## 		if(is.null(MARGIN)){
## 			ArrayApplyNULL(X=X, FUN=FUN, ...)
## 		}else{
## 			ArrayApplyReduce(X, MARGIN, FUN, ...)
## 		}
## 	)

## }



