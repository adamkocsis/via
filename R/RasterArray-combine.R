# XArray with itself
setMethod("c2", signature=c("RasterArray", "RasterArray"), 
	definition=function(x, y){
		# shift indices of the second argument
		indexPlus<- y@index+nlayers(x)

		# combine the indices
		ind <- c(x@index, indexPlus)

		# the final object
		endObj <- RasterArray(c(x@stack, y@stack), index=ind)

		return(endObj)
	}
)

# adding multiple RasterLayers
setMethod("c2", signature=c("RasterArray", "SpatRaster"), 
	definition=function(x, y){
		# The new index
		ind <- c(x@index, (nlayers(x)+1):(nlayers(x)+nlayers(y)))
		if(nlayers(y)==1){
			callSymb <- sys.call(which=-3)
			if(is.symbol(callSymb[[3]])){
				names(ind)[length(ind)] <- deparse(callSymb[[3]])
			}
		}
		endObj <- RasterArray(c(x@stack, y), index=ind)
		return(endObj)

	}
)

setMethod("c2", signature=c("RasterArray", "logical"), 
	definition=function(x, y){
		if(!any(!is.na(y))) "Invalid argument."
		ind<- c(x@index, rep(NA,length(y)))

		# copy the name if it there is one
		if(!is.null(names(y))) names(ind)[(length(ind)-length(y)+1):length(ind)] <- names(y)

		# replace index with new
		x@index <- ind

		# return corrected object
		return(x)
		
	}
)

