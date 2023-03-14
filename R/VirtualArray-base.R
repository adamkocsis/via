#' The proxy of a VirtualArray-derived object
#' 
#' This function returns an object that symbolizes the structure of layers in the \code{XArray}, \code{RasterArray} or \code{SfArray}.
#'
#' The \code{proxy} method wraps the names of layers in the stack using the \code{index} slot of the \code{VirtualArray}.
#'  
#' @param x \code{XArray}, \code{RasterArray} or \code{SfArray} object.
#' @return A \code{vector}, \code{matrix} or \code{array} of characters representing the \code{VirtualArray} structure.
#' @param ... additional arguments passed to class-specific methods.
#' @examples
#' data(exemplar)
#' proxy(exemplar)
#'
#' data(paleocoastlines)
#' proxy(paleocoastlines)
#' @exportMethod proxy
#' @rdname proxy
setGeneric("proxy", function(x,...) standardGeneric("proxy"))

#' @rdname proxy
setMethod(
	"proxy",
	signature="VirtualArray",
	function(x){
		ind <- x@index
		
		# only NAs are present
		if(any(!is.na(ind))){
			if(!is.null(names(x@stack))) ind[]<- names(x@stack)[ind]
		}
		
		return(ind)
	}

)




#' Transpose a \code{\link[via:XArray-class]{VirtualArray}} object
#' 
#' @examples
#' data(exemplar)
#' t(exemplar)
#' data(paleocoastlines)
#' t(paleocoastlines)
#' @param x A \code{\link[via:XArray-class]{VirtualArray}}-class object. 
#' @return A \code{\link[via:XArray-class]{VirtualArray}}-class object.
#' @exportMethod t
#' @rdname t-methods
setMethod(
	"t", 
	"VirtualArray", 
	function(x){
		if(length(dim(x))>2) stop("The array has too many dimensions. ")

		# transpose index
		tIndex<- t(x@index)
		vIndex <- as.numeric(tIndex)

		# ordering
		vIndna <- vIndex[!is.na(vIndex)]

		# reorder the stack
		if(inherits(x@stack, "SpatRaster")){
			x@stack <- x@stack[[vIndna]]
		}else{
			x@stack <- x@stack[vIndna]
		}

		# refill the index
		tIndex[!is.na(tIndex)] <- 1:nlayers(x)

		# copy names
		if(!is.null(colnames(x@index))) rownames(tIndex) <- colnames(x@index)
		if(!is.null(rownames(x@index))) colnames(tIndex) <- rownames(x@index)
		if(!is.null(names(x@index)))  colnames(tIndex) <- names(x@index)

		# replace the index
		x@index <- tIndex



		return(x)

	}
)



# function to defragment the matrix
defragment <- function(x){
	b <- is.na(x)
	x[!b] <- 1:sum(!b)
	return(x)
}


# this utility function will combine the layer specific information
# 2d matrix (vals2d) layer names are represented as colnames
extendDim <- function(proxy, vals2d, newdim=1){
	# the original dimensions of the proxy
	origDim <- dim(proxy)
	if(is.null(origDim)) origDim <- length(proxy) 

	# the names of the proxy
	origNames <- dimnames(proxy)
	if(is.null(origNames)) origNames <- list(names(proxy))

	# number extended
	nVals <-dim(vals2d)[newdim]
	
	# copy the names properly 
	addNames <- dimnames(vals2d)[[newdim]]

	# where are the non-na values
	naMap <- !is.na(proxy)

	# vector shape of the data
	endObj <- rep(NA, prod(c(origDim,nVals)))
	
	# loop through the new dimension
	for(i in 1:nVals){
		# what contains the new dimension?
		if(newdim==1){
			theseVals<- vals2d[i,]
		}else{
			theseVals<- vals2d[,i]
		}
		# the final object. 
		endObj[(1:length(proxy))+(i-1)*length(proxy)] <- theseVals[proxy]
	}
	# dimensions and names set right
	dim(endObj) <- c(origDim,nVals)
	dimnames(endObj) <- c(origNames, list(addNames))
	
	return(endObj)
}

