#' Subset a '\code{\link[via:XArray-class]{VirtualArray}}'-class object
#' 
#' Extract subsets of an object from a class derived from '\code{\link[via:XArray-class]{VirtualArray}}' similarly to a regular array. 
#' 
#' @param x \code{\link[via:XArray-class]{VirtualArray}}-class object.
#' @param i subscript of the first dimension(rows) or vector-like subsetting.
#' @param j subscript of the second dimension (columns).
#' @param ... subscript of additional dimensions.
#' @param drop \code{logical} in case the result of subsetting is a single element, should the \\code{\link[via:XArray-class]{VirtualArray}} wrapper be dropped?
#' @param oneDim \code{logical} In case of multidimensional \code{\link[via:XArray-class]{VirtualArray}}s, setting \code{oneDim} to \code{TRUE} allows the application of one dimensional subscripts.  
#' @return Either the same class as \code{x}, or the class that forms the element of the \code{\link[via:XArray-class]{VirtualArray}}.
# combined
#' @rdname subset
#' @exportMethod subset
#' @examples
#' ex <- rastex()
#' # first 4
#' subset(ex, i=1:4)
#' # missing at the end
#' subset(ex, i=1:12)
#' # character subscript
#' subset(ex, i=c("a", "b"))
#' # logical subscript
#' subs <- rep(TRUE, length(ex))
#' subs[1] <- FALSE # remove first
#' subset(ex, i= subs)
#' # no drop
#' subset(ex, i=1, drop=FALSE)
setMethod(
	"subset", 
	signature(x="VirtualArray"), 
	function(x, i,j, ...,oneDim=FALSE, drop=TRUE){
			# fetch the index
			indDim <- dim(x@index)

			# one dim case
			if(is.null(indDim) | length(indDim)==1){
				originIndex <-x@index[i]
			}

			# two dim case
			if(length(indDim)>=2){
				# multidimensional subscript
				if(!oneDim){
					originIndex <-x@index[i,j,...]
				# one dimensional subscript
				}else{
					originIndex<-x@index[i, drop=TRUE]
				}
			}
		
			# constrain one dimension
			fetchIndex <- originIndex
			dim(fetchIndex) <- NULL

			# drop to a single entity
			if(length(fetchIndex)==1 & drop==TRUE){
				# if it is NA
				if(is.na(fetchIndex)){
					x<-NA
				}else{
					x<- x@stack[[fetchIndex]]
				}

			# keep using RasterArray
			}else{
				# separate the NAs
				bNA <- is.na(fetchIndex)
				if(any(bNA)){
					validFetch <- fetchIndex[!bNA]
				}else{
					validFetch <- fetchIndex
				}

				# wrappers will not be dropped
				if(inherits(x@stack, "list")){
					x@stack <- x@stack[validFetch]
				}else{
					# get the relevant layers
				 	x@stack <- x@stack[[validFetch]]
				}	

				# rewrite the index 
				x@index<- originIndex

				# if the proxy was numeric, it should be reset
				x@index[!bNA] <- 1:nlayers(x@stack)

				return(x)
			}		
	}
)


#' Indexing to extract subsets of a 'code{\link[via:XArray-class]{VirtualArray}}'-class object
#'
#' Single bracket \code{'['} refers to indices and names within the '\code{\link[via:XArray-class]{VirtualArray}}'-class object. Use double brackets to extract layers based on their names (in the \code{@stack}).
#' 
#' @param x An object from a \code{\link[via:XArray-class]{VirtualArray}}-derived class. 
#' @param i subscript of the first dimension(rows) or vector-like subsetting.
#' @param j subscript of the second dimension (columns).
#' @param ... subscript of additional dimensions.
#' @param drop \code{logical} in case the result of subsetting is a single element, should the \code{\link[via:XArray-class]{VirtualArray}}-derived wrapper be dropped? 
#' @return An object from either the same class as \code{x} or the class of its elements.
#' @rdname VirtualArray-single-bracket-method
#' @aliases [,VirtualArray-method
#' @examples
#' ex <- rastex()
#' # numeric subsetting
#' firstThree <- ex[1:3]
#' # character subsetting
#' second <- ex["d"]
#' # logical subsetting
#' subscript <- rep(FALSE, length(ex))
#' subscript[2] <- TRUE
#' second2 <- ex[subscript]
#' data(paleocoastlines)
#' present<- paleocoastlines["0", ]
#' allMargin <- paleocoastlines[, "margin"]
#' 
#' @exportMethod [
setMethod(
	"[",
	signature(x="VirtualArray", i="ANY", j="ANY"),
	definition=function(x,i,j,..., drop=TRUE){
		# save system call
		sysCall <- sys.call(which=-1)

		# look for drop and omit from the call
		call <- as.character(sysCall)
		args <- names(sysCall)
		dropNum <- which(args=="drop")
		if(length(dropNum)>0){
			call <- call[-dropNum]
		}
		
		# check whether one or multidimensional subscripts are necessary
		oneDim<-FALSE
		if(length(call)==3){
			oneDim <- TRUE
		}
		subset(x,i,j,..., oneDim=oneDim, drop=drop)
		
	}
)


#' Indexing to extract the elements of a '\code{\link[via:XArray-class]{VirtualArray}}'-derived class object.
#'
#' Double bracket \code{'[['} refers to elements'/layers' name in the \code{@stack} of the '\code{\link[via:XArray-class]{VirtualArray}}'-derived object. Use single brackets to extract elements based on their position in the '\code{\link[via:XArray-class]{VirtualArray}}'.
#' 
#' @param x \code{\link[via:XArray-class]{VirtualArray}}
#' @param i subscript of the first dimension(rows) or vector-like subsetting.
#' @param drop \code{logical} should the \code{\link[via:XArray-class]{VirtualArray}} be dropped and the element be reduced to the element class?
#' @return A \code{\link[via:XArray-class]{VirtualArray}}-derived class object, or an object of the class that makes up the VirtualArray
#' @rdname VirtualArray-double-bracket-method
#' @aliases [[,VirtualArray-method
#' @exportMethod "[["
#' @examples
#' data(exemplar)
#' # finds a layer
#' exemplar[["sample1"]]
#' # returns a stack
#' exemplar[[c("sample1", "sample2")]]
#' # replaces a layervalues, but not the attributes of the layer
#' exemplar2 <- exemplar
#' exemplar2[["sample1"]] <- exemplar2[["sample2"]]
#' # compare every value in the they are all the same
#' exemplar2[["sample1"]]$x == exemplar2[["sample2"]]$x
setMethod(
	"[[", 
	signature(x="VirtualArray"),
	function(x,i,drop=TRUE){
		# where are NAs in the subscrtip
		bNA <- is.na(i)
		if(sum(bNA)==length(i)) return(i)

		# logical method
		if(is.logical(i)){
			if(length(i)!=length(x)) stop("Invalid subscript length.")
			
			# stack subscript
			usedInd <- i
			usedInd[bNA] <- FALSE
			
			# drop not understood for SpatRaster
			if(!inherits(x, "RasterArray")){
				#select appropriate layers
				newStack<- x@stack[which(usedInd)]
			}else{
				newStack<- x@stack[[which(usedInd)]]
			}

			# index subscript
			newIndex <- x@index[i]
			newIndex[!is.na(newIndex)] <- 1:sum(!is.na(newIndex))
		}

		# either character or numeric
		if(is.character(i) | is.numeric(i)){

			# drop not understood for SpatRaster
			if(!inherits(x, "RasterArray")){
				# XArray - list subsetting
				newStack<- x@stack[i[!bNA]]
			}else{
				newStack<- x@stack[[i[!bNA]]]
			}

			# reindex
			newIndex <- rep(NA, length(i))
			newIndex[!bNA] <- 1:nlayers(newStack)
		}

		# depending on type of object
		if(inherits(newStack, "list")){
			final <- XArray(index=newIndex, stack=newStack)
		}

		if(inherits(newStack, "SpatRaster")){
			final <- RasterArray(index=newIndex, stack=newStack)
		}

		if(drop){
			if(length(final)==1){
				final <- final@stack[[1]]
			}
		}

		return(final)
	}
)



