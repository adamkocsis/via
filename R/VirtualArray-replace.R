#' Replace layers of an object of a class derived from a \code{\link[via:XArray-class]{VirtualArray}}.  
#' 
#' Single bracket \code{'['} refers to indices and names within the \\code{\link[via:XArray-class]{VirtualArray}}. Use double brackets to replace layers based on their names (in the stack).
#' Object types of the same kind \code{SpatRasters} can be used to replace values in \code{\link{RasterArray}}s. \code{sf} objects can be used with \code{\link{SfArray}}s.
#'
#' @param x \code{\link[via:XArray-class]{VirtualArray}}-class object.
#' @param i subscript of the first dimension(rows) or vector-like subsetting.
#' @param j subscript of the second dimension (columns).
#' @param ... subscript of additional dimensions.
#' @param value A same class object as \code{x}.
#' @aliases [<-,VirtualArray-method
#' @return None.
#' @examples
#' ex <- rastex() 
#' # replace third element with missing value
#' ex[3] <- NA
#' # duplicate first element and make it the second too
#' ex[2] <- ex[1]
#' ex
#' 
#' @rdname replacementSingle
#' @exportMethod "[<-"
setReplaceMethod(
	"[", 
	signature(x="VirtualArray", value="logical"),
	definition=function(x,i,j,..., value){
		# fetch the index
		indDim <- dim(x@index)

		# this should only apply to NA
		if(any(!is.na(value))) stop("Replacement with TRUE/FALSE is not valid.")

		sysCall <- sys.call(which=-1)

		if(sum(is.na(value))!=length(value)) stop("Invalid replacement type.")
		if(is.null(indDim) | length(indDim)==1){
			if(length(i)!=length(value) & length(value)!=1) stop("Invalid replacement length.")
			theIndex <- x@index[i]
			x@index[i] <- NA

		}
		
		# multi- dim case
		if(length(indDim)>=2){
			# one dimensinoal 
			if(length(sysCall)==4){
				theIndex <- x@index[i]
				x@index[i] <- NA
			}else{
				theIndex <- x@index[i,j,...]
				x@index[i,j,...] <- NA
			}
		}

		# ensure flat index

		# rebuild the stack
		origInd<- 1:nlayers(x@stack)
		keepOrig <- origInd[!origInd%in%theIndex]
		
		# omit unwanted layers
		if(inherits(x, "XArray")){
			x@stack <- x@stack[keepOrig]
		}
		if(inherits(x, "RasterArray")){
			x@stack <- x@stack[[keepOrig]]
		}
		
		# constrain order again
		x@index<- defragment(x@index)

		return(x)
		
	}
)



# Generalized layer replacement function for VirtualArray. Method dispatch written explicitly as RasterArray[ <- RasterLayer and SpatialArray [<- Spatial*
VirtualArrayReplaceLayer <- function(x,i,j,value,...){
	# fetch the index
	indDim <- dim(x@index)
	# one dim case
	if(is.null(indDim) | length(indDim)==1){
		# pointer to the stack to be replaced
		theIndex <- x@index[i]
		# separte the index based on NA
		bIndNA <- is.na(theIndex)
		
		# if at least one layer stack is not there
		if(any(bIndNA)){
			# usef for the addition
			newI <- i[bIndNA]
			# prepare the new index vector
			tempIndex <- x@index
			tempIndex[newI]<- -1
			tempIndex <- defragment(tempIndex)
			# where are the old layers in the new stack
			oldInNew <- tempIndex[!is.na(x@index)]
			# the index of the new layers
			totallyNew <- tempIndex[newI]
			# add to the rest
			newInd <- c(oldInNew, totallyNew)
			# use this to reorder
			tempInd2 <- rep(NA, length(newInd))
			tempInd2[newInd]<- 1:length(newInd)
			
			# add the new layer to the stack
			if(inherits(value, "SpatRaster")){
				newStack <- c(x@stack, value[[rep(1, length(totallyNew))]])
				# the reorderd stack
				x@stack <- newStack[[tempInd2]]
			# XArray + derived
			}else{
				newStack <- c(x@stack, list(value)[rep(1, length(totallyNew))])
				x@stack <- newStack[tempInd2]
			}
			
			x@index <- tempIndex
		}
		if(any(!bIndNA)){
			# restart the process, now with the new index vector
			theIndex <- x@index[i]
			replaceIndex <- theIndex[!bIndNA]
		
			# simply replace the layers in the stack...
			allVals <- 1:nlayers(x@stack)
			origVals <- allVals[!allVals%in%replaceIndex]
		
			# create a reorderer vector
			newInd <-c(origVals, replaceIndex)
			tempInd2 <- rep(NA, length(newInd))
			tempInd2[newInd] <-  1:length(tempInd2)
			
			if(inherits(value, "SpatRaster")){
				# put the additional elements to the stack
				newStack <- c(x@stack[[origVals]], value[[rep(1, length(replaceIndex))]])

				# reorder to correct
				x@stack <- newStack[[tempInd2]]
			# XArray + derived
			}else{
				# put the additional elements to the stack
				newStack <- c(x@stack[origVals], list(value)[rep(1, length(replaceIndex))])

				# reorder to correct
				x@stack <- newStack[tempInd2]
			}
		}
	}
	# multi- dim case
	if(length(indDim)>=2){
		theIndex <- x@index[i,j,...]
		
		# separte the index based on NA
		bIndNA <- is.na(theIndex)
		
		if(any(bIndNA)){
			fullInd <- 1:length(x@index)
			dim(fullInd) <- dim(x@index)
			newIJ <- fullInd[i,j,...]
			newIJ<- newIJ[bIndNA]
			# prepare the index vector
			tempIndex <- x@index
			tempIndex[newIJ]<- -1
			tempIndex <- defragment(tempIndex)
			# where are the old layers in the new stack
			oldInNew <- tempIndex[!is.na(x@index)]
			# add the index at the end
			totallyNew <- tempIndex[newIJ]
			newInd <- c(oldInNew, totallyNew)
			# use this to reorder
			tempInd2 <- rep(NA, length(newInd))
			tempInd2[newInd]<- 1:length(newInd)
			
			if(inherits(value, "SpatRaster")){
				# add the new layer to the stack
				newStack <- c(x@stack, value[[rep(1, length(totallyNew))]])
				x@stack <- newStack[[tempInd2]]
			# XArray + derived
			}else{
				newStack <- c(x@stack, list(value)[rep(1, length(totallyNew))])
				x@stack <- newStack[tempInd2]
			}

			# the reorderd stack
			x@index <- tempIndex
		}
		if(any(!bIndNA)){
			# restart the process, now with the new index vector
			theIndex <- x@index[i,j,...]
			replaceIndex <- theIndex[!bIndNA]
		
			# simply replace the layers in the stack...
			allVals <- 1:nlayers(x@stack)
			origVals <- allVals[!allVals%in%replaceIndex]
		
			# create a reorderer vector
			newInd <-c(origVals, replaceIndex)
			tempInd2 <- rep(NA, length(newInd))
			tempInd2[newInd] <-  1:length(tempInd2)
			
			# stacking is dependent on the kind of Array
			if(inherits(value, "SpatRaster")){
				# put the additional elements to the stack
				newStack <- c(x@stack[[origVals]], value[[rep(1, length(replaceIndex))]])
				x@stack <- newStack[[tempInd2]]
			# XArray + derived
			}else{
				newStack <- c(x@stack[origVals], list(value)[rep(1, length(replaceIndex))])
				x@stack <- newStack[tempInd2]
			}

			
			# reorder to correct
		
		}
	}

	return(x)

}


#' Replace elements of a \code{VirtualArray}-class objects.
#'
#' Double bracket \code{'[['} refers to layers' name in the names of the \code{@stack} member of the \code{VirtualArray}. Use single brackets to replace elements based on their position in the \code{VirtualArray}.
#' 
#' @param x Object from a class derived from \code{\link[via:XArray-class]{VirtualArray}}.
#' @param i subscript of layers to replace.
#' @param value \code{character} vector.
#' @return None.
#' 
#' @aliases [[<-,VirtualArray-method
#' @aliases [[<-,VirtualArray,ANY,ANY-method
#' @rdname doubleBracketReplace
#' @exportMethod "[[<-"
setReplaceMethod(
	"[[", 
	signature(x="VirtualArray"),
	function(x,i, value){
		x@stack[[i]] <- value
		return(x)
	}
)
