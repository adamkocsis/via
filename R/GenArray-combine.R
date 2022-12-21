################################################################
# Internals for c- methods

# Adding as ingle element
setMethod("c2", signature=c("GenArray", "ANY"), 
	definition=function(x, y){
		callStack <- sys.calls()
		# check the type - should be the same as the rest of GenArray
		targetClass <- class(x@stack[[1]])[1]
		naCase <- FALSE
		# if the type is not the same as the rest
		if(!inherits(y, targetClass)){
			# if it has more than one values -> halt
			if(length(y)>1){
				stop("Incompatible class.")
			# if it is only one value it might still be good
			}else{
				# if that is not missing - > halt
				if(!is.na(y)){
					stop("Incompatible class.")
				}else{
					naCase <- TRUE
				}
			}
		}

		
		# The new index
		if(!naCase){
			ind <- c(x@index, nlayers(x)+1)
			callSymb <- sys.call(which=-3)
			endObj <- GenArray(c(x@stack, list(y)), index=ind)

			if(is.symbol(callSymb[[3]])){
				names(endObj@stack)[nlayers(endObj)] <- deparse(callSymb[[3]])
			}

		}else{
			ind <- c(x@index, NA)

			endObj <- GenArray(x@stack, index=ind)
		}
		return(endObj)

	}
)

# XArray with itself
setMethod("c2", signature=c("GenArray", "GenArray"), 
	definition=function(x, y){
		# shift indices of the second argument
		indexPlus<- y@index+nlayers(x)

		# combine the indices
		ind <- c(x@index, indexPlus)

		# the final object
		endObj <- GenArray(c(x@stack, y@stack), index=ind)

		return(endObj)
	}
)
