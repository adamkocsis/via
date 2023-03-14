# build from existing stack with existing index, or dimensions
#' @export XArray
setMethod("initialize",signature="XArray",
	definition=function(.Object,stack, index=NULL, dim=NULL){
		# some defense for index
		if(is.null(index)){
			index <- 1:length(stack)
		}

		# check whether the stack has the same types
		if(!inherits(stack,"list")) stop("The 'stack' has to be a 'list' - class object.")
		if(length(unique(lapply(stack, class)))!=1) stop("The 'stack' can only contain a single class of items.")
	
		if(is.null(dim)){ 
			if(!is.numeric(index)) stop("The 'index' has to be a 'numeric' object.")
			

			# where were supposed to be NAs
			bNA <- is.na(index)

			if(any(index[!bNA]%%1!=0) | any(index[!bNA]<1)) stop("The 'index' has to contain positive integer values.")
			
			# the number of valid entries mismatch the number of layers
			if(sum(!bNA)!=length(stack)) stop("You have to provide as many layers as many valid entries in index.")

			# reorder the stack
			noNAInd <- index[!bNA]
			newStack <- stack[noNAInd]

			# force index to be monotonous integer sequence
			newIndex <- index
			newIndex[] <- NA
			newIndex[!bNA] <- 1:length(stack)

			# store final object
			.Object@index <- newIndex
			.Object@stack <- newStack
			
		}else{
			if(!is.numeric(dim)) stop("The 'dim' argument has to be a 'numeric' vector.")
			if(length(stack)!=prod(dim, na.rm=TRUE)) warning("The number of layers in the does not equal the product of the 'dim' vector.")
			.Object@stack<- stack
			index <- array(1:length(stack), dim=dim)
			# in case of reuse
			index[duplicated(as.numeric(index))] <- NA
			.Object@index<- index
			
			
		}
		return(.Object)
	}
)

setMethod(
	"show",
	signature="XArray", 
	function (object) 
	{
	    cat("class         :", class(object), "\n")
	    ## if (rotated(object)) {
	    ##     cat("rotated     : TRUE\n")
	    ## }
	    mnr <- 15
	#   if (filename(object) != "") {
	#       cat("filename    :", filename(object), "\n")
	#   }

	    nl <- nlayers(object)
	    if (nl > 0) {
	   		cat("Element properties: \n")
			cat("- class       : ", class(object@stack[[1]]), "\n")

  			cat("Array properties: \n")
  			adim <- dim(object)
  			allName <- names(object)
		   
	        if(length(adim)==1){
		        cat("- dimensions  : ", paste(adim, collapse=", "), 
		            "  (vector)\n", 
		            sep = "")
		      
		    }else{
		    	allName<- dimnames(object)
		    	if(length(allName)==2){
			    	cat("- dimensions  : ", paste(adim, collapse=", "), 
			            "  (nrow, ncol)\n", 
			            sep = "")
			    }else{
			    	cat("- dimensions  : ", paste(adim, collapse=", "), 
			            "  (nrow, ncol, ...)\n", 
			            sep = "")
			    }
		#    	for(i in 1:length(allName)){
		#			if(i==1) cat("- rownames    : ", paste(allName[[i]], collapse=", "), "\n", sep = "")
		#			if(i==2) cat("- colnames    : ", paste(allName[[i]], collapse=", "), "\n", sep = "")
		#			if(i>2) cat(paste("- Dim", i, " names", sep=""), "  : ", paste(allName[[i]], collapse=", "), "\n", sep = "")
		#    	}
				

		    	  
		    }
		    cat("- num. layers : ", nlayers(object), "\n", 
		        sep = "")
			cat("- missing     : ", sum(is.na(object@index)), "\n", 
				sep = "")
		    cat("- proxy:\n ")
		    print(proxy(object))
		   
	    } else {
	        cat("nlayers       :", nl, "\n")
			if(sum(is.na(object@index))>0){
				cat("- missing     : ", sum(is.na(object@index)), "\n", 
					sep = "")
				cat("- proxy:\n ")
				print(proxy(object))
			}
	    } 
	    cat("\n")
	}
)


