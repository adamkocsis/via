# build from existing stack with existing index, or dimensions
#' @export SfcArray
setMethod("initialize",signature="SfcArray",
	definition=function(.Object,stack, index=NULL, dim=NULL){
		# defend for the presence of sp
		if(!requireNamespace("sf", quietly=TRUE)){
			stop("This function requires the sf package.")
		}	
		# execute everything from the XArray constructor
		ga <- XArray(stack=stack, index=index, dim=dim)

		# get the crs
		firstCRS <- sf::st_crs(ga@stack[[1]])
	
		# and then test the entities for the class
		for(i in 1:nlayers(ga@stack)){
			x <- ga@stack[[i]]
			# check for class
			if(!inherits(x, "sfc")) stop("At least one element is not an sfc object")
			if(inherits(x, "sf")) stop("At least one element is an sf-class object. Use SfArray instead.")
			if(sf::st_crs(x)!=firstCRS) stop("Mismatching CRS.")
		}

		# if everything goes well, all is fine! 
		.Object@stack <- ga@stack
		.Object@index <- ga@index

	
		return(.Object)
	}
)

setMethod(
	"show",
	signature="SfcArray", 
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
			cat("- geodetic CRS: ", format(sf::st_crs(object@stack[[1]])), "\n")

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


