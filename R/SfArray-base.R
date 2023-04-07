#' @export SfArray
setMethod("initialize",signature="SfArray",
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
			if(!inherits(x, "sf")) stop("At least one element is not an sf object")
			if(sf::st_crs(x)!=firstCRS) stop("Mismatching CRS.")
		}

		# if everything goes well, all is fine! 
		.Object@stack <- ga@stack
		.Object@index <- ga@index

	
		return(.Object)
	}
)
