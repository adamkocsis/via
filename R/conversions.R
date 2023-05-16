################################################################################
# To XArray

#' Coerce into an \code{\link[via:SfcArray-class]{SfcArray}} or \code{\link[via:SfArray-class]{SfArray}} object
#' 
#' @param from Either a \code{\link[via:SfcArray-class]{SfcArray}}, \code{\link[via:SfArray-class]{SfArray}} or \code{\link[via:XArray-class]{XArray}}-class object
#' @rdname coercion
#' @return Either a \code{\link[via:SfcArray-class]{SfcArray}}, \code{\link[via:SfcArray-class]{SfArray}} or \code{\link[via:XArray-class]{XArray}}-class object
#' @export
setGeneric("as.XArray", function(from) standardGeneric("as.XArray"))

# demotions
#' @rdname coercion
setMethod(as.XArray, signature=c("SfcArray"), definition=function(from){

	# the processed version
	XArray(index=from@index,stack=from@stack)
})

## #' @rdname coercion
## #' @name as
## setAs(from="SfcArray", to="XArray", function(from){
## 	as.XArray(from)
## })

#' @rdname coercion
setMethod(as.XArray, signature=c("SfArray"), definition=function(from){

	# the processed version
	XArray(index=from@index,stack=from@stack)
})

## #' @rdname coercion
## #' @name as
## setAs(from="SfArray", to="XArray", function(from){
## 	as.XArray(from)
## })

################################################################################
# To SfcArray

#' @rdname coercion
#' @export
setGeneric("as.SfcArray", function(from) standardGeneric("as.SfcArray"))

#' @rdname coercion
setMethod(as.SfcArray, signature=c("XArray"), definition=function(from){
	# defend for the presence of sf
	if(!requireNamespace("sf", quietly=TRUE)){
		stop("This function requires the sf package.")
	}	

	# try the promotion
	SfcArray(index=from@index,stack=from@stack)
})


setAs(from="XArray", to="SfcArray", function(from){
	as.SfcArray(from)
})

#' @rdname coercion
setMethod(as.SfcArray, signature=c("SfArray"), definition=function(from){
	# defend for the presence of sf
	if(!requireNamespace("sf", quietly=TRUE)){
		stop("This function requires the sf package.")
	}	

	# take the stack
	theStack <- from@stack 

	# go through every element
	for(i in 1:length(theStack)){
		# the current element
		x <- theStack[[i]]
		theStack[[i]]<- x$geometry 
	}

	# the processed version
	SfcArray(index=from@index,stack=theStack)
})


## setAs(from="SfArray", to="SfcArray", function(from){
## 	as.SfcArray(from)
## })

################################################################################
# To SfArray

#' Coerce into an SfArray or SfcArray object
#' 
#' @rdname coercion
#' @export
setGeneric("as.SfArray", function(from) standardGeneric("as.SfArray"))

#' @rdname coercion
setMethod(as.SfArray, signature=c("XArray"), definition=function(from){
	# defend for the presence of sf
	if(!requireNamespace("sf", quietly=TRUE)){
		stop("This function requires the sf package.")
	}	

	# try the promotion
	SfArray(index=from@index,stack=from@stack)
})


setAs(from="XArray", to="SfArray", function(from){
	as.SfArray(from)
})

#' @rdname coercion
setMethod(as.SfArray, signature=c("SfcArray"), definition=function(from){

	# defend for the presence of sf
	if(!requireNamespace("sf", quietly=TRUE)){
		stop("This function requires the sf package.")
	}	

	# take the stack
	theStack <- from@stack 

	# go through every element
	for(i in 1:length(theStack)){
		# the current element
		x <- theStack[[i]]
		theStack[[i]]<- sf::st_sf(data.frame(a=1:length(x)), geom=x)
	}

	# the processed version
	SfArray(index=from@index,stack=theStack)
})

#' @rdname coercion
#' @name as
setAs(from="SfcArray", to="SfArray", function(from){
	as.SfArray(from)
})
