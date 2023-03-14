
#' Dimensions of \code{\link[via:XArray-class]{VirtualArray}}-derived class objects
#' 
#' The function returns the dimensions of the array in which elements are organized.
#' @param x A \code{\link[via:XArray-class]{VirtualArray}}-derived class object.
#' @return A \code{numeric} vector.
#' 
#' @examples
#' data(exemplar)
#' dim(exemplar)
#' @exportMethod dim
setMethod(
	"dim", 
	signature="VirtualArray", 
	function(x){
		proxyDim <- dim(x@index)
		if(is.null(proxyDim)) proxyDim <- length(x@index)
		proxyDim
	} 
)


#' Names of one-dimensional \code{\link[via:XArray-class]{VirtualArray}}-derived class objects.
#' 
#' Get or set the names of one-dimensional \code{\link[via:XArray-class]{VirtualArray}}-derived class objects 
#' @param x \code{\link[via:XArray-class]{VirtualArray}}-derived class object.
#' @param value \code{character} vector.
#' @return A \code{character} vector of names or \code{NULL}.
#' 
#' @examples
#' ex <- rastex()
#' names(ex)
#' names(ex)[4] <- "weirdo"
#' # NULL
#' @rdname names
#' @exportMethod names
setMethod(
	"names",
	signature="VirtualArray",
	function(x){	
		names(x@index)
	}
)

#' @rdname names
#' @exportMethod "names<-"
setReplaceMethod(
	"names",
	signature="VirtualArray",
	definition=function(x,  value){
		# not defined for matrices or higher
		if(is.null(names(x))) names(x@index) <- rep(NA, length(x@index))
		names(x@index) <- value 
		return(x)
})


#' Number of elements or layers in a \code{\link[via:XArray-class]{VirtualArray}}-derived class object
#' 
#' Function to return the length of the array in which elements are organized.
#' 
#' The \code{length()} function returns the number elements that should be present based on the array structure itself, and not the total number of values stored in the object. As the object can contain missing values, the number of actual layers can be queried with \code{\link{nlayers}}. 
#' 
#' @param x a \code{\link[via:XArray-class]{VirtualArray}}-derived class object.
#' @return A \code{numeric} value. 
#' @examples
#' ex <- rastex()
#' # omit third element
#' ex[3] <- NA
#' # number of elements in the RasterArray
#' length(ex)
#' # remaining number values in the stack 
#' length(ex@stack)
#' # the number of remaining layers in the RasterArray
#' nlayers(ex)
#' 
#' @rdname arraylength
#' @exportMethod length
setMethod(
	"length",
	signature="VirtualArray",
	function(x) length(x@index)
)


#' @rdname layers
setMethod(
	"layers", 
	signature="VirtualArray", 
	function(x){
		# returns the layer names
		names(x@stack)

	} 
)


#####################

#' Column names of two-dimensional \code{\link[via:XArray-class]{VirtualArray}}-derived class object.
#' 
#' Get or set the column names of two-dimensional code{\link[via:XArray-class]{VirtualArray}}-derived class objects 
#' @param x \code{\link[via:XArray-class]{VirtualArray}}-derived class object.
#' @param value \code{character} vector.
#' @return A \code{character} vector of column names or \code{NULL}.
#' 
#' @examples
#' data(paleocoastlines)
#' colnames(paleocoastlines)
#' colnames(paleocoastlines) <- c("a", "b")
#' @rdname colnames
#' @exportMethod colnames
setMethod(
	"colnames",
	signature="VirtualArray",
	function(x) colnames(x@index)
)

#' @rdname colnames
#' @exportMethod "colnames<-"
setReplaceMethod(
	"colnames",
	signature="VirtualArray",
	definition=function(x,  value){
		# not defined for matrices or higher
		if(length(dim(x))!=2) stop("The proxy is not a 2D matrix.")
		colnames(x@index) <- value 
		return(x)
})



#' Row names of two-dimensional \code{\link[via:XArray-class]{VirtualArray}}-derived class objects.
#' 
#' Get or set the row names of two-dimensional \code{\link[via:XArray-class]{VirtualArray}}-derived class object  
#' @param x \code{\link[via:XArray-class]{VirtualArray}}-class object.
#' @param value \code{character} vector.
#' @return A \code{character} vector of row names or \code{NULL}.
#' 
#' @examples
#' data(paleocoastlines)
#' rownames(paleocoastlines)
#' rownames(paleocoastlines) <- paste(rownames(paleocoastlines), "Ma")
#' @rdname rownames
#' @exportMethod rownames
setMethod(
	"rownames",
	signature="VirtualArray",
	function(x) rownames(x@index)
)

#' @rdname rownames
#' @exportMethod "rownames<-"
setReplaceMethod(
	"rownames",
	signature="VirtualArray",
	definition=function(x,  value){
		# not defined for matrices or higher
		if(length(dim(x))!=2) stop("The proxy is not a 2D matrix.")
		rownames(x@index) <- value 
		return(x)
})


#' Names of a multidimensional \code{\link[via:XArray-class]{VirtualArray}}-derived class object.
#' 
#' Get or set the dimnames of multidimensional \code{\link[via:XArray-class]{VirtualArray}}-derived class object.
#' @param x \code{\link{RasterArray}} or \code{\link{SfArray}} object.
#' @param value \code{character} vector.
#' @return A \code{list} of \code{character} vectors or \code{NULL}.
#' 
#' @examples
#' ex <- rastex()
#' dimnames(ex)
#' data(paleocoastlines)
#' dimnames(paleocoastlines)
#' dimnames(paleocoastlines)[[2]] <- c("first", "second")
#' names(dimnames(paleocoastlines)) <- c("age", "type")
#' @rdname dimnames
#' @exportMethod dimnames
setMethod(
	"dimnames",
	signature="VirtualArray",
	function(x) dimnames(x@index)
)

#' @rdname dimnames
#' @exportMethod "dimnames<-"
setReplaceMethod(
	"dimnames",
	signature="VirtualArray",
	definition=function(x,  value){
		# not defined for matrices or higher
		if(is.null(dim(x))) stop("One-dimensional VirtualArrays have no dimnames.")
		dimnames(x@index) <- value 
		return(x)
})



#' Number of columns and rows of a \code{\link[via:XArray-class]{VirtualArray}}-derived class object.
#' 
#' Unlike the \code{ncol} and \code{nrow} functions of the terra package, this function returns the number of columns and rows of the \code{VirtualArray}-derived container, rather than the dimensions of the contained \code{SpatRaster}s. 
#' 
#' @param x A \code{\link[via:XArray-class]{VirtualArray}}-derived class object.
#' @rdname adimatt
#' @return A \code{numeric} value of the number of columns and rows.
#' @exportMethod ncol
#' @examples
#' data(paleocoastlines)
#' ncol(paleocoastlines)
#' nrow(paleocoastlines)
setMethod(
	"ncol", 
	signature="VirtualArray", 
	function(x){
		ncol(x@index)
	} 
)

#' @rdname adimatt
#' @exportMethod nrow
setMethod(
	"nrow", 
	signature="VirtualArray", 
	function(x){
		nrow(x@index)
	} 
)

