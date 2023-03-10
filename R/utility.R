#' Redefine bounds of a named matrix
#' 
#' The function restructures a \code{\link[base]{matrix}} and extends its current limits to a range defined by a names attribute
#' 
#' This is essentially a subsetting function that allows you to subset even when the rownames or colnames vector
#' extends beyond the bounds of a matrix and traditional subsetting methods result in the notorious 'out of bounds' error.
#' @param x The matrix to be restructured.
#' @param cols Column names guiding the restructuring.
#' @param rows Row names guiding the restructuring.
#' 
#' @return A matrix with extended bounds. 
#' @examples
#' a<-matrix(1:9, ncol=3)
#' rownames(a) <- c("a", "c", "d")
#' newbounds(a, rows=letters[1:5])
#' @export
newbounds <- function(x, cols=NULL, rows=NULL){
  if(!is.matrix(x)) stop("The newbounds() function is only applicable to matrices.")
  
  if(!is.null(rows)){
    if(is.null(rownames(x))) stop("The matrix must have rownames.")
    newX <- matrix(NA, ncol=ncol(x), nrow=length(rows))
    colnames(newX) <- colnames(x)
    rownames(newX) <- rows
    # reorder items to match the new order
    ordering <- rows[rows%in%rownames(x)]
    x2 <- x[ordering, , drop=FALSE]
    
    # insert into new bounds
    newX[rows%in%rownames(x2), ] <- x2[rownames(x2)%in%rows, , drop=FALSE]
  }
  if(!is.null(cols)){
    if(is.null(colnames(x))) stop("The matrix must have colnames.")
    newX <- matrix(NA, nrow=nrow(x), ncol=length(cols))
    rownames(newX) <- rownames(x)
    colnames(newX) <- cols
    # reorder items to match the new order
    ordering <- cols[cols%in%colnames(x)]
    x2 <- x[,ordering , drop=FALSE]
    
    # insert into new bounds
    newX[,cols%in%colnames(x)] <- x[, colnames(x)%in%cols, drop=FALSE]
  }
  return(newX)
}



#' Names as numerics
#' 
#' The set of functions return names of objects directly cast to numeric values.
#' 
#' @param x Object with names, colnames or rownames attributes.
#' @rdname nums
#' @return Numeric vector.
#' @examples
#' 
#' # base R object
#' a <- 1:10
#' names(a) <- seq(10, 100, 10)
#' nums(a)
#' 
#' # XArray
#' data(exemplar)
#' colnums(exemplar)
#' rownums(exemplar)
#' @export
nums <- function(x){
  as.numeric(names(x))
}

#' @rdname nums
#' @export
colnums<- function(x){
  as.numeric(colnames(x))
}

#' @rdname nums
#' @export
rownums <- function(x){
  as.numeric(rownames(x))
}




# one dimensional subscript of n dimensional array on a given margin
marginsubset <- function(x, mar, i){
  # number of dimensions necessary
  dims <- length(dim(x))
  
  # construct subsetting call
  callThis <- paste("x[", paste(rep(",",mar-1), collapse=""),"i", paste(rep(",", dims-mar), collapse=""), "]", collapse="")
  
  # as an expression
  express <- parse(text=callThis)
  
  eval(express)
}




#' @rdname arraylength
#' @exportMethod nlayers
setMethod(
	"nlayers",
	signature="list",
	function(x) length(x)
)


#' @name nlayers
#' @rdname arraylength
#' @aliases nlayers,SpatRaster-method
#' @exportMethod nlayers
setMethod(
	"nlayers",
	signature="SpatRaster",
	function(x){
	dims <- dim(x)
	return(dims[3])
	}
)
