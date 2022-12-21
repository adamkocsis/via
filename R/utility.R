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
#' The set of functions return names of objects directly transformed to numeric values.
#' 
#' @param x Object with names, colnames or rownames attributes.
#' @rdname nums
#' @return Numeric vector.
#' @examples
#' data(dems)
#' # ages as numerics
#' nums(dems)
#' # younger than 20Ma
#' dems[nums(dems)<20]
#' 
#' 
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


#Accessing file within the package
pkg_file <- function(...) {
  system.file(..., package = "genarray")
}





# function used by mapedge

detailedBounds <- function(x,y, xmin=-180, xmax=180, ymin=-90, ymax=90){
  rbind(
    cbind(seq(xmin, xmax, length.out=x), rep(ymax, x)),
    cbind(rep(xmax, y), seq(ymax, ymin, length.out=y)),
    cbind(seq(xmax, xmin, length.out=x), rep(ymin, x)),
    cbind(rep(xmin, y), seq(ymin, ymax, length.out=y))
  )
}


#' Function to quickly draft the edge of the equirectangular projection 
#' 
#' Function to plot the edge of a map with different projections.
#' 
#' @param x (\code{numeric}) Number of segments in the x (longitude) dimension. 
#' @param y (\code{numeric}) Number of segments in the y (latitude) dimension. 
#' @param xmin (\code{numeric}) Minimum value of x (longitude).
#' @param xmax (\code{numeric}) Maximum value of x (longitude).
#' @param ymin (\code{numeric}) Maximum value of y (latitude).
#' @param ymax (\code{numeric}) Minimum value of y (latitude).
#' 
#' @return A \code{sfc_POLYGON} class object from the sf package.
#' @examples
#' library(sf)
#' edge <- mapedge()
#' molledge <- st_transform(edge, crs="ESRI:54009")
#' 
#' @export
mapedge <- function(x=360, y=180, xmin=-180, xmax=180, ymin=-90, ymax=90){
	if(!requireNamespace("sf", quietly=TRUE)){
		stop("This function requires the 'sf' package to run!")
	}
	# return a rectangle
  	rectangle <- detailedBounds(x, y, xmin, xmax, ymin, ymax)

  	# now make it a SpatialPolygons
  	final <- sf::st_geometry(sf::st_polygon(list(rectangle)))
	sf::st_crs(final) <- 4326

  	# return object
  	return(final)
}

#' @rdname arraylength
#' @exportMethod nlayers
setMethod(
	"nlayers",
	signature="list",
	function(x) length(x)
)

#' @rdname arraylength
#' @exportMethod nlayers
setMethod(
	"nlayers",
	signature="SpatRaster",
	function(x){
	 dims <- dim(x)
	 return(dims[3])
	}
)
