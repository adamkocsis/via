# to be used as index in the the Arrays
setClassUnion("arrayORmatrixORvector", c("vector", "matrix", "array"))


VirtualArray <- setClass("VirtualArray", slots=list(index="arrayORmatrixORvector", stack="ANY"))


#' Virtual array of general R objects
#' 
#' Template for construction of virtual arrays ('\code{VirtualArray}') and a derived class ('\code{XArray}') to instantiate it with general objects.
#'
#' The '\code{VirtualArray}' class implements structures to organize objects of the same class in multidimensional arrays. Subsetting rules were defined using the proxy object in the \code{index} slot. The '\code{VirtualArray}' is the base class for '\code{XArray}' and '\code{\link[via:RasterArray-class]{RasterArray}}' classes.
#' The '\code{XArray}' class derived from \code{VirtualArray} allows the instantiation of basic virtual arrays with genearl R objects, which form a single \code{list} in the \code{@stack slot}. The '\code{\link[via:SfArray-class]{SfArray}}' class is derived from the '\code{XArray}' class.
#' 
#' The class has two slots:
#' \code{@stack}: A list containing objects of the same class (i.e. layers).
#' \code{@index}: A proxy object that represents the structure of the entities. 
#' 
#' 
#' @param stack A \code{list}-class object.
#' @param index A \code{vector}, \code{matrix} or \code{array} type object. Includes the indices of layers in the stack.
#' @param dim A \code{numeric} vector. Same as for \code{array}, creates \code{proxy} procedurally.
#' @return An \code{XArray}-class object.
#' @examples
#' # 2d XArray of vectors
#'   data(exemplar)
#'   st <-exemplar@stack
#'   ind <- 1:nlayers(st)
#'   dim(ind) <- c(3,4)
#'   dimnames(ind) <- list(n = c(10, 20, 30), seed = 1:4)
#'   xa<- XArray(stack=st, index=ind)
#'   
#' @exportClass XArray
XArray <- setClass("XArray", contains="VirtualArray")

#' Array of '\code{\link[terra:rast]{SpatRaster}}'-class objects
#' 
#' Array class for easier navigation of multilayer rasters 
#' 
#' The class implements structures to organize single-layer '\code{\link[terra:rast]{SpatRaster}}'-class objects that have the same dimensions and coordinate reference system. Subsetting rules were defined using the proxy object in the \code{@index} slot. See examples for implementations.
#' 
#' The class has two slots:
#' \code{@stack}: A '\code{\link[terra:rast]{SpatRaster}}'-class object with multiple layers, the actual data.
#' \code{index}: A proxy object that represents the organization of the layer in the array. 
#' 
#' @param stack A \code{\link[terra:rast]{SpatRaster}} object.
#' @param index A \code{vector}, \code{matrix} or \code{array} type object. Includes either the indices of layers in the stack, or their names.
#' @param dim A \code{numeric} vector. Same as for \code{array}, creates \code{proxy} procedurally.
#' @return A '\code{\link[via:RasterArray-class]{RasterArray}}'-class object.
#' @examples
#' # example data
#'   ex <- rastex()
#'   st <-ex@stack
#'   ind <- 1:6
#'   names(ind) <- letters[1:length(ind)]
#'   ra<- RasterArray(stack=st, index=ind)
#'   
#' @exportClass RasterArray
RasterArray <- setClass("RasterArray", contains="VirtualArray")


#' Array of '\code{\link[sf:sf]{sf}}'-derived class data 
#' 
#' Array class for easier navigation of vector spatial datasets 
#' 
#' The class implements structures to organize entire '\code{\link[sf:sfc]{sfc}}' and '\code{\link[sf:sf]{sf}}' objects that share coordinate reference systems. The 'SfcArray' class is derived from '\code{\link[via:XArray-class]{XArray}}' and represents arrays of geometry sets. The '\code{SfArray}' class is derived from '\code{SfArray}', that allows the wrapping of '\code{\link[sf:sf]{sf}}' objects with attributes. Subsetting rules were defined using the proxy object in the \code{@index} slot. See examples for implementations. 
#' 
#' The classes have two slots:
#' \code{@stack}: A \code{list} object with multiple '\code{\link[sf:sf]{sf}}' class layers, the actual data.
#' \code{@index}: A proxy object that represents the organization of the layers. 
#' 
#' @param stack A \code{list} of \code{sf}-class objects or \code{sfc}-class objects.
#' @param index A \code{vector}, \code{matrix} or \code{array} type object. Includes either the indices of layers in the stack, or their names.
#' @param dim A \code{numeric} vector. Same as for \code{array}, creates \code{proxy} procedurally.
#' @return An '\code{\link[via:SfArray-class]{SfcArray}}' or '\code{\link[via:SfArray-class]{SfArray}}'-class object.
#' @rdname SfArray-class
#' @examples
#' # example data
#'   library(sf) 
#'   data(paleocoastlines) 
#'   st <-paleocoastlines@stack
#'   ind <- 1:nlayers(st)
#'   dim(ind) <- c(3,2)
#'   dimnames(ind) <- list(age=c(0, 10, 20), c("margin", "coastlines"))
#'   sa<- SfcArray(stack=st, index=ind)
#'   
#' @exportClass SfcArray
SfcArray <- setClass("SfcArray", contains="XArray")




#' @name SfArray
#' @aliases SfArray-class 
#' @rdname SfArray-class
#' @exportClass SfArray
SfArray <- setClass("SfArray", contains="SfcArray")



