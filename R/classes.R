# to be used as index in the the Arrays
setClassUnion("arrayORmatrixORvector", c("vector", "matrix", "array"))


VirtualArray <- setClass("VirtualArray", slots=list(index="arrayORmatrixORvector", stack="ANY"))


#' Virtual Array of general R objects
#' 
#' Array template 
#' 
#' The class implements structures to organize objects of the same class in multidimensional arrays. Subsetting rules were defined using the proxy object in the \code{index} slot. See examples for implementations.
#' 
#' The class has two slots:
#' stack: A list containing objects of the same class (i.e. layers).
#' index: A proxy object that represents the structure of the entities. 
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

#' Array of SpatRasters
#' 
#' Array class for easier navigation of multilayer rasters 
#' 
#' The class implements structures to organize single-layer SpatRasters that have the same dimensions. Subsetting rules were defined using the proxy object in the \code{index} slot. See examples for implementations.
#' 
#' The class has two slots:
#' stack: A SpatRaster object with multiple layers, the actual data.
#' index: A proxy object that represents the organization of the layers. 
#' 
#' @param stack A \code{SpatRaster} class object.
#' @param index A \code{vector}, \code{matrix} or \code{array} type object. Includes either the indices of layers in the stack, or their names.
#' @param dim A \code{numeric} vector. Same as for \code{array}, creates \code{proxy} procedurally.
#' @return A \code{RasterArray}-class object.
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


#' Array of Sf-class data 
#' 
#' Array class for easier navigation of vector spatial datasets 
#' 
#' The class implements structures to organize entire Sf-objects that share coordinate reference systesm. Subsetting rules were defined using the proxy object in the \code{index} slot. See examples for implementations. The class is derived from the \code{VirtualArray} class.
#' 
#' The class has two slots:
#' stack: A list object with multiple sf layers, the actual data.
#' index: A proxy object that represents the organization of the layers. 
#' 
#' @param stack A \code{list} of \code{sf}-class objects.
#' @param index A \code{vector}, \code{matrix} or \code{array} type object. Includes either the indices of layers in the stack, or their names.
#' @param dim A \code{numeric} vector. Same as for \code{array}, creates \code{proxy} procedurally.
#' @return An \code{SfArray}-class object.
#' @examples
#' # example data
#'   library(sf) 
#'   data(paleocoastlines) 
#'   st <-paleocoastlines@stack
#'   ind <- 1:nlayers(st)
#'   dim(ind) <- c(3,2)
#'   dimnames(ind) <- list(age=c(0, 10, 20), c("margin", "coastlines"))
#'   sa<- SfArray(stack=st, index=ind)
#'   
#' @exportClass RasterArray
SfArray <- setClass("SfArray", contains="XArray")



