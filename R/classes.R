# to be used as index in the the Arrays
setClassUnion("arrayORmatrixORvector", c("vector", "matrix", "array"))

# base virtual class
VirtualArray <- setClass("VirtualArray", slots=list(index="arrayORmatrixORvector", stack="ANY"))


#' General Array 
#' 
#' Array template 
#' 
#' The class implements structures to organize RasterLayers that have the same dimensions. Subsetting rules were defined using the proxy object in the \code{index} slot. See examples for implementations.
#' 
#' The class has two slots:
#' stack: RasterStack, the actual data.
#' index: A proxy object that represents the organization of the layers. 
#' 
#' 
#' @param stack A \code{RasterStack} class object.
#' @param index A \code{vector}, \code{matrix} or \code{array} type object. Includes either the indices of layers in the stack, or their names.
#' @param dim A \code{numeric} vector. Same as for \code{array}, creates \code{proxy} procedurally.
#' @return A \code{RasterArray} class object.
#' @examples
#' # data import
#'   data(dems)
#'   st <-dems@stack
#'   ind <- 1:nlayers(st)
#'   names(ind) <- letters[1:length(ind)]
#'   ra<- RasterArray(stack=st, index=ind)
#'   
#' @exportClass XArray
XArray <- setClass("XArray", contains="VirtualArray")

#' Array of rasters 
#' 
#' Array class for easier navigation of multilayer rasters 
#' 
#' The class implements structures to organize single-layer SpatRasters that have the same dimensions. Subsetting rules were defined using the proxy object in the \code{index} slot. See examples for implementations.
#' 
#' The class has two slots:
#' stack: RasterStack, the actual data.
#' index: A proxy object that represents the organization of the layers. 
#' 
#' @param stack A \code{RasterStack} class object.
#' @param index A \code{vector}, \code{matrix} or \code{array} type object. Includes either the indices of layers in the stack, or their names.
#' @param dim A \code{numeric} vector. Same as for \code{array}, creates \code{proxy} procedurally.
#' @return A \code{RasterArray} class object.
#' @examples
#' # data import
#'   one <- oneRast()
#'   st <-one@stack
#'   ind <- 1:nlayers(st)
#'   names(ind) <- letters[1:length(ind)]
#'   ra<- RasterArray(stack=st, index=ind)
#'   
#' @exportClass RasterArray
RasterArray <- setClass("RasterArray", contains="VirtualArray")


#VectArray <- setClass("VectArray", contains="VirtualArray")
#SpArray <- setClass("SpArray", contains="VirtualArray")
SfArray <- setClass("SfArray", contains="XArray")



