#' Virtual Arrays for R
#'
#' The base class VirtualArray is defined, which acts as a wrapper around lists that allow users to fold arbitrary sequential data into n-dimensional, R-style virtual arrays. The derived VirtualArray class is defined to be used for homogeneous lists that contain a single class of objects. The RasterArray and SfArray classes enable the use of stacked spatial data instead of lists.
#' 
#' This is still the Beta version. As is R, this is free software and comes with ABSOLUTELY NO WARRANTY. Nevertheless, notes about found bugs and suggestions are more than welcome. 
#'
#' @author Adam T. Kocsis (adam.t.kocsis@gmail.com)
#' @docType package
#' @name via
NULL

#' @importFrom methods new cbind2 rbind2
NULL

