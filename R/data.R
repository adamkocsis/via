#' PaleoMAP PaleoCoastlines (excerpt)
#'
#' A dataset containing the coastline reconstructions based on the PaleoMAP PaleoDEMS \url{https://www.earthbyte.org/paleodem-resource-scotese-and-wright-2018/} and the Paleobiology Database \url{https://paleobiodb.org} for 0, 10 and 20Ma. 
#'
#' This is version v7. The article describing the entire set is under review. Once that is published, the entire dataset will be available.
#'
#' @format A \code{\link[via:SfcArray-class]{SfcArray}} with 3 continental margin and 3 paleocoastline layers (3 rows and 2 columns).
#' @source 
#' Kocsis, A. T., & Scotese, C. R. (2020). PaleoMAP PaleoCoastlines data [Data set]. Zenodo. https://doi.org/10.5281/zenodo.3903164
#' @usage data(paleocoastlines)
"paleocoastlines"

#' Example '\code{\link[via:XArray-class]{XArray}}'-class object
#'
#' A '\code{\link[via:XArray-class]{XArray}}'-class objects of \code{data.frame}s, which were made from a single \code{data.frame} with random sampling. The original object had two columns, the first (\code{x}) an integer seqence \code{1:100}, the second \code{y} a variable produced with \code{0.5 * x -30 + N(0,10)}.
#' 
#' @format: \code{\link[via:XArray-class]{XArray}} with 3 sample sizes (rows), and 4 different seeds (column). 
#' @usage data(exemplar)
"exemplar"

#' Procedural example structure to demonstrate the capabilities of the '\code{\link[via:RasterArray-class]{RasterArray}}' class
#'
#' Binary versions of \code{\link[terra:rast]{SpatRaster}}-class objects are problematic, this function is used to instantiate a \code{\link[via:RasterArray-class]{RasterArray}} example.
#' 
#' @return A two-dimensional \code{\link[via:RasterArray-class]{RasterArray}}-class object, with three rows and four columns.
#' @rdname rastex
#' @examples
#' # create example
#' example <- rastex()
#' 
#' # subset - single bracket
#' example['b']
#' 
#' # subset - single bracket
#' example[c(4, 6)]
#' 
#' # subset - double bracket
#' example[["layer_2"]]
#' @export
rastex <- function(){

	if(!requireNamespace("terra", quietly=TRUE)){
		stop("This function requires the 'terra' package.")
	}

    r1 <- terra::rast(res=c(1,1))
    terra::values(r1) <- 1:terra::ncell(r1)

    stack <- c(r1, r1+1,r1+2, r1+3, r1+4, r1+5)
    names(stack) <- paste("layer", 1:6, sep="_")

    one <- RasterArray(stack)
    names(one) <- letters[1:length(one)]
    return(one)
}

