#' PaleoMAP PaleoCoastlines (excerpt)
#'
#' A dataset containing the coastline reconstructions based on the PaleoMAP PaleoDEMS \url{https://www.earthbyte.org/paleodem-resource-scotese-and-wright-2018/} and the Paleobiology Database \url{https://paleobiodb.org} for 0, 10 and 20Ma. 
#'
#' This is version v7. The article describing the entire set is under review. Once that is published, the entire dataset will be available.
#'
#' @format A \code{\link[via:SfArray-class]{SfArray}} with 3 continental margin and 3 paleocoastline layers.
#' @source 
#' Kocsis, A. T., & Scotese, C. R. (2020). PaleoMAP PaleoCoastlines data [Data set]. Zenodo. https://doi.org/10.5281/zenodo.3903164
#' @usage data(paleocoastlines)
"paleocoastlines"

#' Example XArray object
#'
#' An XArray of \code{data.frames} made from a single \code{data.frame} with random sampling. The original object had two columns, the first (\code{x}) an integer seqence \code{1:100}, the second \code{y} a variable produced with \code{0.5 * x -30 + N(0,10)}.
#' 
#' @format: \code{\link[via:XArray-class]{XArray}} with 3 sample sizes (rows), and 4 different seeds (column). 
#' @usage data(exemplar)
"exemplar"

#' Procedural example structures to demonstrate the capabilities of the RasterArray-class
#'
#' 
#' 
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
		stop("This function requires the terra package.")
	}

    r1 <- terra::rast(res=c(1,1))
    terra::values(r1) <- 1:terra::ncell(r1)

    stack <- c(r1, r1+1,r1+2, r1+3, r1+4, r1+5)
    names(stack) <- paste("layer", 1:6, sep="_")

    one <- RasterArray(stack)
    names(one) <- letters[1:length(one)]
    return(one)
}

