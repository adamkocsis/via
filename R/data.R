#' Example structures to demonstrate the capabilities of the package
#' @rdname examples
#' @export
oneRast <- function(){

	if(!requireNamespace("terra", quietly=TRUE)){
		stop("This function requires the terra package.")
	}

    r1 <- terra::rast(res=c(90,90))
    values(r1) <- 1:terra::ncell(r1)

    stack <- c(r1, r1+1,r1+2, r1+3, r1+4, r1+5)
    names(stack) <- paste("layer", 1:nlayers(stack), sep="_")

    one <- RasterArray(stack)
    names(one) <- letters[1:length(one)]
    return(one)
}

#' @rdname examples
#' @export
twoRast <- function(){

	if(!requireNamespace("terra", quietly=TRUE)){
		stop("This function requires the terra package.")
	}

    r1 <- terra::rast(res=c(90,90))
    values(r1) <- 1:terra::ncell(r1)

    stack <- c(r1, r1+1,r1+2, r1+3, r1+4, r1+5)
    names(stack) <- paste("layer", 1:nlayers(stack), sep="_")

    ind <- matrix(1:nlayers(stack), ncol=2,nrow=3)
    colnames(ind) <- paste0("c", 1:ncol(ind))
    rownames(ind) <- paste0("r", 1:nrow(ind))

    two <- RasterArray(stack=stack, index=ind)
    return(two)
}
