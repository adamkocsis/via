# Functions that are practically inherited from Raster*

#' Resolution of a RasterArray object
#' 
#' The methods are inherited from the \code{RasterStack} class, see \code{\link[raster]{resolution}}. Replacement is not allowed.
#' 
#' @param x a \code{RasterArray} class object.
#' @return A \code{numeric} vector.
#' 
#' @rdname res
#' @examples
#' data(dems)
#' res(dems)
#' yres(dems)
#' xres(dems)
#' @export xres
setMethod(
	"xres",
	signature="RasterArray",
	function(x){
		xres(x@stack)
	} 
)


#	setMethod(
#		"filename",
#		signature="RasterArray",
#		function(x){
#			filename(x@stack)
#		} 
#	)

#' @rdname res
#' @export yres
setMethod(
	"yres",
	signature="RasterArray",
	function(x){
		yres(x@stack)
	} 
)

#' @rdname res
#' @export res
setMethod(
	"res",
	signature="RasterArray",
	function(x){
		res(x@stack)
	} 
)


#' Rotate a \code{\link[chronosphere:RasterArray-class]{RasterArray}} object
#' 
#' The method is inherited from the \code{\link[raster:raster]{RasterStack}} class.
#' 
#' @param x (\code{\link[chronosphere:RasterArray-class]{RasterArray}}) Object.
#' @param ... Additional arguments passed to the \code{\link[raster]{rotate}} function.
#' @rdname rotate
#' @return A \code{\link[chronosphere:RasterArray-class]{RasterArray}}-class object.
#' @name rotate
NULL

#' @rdname rotate
setMethod(
	"rotate",
	signature=c("RasterArray"),
	function(x,...){
		x@stack <- terra::rotate(x@stack,...)
		return(x)
	}
)



#' @rdname ext
setMethod(
	"ext",
	signature=c("RasterArray"),
	function(x,...){
		a <- ext(x@stack,...)
		return(a)
	}
)


#' Resample a RasterArray object
#' 
#' The method is inherited from the \code{RasterStack} class.
#' 
#' @param x a \code{RasterArray} class object.
#' @param y The y argument of the \code{\link[raster]{resample}} function.
#' @return A resampled \code{RasterArray} class object.
#' @param ... arguments passed to the \code{\link[raster]{resample}} function.
#' 
#' @examples
#' data(dems)
#' template <- raster(res=5)
#' resampled <- resample(dems, template)
#' @exportMethod resample
setMethod(
	"resample",
	signature=c("RasterArray", "ANY"),
	function(x,y,...){
		x@stack <- resample(x@stack, y,...)
		return(x)
	}
)

#' Crop a RasterArray object
#' 
#' The method is inherited from the \code{RasterStack} class.
#' 
#' @param x a \code{RasterArray} class object.
#' @param y an xtent object, or any object from which an Extent object can be extracted (see Details)
#' @param ... arguments passed to the \code{\link[raster]{crop}} function.
#' @return A cropped \code{RasterArray} class object.
#' 
#' @examples
#' data(dems)
#' # crop to Australia
#' ext <- ext(c(                
#'   xmin = 106.58,
#'   xmax = 157.82,
#'   ymin = -45.23,
#'   ymax = 1.14 
#' )) 
#' # cropping all DEMS (Australia drifted in)
#' au<- crop(dems, ext)
#' 
#' @exportMethod crop
setMethod(
	"crop",
	signature=c("RasterArray"),
	function(x,y,...){
		x@stack <- terra::crop(x@stack,y,...)
		return(x)
	}
)

#' Aggregate raster cells in a \code{\link[chronosphere:RasterArray-class]{RasterArray}} object
#' 
#' The method is inherited from the \code{\link[raster:raster]{RasterStack}} class.
#' 
#' @param x a \code{\link[chronosphere:RasterArray-class]{RasterArray}}-class object.
#' @param ... arguments passed to the \code{\link[raster]{aggregate}} function.
#' 
#' @exportMethod aggregate
#' @return An aggregated \code{\link[chronosphere:RasterArray-class]{RasterArray}} class object.
#' @examples
#' data(dems)
#' agg <- aggregate(dems, 5)
#' @rdname aggregate
#' @name aggregate
NULL

#' @rdname aggregate
setMethod(
	"aggregate",
	signature=c("RasterArray"),
	function(x,...){
		x@stack <- terra::aggregate(x@stack,...)
		return(x)
	}
)

#' Disaggregate raster cells in a RasterArray object
#' 
#' The method is inherited from the \code{RasterStack} class.
#' 
#' @param x a \code{RasterArray} class object.
#' @return A disaggregated \code{RasterArray} class object.
#' @param ... arguments passed to the \code{\link[raster]{disaggregate}} function.
#' 
#' @exportMethod disagg
#' @examples
#' data(dems)
#' disagg <- disagg(dems, 3)
#' @rdname disagg
#' @name disagg
NULL

#' @rdname disagg
setMethod(
	"disagg",
	signature=c("RasterArray"),
	function(x,...){
		x@stack <- disagg(x@stack,...)
		return(x)
	}
)



#' @rdname extract
setMethod(
	"extract",
	signature=c(x="RasterArray", y="matrix"),
	definition=function(x, y){
		# rasters
		dimX <- dim(x)
		# coordinates
		nrowY <- nrow(y)
		
		# depending on dim of X
		if(length(dimX)==1){
			xname <- list(names(x))
		}else{
			xname <- dimnames(x)
		}
		if(length(x)>0){
			if(nlayers(x)>0){
				# will create a 2d solution - rows: points, columns-rasters
				temp <- terra::extract(x@stack, y)

				# in 2d
				prox <- proxy(x)
				# flatten
				dim(prox) <-NULL

				output <- newbounds(temp, cols=prox)

				# reset the dim
				dim(output) <-  c(nrowY, dimX)
				dimnames(output) <- c(list(rownames(y)), xname)
			}else{
				output <- array(NA, dim=c(nrowY, dimX))
			}

		}else{
			output <- NULL
		}
		return(output)

	}
)

#' @param margin (\code{numeric}) A single value describing which margin (dimension of \code{x}) \code{by} is referring to (1: rows, 2: columns, etc.).
#' @rdname extract
setMethod(
	"extract", 
	signature=c(x="RasterArray", y="data.frame"), 
	definition=function(x, y, by=NULL, margin=1, lng="plng", lat="plat", force=NULL){
	
	# fall back to matrix-method
	if(is.null(by)) {
		if(any(!c(lng, lat)%in%colnames(y))) stop(paste0(lng, " and/or ", lat, " are not among the column names of y."))		
		y <- as.matrix(y[, c(lng, lat)])
		vals <- extract(x, y)
	
	# data.frame-proper method - with by
	}else{
	#	dimX <- dim(x)
	#	if(length(dimX)>2) stop("'x' has to be a one or two-dimensional RasterArray.")

		# column that contains which map the coordinate belongs to
		if(is.character(by)){
			if(!by%in%colnames(y)) stop("The argument by has to be a column of y. ")
			interactor <- y[, by]
		# separate vector
		}else{
			if(is.vector(by) & (length(by)==nrow(y))){
				interactor <- by
			}else{
				stop("Invalid by argument.")
			}
		}
		
		# force subscript type
		if(!is.null(force)){
			if(force=="numeric"){
				interactor <- as.numeric(as.character(interactor))
			}
			if(force=="character"){
				interactor <- as.character(interactor)
			}
		}else{
			if(class(interactor)=="factor"){
				warning("Factor variable 'by' is forced to character.")
				interactor <- as.character(interactor)
			}
		}
	
		# iterate by
		doFor <- sort(unique(interactor))

		# coordinates
		coords <- as.matrix(y[, c(lng, lat)])
		rownames(coords) <- rownames(y)
	
		if(is.null(dimnames(x))){
			marginNames <- names(x)
			otherNames <- NULL
		}else{
			marginNames <- dimnames(x)[[margin]]
			otherNames <- dimnames(x)[-margin]
		}

		# dimensions of x - for RasterArray, vectors have dim
		dimX <- dim(x)
		
		# create the output structure
		output <- array(NA, dim=c(length(interactor), dimX[-margin]))
		dimnames(output) <- c(
			list(rownames(y)),
			otherNames
		)

		# make the index lookup table
		indexLookup <- matrix(NA, nrow=nrow(y), ncol=prod(dimX[-margin]))
		rownames(indexLookup) <- rep(NA, nrow(indexLookup))

		# offset value
		offset <- 0

		# coordcounter
		coordcounter <- 0

		# define static later!
		flatArray <- NULL

		# should warnings be triggered
		warn <- FALSE

		for(i in 1:length(doFor)){
			# select the appropriate part of the RasterArray
			subArr <- marginsubset(x, margin, doFor[i])
			
			# what if this is NA - non-existant layer is referred
			if(class(subArr)!="RasterArray" & class(subArr)!="RasterLayer"){
				if(is.na(subArr)){
					warn <- TRUE
				}else{
					warning("Unexpected case. Please send your function call to the package maintainers.")
				}
				
			}else{

				# get the appropriate coordinates
				iThis<- which(interactor==doFor[i])
				subCoords <- coords[iThis, , drop=FALSE]

				# extract the values with the matrix method
				partArray <- extract(subArr,subCoords)

				# move forward only if this thing makes sense
				# NULL output for empty rasterarray
				if(!is.null(partArray)){

					# store the whole thing
					flatArray<-c(flatArray, as.numeric(partArray))

					# get the indices
						indArray <- partArray
						indArray[] <- 1:length(indArray)

						# increase the offset
						indArray <- indArray + offset

						# copy the index Array 
						# they should go to these rows
						coordIndex <- coordcounter + 1:nrow(subCoords)

						# it working, but I am not sure that it is ok!
						indexLookup[coordIndex,] <- indArray
						rownames(indexLookup)[coordIndex] <- rownames(subCoords)

						# the offsets
						# processed coordiantes
						coordcounter <- coordcounter + nrow(subCoords)

						# indices
						offset <- offset +length(indArray)
				}
			}
		}

		# reorder based on the originally defined output
		newIndex <- newbounds(indexLookup, rows=rownames(coords))
		
		# then paste this in!
		reordered <- flatArray[as.numeric(newIndex)]

		output[] <- reordered
		vals <- output

		# do some formatting
		if(length(dim(output))==1){
			vals <- as.numeric(vals)
			names(vals) <- names(output)
		}
		if(warn) warning("Argument 'by' refers to non-existant elements in 'x'. ")

	}
		
	return(vals)

})


#' @rdname project
setMethod("project", "RasterArray", 
	function(x, y, ...){
		x@stack <- terra::project(x=x@stack, y=y, ...)
		return(x)
	}
)




