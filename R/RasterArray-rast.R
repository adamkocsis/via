# Functions that are practically inherited from SpatRaster*

#' Resolution of a RasterArray object
#' 
#' The methods are inherited from the \code{RasterStack} class, see \code{\link[terra]{res}}. Replacement is not allowed.
#' 
#' @param x a \code{RasterArray} class object.
#' @return A \code{numeric} vector.
#' 
#' @rdname res
#' @examples
#' ex <- rastex() 
#' res(ex)
#' yres(ex)
#' xres(ex)
#' @export xres
setMethod(
	"xres",
	signature="RasterArray",
	function(x){
		if(!requireNamespace("terra", quietly=TRUE)){
			stop("This function requires the terra package.")
		}	
		terra::xres(x@stack)
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
		if(!requireNamespace("terra", quietly=TRUE)){
			stop("This function requires the terra package.")
		}	
		terra::yres(x@stack)
	} 
)

#' @rdname res
#' @export res
setMethod(
	"res",
	signature="RasterArray",
	function(x){
		if(!requireNamespace("terra", quietly=TRUE)){
			stop("This function requires the terra package.")
		}	
		terra::res(x@stack)
	} 
)


#' Rotate a \code{\link[via:RasterArray-class]{RasterArray}} object
#' 
#' The method is inherited from the \code{\link[terra:rast]{SpatRaster}} class.
#' 
#' @param x (\code{\link[via:RasterArray-class]{RasterArray}}) Object.
#' @param ... Additional arguments passed to the \code{\link[terra]{rotate}} function.
#' @rdname rotate
#' @return A \code{\link[via:RasterArray-class]{RasterArray}}-class object.
#' @name rotate
NULL

#' @rdname rotate
setMethod(
	"rotate",
	signature=c("RasterArray"),
	function(x,...){
		if(!requireNamespace("terra", quietly=TRUE)){
			stop("This function requires the terra package.")
		}	
		x@stack <- terra::rotate(x@stack,...)
		return(x)
	}
)



#' @rdname ext
setMethod(
	"ext",
	signature=c("RasterArray"),
	function(x,...){
		if(!requireNamespace("terra", quietly=TRUE)){
			stop("This function requires the terra package.")
		}	
		a <- terra::ext(x@stack,...)
		return(a)
	}
)


#' Resample a RasterArray object
#' 
#' The method is inherited from the \code{SpatRaster} class.
#' 
#' @param x a \code{RasterArray} class object.
#' @param y The y argument of the \code{\link[terra]{resample}} function.
#' @return A resampled \code{RasterArray} class object.
#' @param ... arguments passed to the \code{\link[terra]{resample}} function.
#' 
#' @rdname resample
#' @aliases resample,RasterArray-method
#' @examples
#' ex <- rastex()
#' if(requireNamespace("terra", quietly=TRUE)){
#'   template <- terra::rast(res=5)
#'   resampled <- resample(ex, template)
#' }
#' @export resample
setMethod(
	"resample",
	signature=c("RasterArray", "ANY"),
	function(x,y,...){
		if(!requireNamespace("terra", quietly=TRUE)){
			stop("This function requires the terra package.")
		}	
		x@stack <- terra::resample(x@stack, y,...)
		return(x)
	}
)

#' Crop a RasterArray object
#' 
#' The method is inherited from the \code{RasterStack} class.
#' 
#' @param x a \code{RasterArray} class object.
#' @param y an xtent object, or any object from which an Extent object can be extracted (see Details)
#' @param ... arguments passed to the \code{\link[terra]{crop}} function.
#' @return A cropped \code{RasterArray} class object.
#' 
#' @examples
#' ex <- rastex()
#' # crop to a specific area
#' if(requireNamespace("terra", quietly=TRUE)){
#'  ext <- terra::ext(c(                
#'    xmin = 106.58,
#'    xmax = 157.82,
#'    ymin = -45.23,
#'    ymax = 1.14 
#'  )) 
#'  # cropping all 
#'  au<- crop(ex, ext)
#' }
#' 
#' @rdname crop
#' @exportMethod crop
setMethod(
	"crop",
	signature=c("RasterArray"),
	function(x,y,...){
		if(!requireNamespace("terra", quietly=TRUE)){
			stop("This function requires the terra package.")
		}	
		x@stack <- terra::crop(x@stack,y,...)
		return(x)
	}
)

#' Aggregate raster cells in a \code{\link[via:RasterArray-class]{RasterArray}} object
#' 
#' The method is inherited from the \code{\link[terra:rast]{SpatRaster}} class.
#' 
#' @param x a \code{\link[via:RasterArray-class]{RasterArray}}-class object.
#' @param ... arguments passed to the \code{\link[terra]{aggregate}} function.
#' 
#' @exportMethod aggregate
#' @return An aggregated \code{\link[via:RasterArray-class]{RasterArray}} class object.
#' @examples
#' library(terra)
#' ex <- rastex()
#' agg <- aggregate(ex, 30)
#' @rdname aggregate
#' @name aggregate
NULL

#' @rdname aggregate
setMethod(
	"aggregate",
	signature=c("RasterArray"),
	function(x,...){
		if(!requireNamespace("terra", quietly=TRUE)){
			stop("This function requires the terra package.")
		}	
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
#' @param ... arguments passed to the \code{\link[terra]{disagg}} function.
#' 
#' @exportMethod disagg
#' @examples
#' ex <- rastex() 
#' disagg <- disagg(ex, 3)
#' @rdname disagg
#' @name disagg
NULL

#' @rdname disagg
setMethod(
	"disagg",
	signature=c("RasterArray"),
	function(x,...){
		if(!requireNamespace("terra", quietly=TRUE)){
			stop("This function requires the terra package.")
		}	
		x@stack <- terra::disagg(x@stack,...)
		return(x)
	}
)




#' Projecting a RasterArray-class object
#'
#' Practically the same as terra::project
#'
#' 
#' @rdname project
setMethod("project", "RasterArray", 
	function(x, y, ...){
		if(!requireNamespace("terra", quietly=TRUE)){
			stop("This function requires the terra package.")
		}	
		x@stack <- terra::project(x=x@stack, y=y, ...)
		return(x)
	}
)




