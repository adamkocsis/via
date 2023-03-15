# NOTE: sf uses S3, if it is not available, the substitute generics have to be defined for S3 and not S4!

#' Names of layers in the \code{stack} of a '\code{\link[via:XArray-class]{VirtualArray}}'-class object
#' 
#' @param x A \code{\link[via:XArray-class]{VirtualArray}}-derived class object.
#' @param ... additional arguments passed to class-specific methods.
#' @return A \code{character} vector of names.
#' @exportMethod layers
#' 
#' @examples
#' # names of layers in the stack
#' data(exemplar)
#' layers(exemplar)
#' @rdname layers
setGeneric("layers", function(x,...) standardGeneric("layers"))





#' @rdname arraylength
setGeneric(
	name="nlayers",
	def=function(x){
		standardGeneric("nlayers")
	}
)

#' Dimensions of layers in a '\code{\link[via:XArray-class]{VirtualArray}}'-class object
#' 
#' The funcion will return the dimensions '\code{\link[terra:rast]{SpatRaster}}'-class layers.
#' 
#' @param x A \code{\link[via:XArray-class]{VirtualArray}} class object.
#' @return A \code{numeric} vector with the number of rows and columns in the \code{\link[via:XArray-class]{VirtualArray}}s.
#' @param ... additional arguments passed to class-specific methods.
#' 
#' @rdname dimlayer
#' @exportMethod dimlayer
setGeneric("dimlayer", function(x,...) standardGeneric("dimlayer"))


#' The total number of values in a '\code{\link[via:RasterArray-class]{RasterArray}}'-class object
#' 
#' @param x A \code{\link[via:RasterArray-class]{RasterArray}}-class object.
#' @param ... additional arguments passed to class-specific methods.
#' @return A \code{numeric} value.
#' 
#' @export nvalues
#' @examples 
#' ex <- rastex()
#' nvalues(ex)
#' @rdname nvalues
setGeneric("nvalues", function(x,...) standardGeneric("nvalues"))


# Generics from terra
# ncell from 
#' @name ncell
#' @rdname ncell
if(!requireNamespace("terra", quietly=TRUE)){
	setGeneric("ncell", function(x) standardGeneric("ncell"))
}else{
	setGeneric("ncell", def=terra::ncell, package="terra")
}

# Generics from terra
#' @name xres
#' @rdname res
if(!requireNamespace("terra", quietly=TRUE)){
	setGeneric("xres", function(x) standardGeneric("xres"))
}else{
	setGeneric("xres", def=terra::xres, package="terra")
}

#' @name yres
#' @rdname res
if(!requireNamespace("terra", quietly=TRUE)){
	setGeneric("yres", function(x) standardGeneric("yres"))
}else{
	setGeneric("yres", def=terra::yres, package="terra")
}

#' Resolution of a '\code{\link[via:RasterArray-class]{RasterArray}}'-class object
#' 
#' @name res
#' @rdname res
if(!requireNamespace("terra", quietly=TRUE)){
	setGeneric("res", function(x) standardGeneric("res"))
}else{
	setGeneric("res", def=terra::res, package="terra")
}

#' Resampling a '\code{\link[via:RasterArray-class]{RasterArray}}'-class object
#' 
#' @name resample
#' @rdname resample
if(!requireNamespace("terra", quietly=TRUE)){
	setGeneric("resample", function(x,y,...) standardGeneric("resample"))
}else{
	setGeneric("resample", def=terra::resample, package="terra")
}



#' Cropping a '\code{\link[via:RasterArray-class]{RasterArray}}'-class object
#' 
#' @name crop
#' @rdname crop
if(!requireNamespace("terra", quietly=TRUE)){
	setGeneric("crop", function(x,y,...) standardGeneric("crop"))
}else{
	setGeneric("crop", def=terra::crop, package="terra")
}

if(!requireNamespace("terra", quietly=TRUE)){
	setGeneric("aggregate", function(x,...) standardGeneric("aggregate"))
}else{
	setGeneric("aggregate", def=terra::aggregate, package="terra")
}

if(!requireNamespace("terra", quietly=TRUE)){
	setGeneric("disagg", function(x,...) standardGeneric("disagg"))
}else{
	setGeneric("disagg", def=terra::disagg, package="terra")
}

#' Project a '\code{\link[via:RasterArray-class]{RasterArray}}'-class object
#'
#' The method implemets the \code{\link[terra]{project}} function for '\code{\link[via:RasterArray-class]{RasterArray}}'-class objects.
#' 
#' @param x A \code{\link[via:RasterArray-class]{RasterArray}} object to project.
#' @param y A \code{\link[via:RasterArray-class]{RasterArray}} the same options as in \code{\link[terra]{project}}.
#' @param ... additional arguments as for \code{\link[terra]{project}}.
#' @rdname project
#' @return A projected \code{\link[via:RasterArray-class]{RasterArray}}-class object.
#' @export project
#' @examples
#' # project first three to mollweide
#' ex <- rastex() 
#' mollEx <- project(ex[1:3], y="ESRI:54009")
"project"
if(!requireNamespace("terra", quietly=TRUE)){
	setGeneric("project", function(x,...) standardGeneric("project"))
}else{
	setGeneric("project", def=terra::project, package="terra")
}

if(!requireNamespace("terra", quietly=TRUE)){
	setGeneric("mask", function(x,...) standardGeneric("mask"))
}else{
	setGeneric("mask", def=terra::mask, package="terra")
}

if(!requireNamespace("terra", quietly=TRUE)){
	setGeneric("rotate", function(x,...) standardGeneric("rotate"))
}else{
	setGeneric("rotate", def=terra::rotate, package="terra")
}

#' Extent of a '\code{\link[via:RasterArray-class]{RasterArray}}'-class object
#' 
#' The method is inherited from the '\code{\link[terra:rast]{SpatRaster}}' class.
#' 
#' @param x a \code{\link[via:RasterArray-class]{RasterArray}}-class object.
#' @param ... arguments passed to the \code{\link[terra]{ext}} function.
#' 
#' @return An aggregated \code{\link[via:RasterArray-class]{RasterArray}}-class object.
#' @examples
#' ex <- rastex()
#' extent <- ext(ex)
#' @rdname ext
#' @export 
#' @name ext
if(!requireNamespace("terra", quietly=TRUE)){
	setGeneric("ext", function(x,...) standardGeneric("ext"))
}else{
	setGeneric("ext", def=terra::ext, package="terra")
}


#' Coordinate reference system of an '\code{\link[via:SfArray-class]{SfArray}}'-class object
#' 
#' The method is inherited from the '\code{\link[sf:sf]{sf}}' class.
#' 
#' @param x a \code{\link[sf:sf]{sf}}-class object.
#' @param ... arguments passed to the \code{\link[sf:st_crs]{st_crs}} function.
#' 
#' @return An aggregated \code{\link[via:RasterArray-class]{RasterArray}} class object.
#' @examples
#' data(paleocoastlines)
#' crs <- st_crs(paleocoastlines)
#' @rdname st_crs
#' @name st_crs
#' @export 
if(!requireNamespace("sf", quietly=TRUE)){
	st_crs <- function(x, ...){
		UseMethod("st_crs", x)
	}
#	setGeneric("st_crs", function(x) standardGeneric("st_crs")) # WRONG, S3 instead of S4!
}else{
	setGeneric("st_crs", def=sf::st_crs, package="sf")
}

#' Projection change of an '\code{\link[via:SfArray-class]{SfArray}}'-class object
#' 
#' The method is inherited from the '\code{\link[sf:sf]{sf}}' class.
#' 
#' @param x a \code{\link[sf:sf]{sf}}-class object.
#' @param ... arguments passed to the \code{\link[raster]{extent}} function.
#' 
#' @return An \code{\link[via:RasterArray-class]{RasterArray}}-class object.
#' @examples
#' data(paleocoastlines)
#' moll<- st_transform(paleocoastlines, "ESRI:54009")
#' plot(moll["20", "margin"]$geometry, col="cyan")
#' plot(moll["20", "coastline"]$geometry, add=TRUE, col="brown")
#' @rdname st_transform
#' @name st_transform
#' @export 
if(!requireNamespace("sf", quietly=TRUE)){
	st_transform <- function(x,...){
		UseMethod("st_transform",x)
	}
#	setGeneric("st_transform", function(x) standardGeneric("st_transform"))# WRONG, S3 instead of S4!
}else{
	setGeneric("st_transform", def=sf::st_transform, package="sf")
}

#' Bounding box of an '\code{\link[via:SfArray-class]{SfArray}}'-class object
#' 
#' The method is inherited from the '\code{\link[sf:sf]{sf}}' class.
#' 
#' @param x a \code{\link[sf:sf]{sf}}-class object.
#' @param ... arguments passed to the \code{\link[raster]{extent}} function.
#' 
#' @return An \code{\link[via:RasterArray-class]{RasterArray}} class object.
#' @examples
#' data(paleocoastlines)
#' bb<- st_bbox(paleocoastlines)
#' @rdname st_bbox
#' @name st_bbox
#' @export 
if(!requireNamespace("sf", quietly=TRUE)){
#	setGeneric("st_bbox", function(x) standardGeneric("st_bbox"))# WRONG, S3 instead of S4!
	st_bbox <- function(x,...){
		UseMethod("st_bbox",x)
	}
}else{
	setGeneric("st_bbox", def=sf::st_bbox, package="sf")
}
