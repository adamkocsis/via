#' Names of \code{\link{RasterArray}} or \code{\link{SpatialArray}} Layers in the stack
#' 
#' @param x A \code{\link{RasterArray}} or \code{\link{SpatialArray}} class object.
#' @param ... additional arguments passed to class-specific methods.
#' @return A \code{character} vector of names.
#' @exportMethod layers
#' 
#' @examples
#' # names of layers in the stack
#' data(dems)
#' layers(dems)
#' @rdname layers
setGeneric("layers", function(x,...) standardGeneric("layers"))





setGeneric(
	name="nlayers",
	def=function(x){
		standardGeneric("nlayers")
	}
)

#' Dimensions of RasterLayers in a RasterArray object
#' 
#' The funcion will return the dimensions RasterLayers
#' 
#' @param x A \code{RasterArray} class object.
#' @return A \code{numeric} vector with the number of rows and columns in the \code{RasterLayer}s.
#' @param ... additional arguments passed to class-specific methods.
#' 
#' @rdname dimlayer
#' @exportMethod dimlayer
setGeneric("dimlayer", function(x,...) standardGeneric("dimlayer"))


#' The total number of values in a RasterArray object
#' 
#' @param x A \code{RasterArray} class object.
#' @param ... additional arguments passed to class-specific methods.
#' @return A \code{numeric} value.
#' 
#' @export nvalues
#' @examples 
#' data(dems)
#' nvalues(dems)
#' @rdname nvalues
setGeneric("nvalues", function(x,...) standardGeneric("nvalues"))


# Generics from terra
# ncell from 
if(!requireNamespace("terra", quietly=TRUE)){
	setGeneric("ncell", function(x) standardGeneric("ncell"))
}else{
	setGeneric("ncell", def=terra::ncell, package="terra")
}

# Generics from terra
# ncell from 
if(!requireNamespace("terra", quietly=TRUE)){
	setGeneric("xres", function(x) standardGeneric("xres"))
}else{
	setGeneric("xres", def=terra::xres, package="terra")
}

if(!requireNamespace("terra", quietly=TRUE)){
	setGeneric("yres", function(x) standardGeneric("yres"))
}else{
	setGeneric("yres", def=terra::yres, package="terra")
}

if(!requireNamespace("terra", quietly=TRUE)){
	setGeneric("res", function(x) standardGeneric("res"))
}else{
	setGeneric("res", def=terra::res, package="terra")
}

if(!requireNamespace("terra", quietly=TRUE)){
	setGeneric("resample", function(x,y,...) standardGeneric("resample"))
}else{
	setGeneric("resample", def=terra::resample, package="terra")
}

#' Extraction of values from multiple RasterLayers in a RasterArray object
#' 
#' The function takes a set of time-dependent coordinates and extracts the value they point to from associted RasterLayers in a RasterArray.
#' 
#' @param y (\code{matrix} or \code{data.frame}). The data table containing the coordinates and (optionally) 
#' 	the indices or names of the associated \code{RasterLayer}s in \code{x}.
#' @param x (\code{RasterArray}). A set of \code{RasterLayers} that are associated with entries (one dimension) or the rows of \code{x}.
#' @param by (\code{character} or \code{vector}) In case of a \code{data.frame} input, the link between \code{x} and \code{y}. If \code{by} is a \code{character}
#' string then it is expected to be column of \code{x} and should contain the \code{names} or the indices of the
#' associated \code{RasterLayer}s in \code{x}. If it is a \code{vector} its length should match the number of rows in \code{x}
#' and it will be used as if it were a column of \code{x}.
#' @param lng (\code{character}) A column of \code{x} that includes the paleolongitudes.
#' @param lat (\code{character}) A column of \code{x} that includes the paleolatitudes.
#' @param force (\code{character}) If set to \code{"numeric"} the \code{by} argument or the column it points to will be converted to
#' numeric values, and x will be subsetted with \code{numeric} subscripts of the \code{x} \code{RasterArray}. If set to \code{"character"}, the by column (or vector) will be
#' forced to \code{character} values and will be used as character subscripts.
#' @rdname extract
#' @name extract
#' @return A \code{numeric} \code{vector}, \code{matrix} or \code{array} of values.
#' @examples
#' # one pair of random coordinates from Africa
#' mat <- matrix(c(                
#'   -1.34, 42.96
#' ), ncol=2, byrow=TRUE) 
#' 
#' # repeat four times
#' mat<- mat[rep(1,4), ]
#' 
#' # assign default names and age
#' df<- data.frame(plng=mat[, 1],plat=mat[, 2], age=c(1,3,5, 1))
#' rownames(df) <- paste("point", 1:nrow(df))
#' 
#' # first coordinate pair will be extrated from RasterLayer 1 ["0"]
#' # second coordinate pair will be extracted from RasterLayer 3 ["10"]
#' # thrid coordinate pair will be extracted from RasterLayer 5 ["20"]
#' # fourth coordinate pair will be extracted from RasterLayer 1 ["0"]
#' data(dems)
#' extract(dems, df, by="age")
#' 
#' # by=NULL will be implemented in the next update 
#' # (all coordinates extracted from all layers)
#' @export extract
"extract"

if(!requireNamespace("terra", quietly=TRUE)){
	setGeneric("extract", function(x,y,...) standardGeneric("extract"))
}else{
	setGeneric("extract", def=terra::extract, package="terra")
}

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
	setGeneric("disagg", function(x) standardGeneric("disagg"))
}else{
	setGeneric("disagg", def=terra::disagg, package="terra")
}

#' Project a RasterArray object
#'
#' The method implemets the \code{\link[raster]{projectRaster}} function for \code{RasterArray} class objects.
#' 
#' @param from A \code{Raster*} \code{RasterArray} object to project.
#' @param to \code{Raster*} object with the parameters to which 'from' should be projected
#' @param res single or (vector of) two numerics. To, optionally, set the output resolution if 'to' is missing
#' @param crs character or object of class 'CRS'. PROJ.4 description of the coordinate reference system. In projectRaster this is used to set the output CRS if 'to' is missing, or if 'to' has no valid CRS
#' @param method method used to compute values for the new RasterLayer. Either 'ngb' (nearest neighbor), which is useful for categorical variables, or 'bilinear' (bilinear interpolation; the default value), which is appropriate for continuous variables.
#' @param alignOnly \code{logical}. Use to or other parameters only to align the output (i.e. same origin and resolution), but use the projected extent from from
#' @param over \code{logical}. If TRUE wrapping around the date-line is turned off. This can be desirable for global data (to avoid mapping the same areas twice) but it is not desireable in other cases
#' @param filename \code{character} output filname. Not applicable for RasterArray class objects.
#' @param ... additional arguments as for \code{\link[raster]{writeRaster}}.
#' @rdname project
#' @return A projected \code{RasterArray} class object.
#' @export project
#' @examples
#' # project first three to mollweide
#' data(dems)
#' suppressWarnings(
#'   mollDem <- project(dems[1:3], crs=CRS("+proj=moll"))
#' )
"project"
if(!requireNamespace("terra", quietly=TRUE)){
	setGeneric("project", function(x,...) standardGeneric("project"))
}else{
	setGeneric("project", def=terra::project, package="terra")
}

if(!requireNamespace("terra", quietly=TRUE)){
	setGeneric("mask", function(x) standardGeneric("mask"))
}else{
	setGeneric("mask", def=terra::mask, package="terra")
}

if(!requireNamespace("terra", quietly=TRUE)){
	setGeneric("rotate", function(x) standardGeneric("rotate"))
}else{
	setGeneric("rotate", def=terra::rotate, package="terra")
}

#' Extent of a \code{\link[chronosphere:RasterArray-class]{RasterArray}} object
#' 
#' The method is inherited from the \code{\link[raster:raster]{RasterStack}} class.
#' 
#' @param x a \code{\link[chronosphere:RasterArray-class]{RasterArray}}-class object.
#' @param ... arguments passed to the \code{\link[raster]{extent}} function.
#' 
#' @exportMethod ext
#' @return An aggregated \code{\link[chronosphere:RasterArray-class]{RasterArray}} class object.
#' @examples
#' data(dems)
#' agg <- ext(dems)
#' @rdname ext
#' @name ext
if(!requireNamespace("terra", quietly=TRUE)){
	setGeneric("ext", function(x) standardGeneric("ext"))
}else{
	setGeneric("ext", def=terra::ext, package="terra")
}


#' Coordinate_reference system of a \code{\link[genarray:SfArray-class]{SfArray}} object
#' 
#' The method is inherited from the \code{\link[sf:sf]{sf}} class.
#' 
#' @param x a \code{\link[sf:sf-class]{sf}}-class object.
#' @param ... arguments passed to the \code{\link[raster]{extent}} function.
#' 
#' @return An aggregated \code{\link[chronosphere:RasterArray-class]{RasterArray}} class object.
#' @examples
#' data(dems)
#' agg <- ext(dems)
#' @rdname st_crs
#' @name st_crs
if(!requireNamespace("sf", quietly=TRUE)){
	setGeneric("st_crs", function(x) standardGeneric("st_crs"))
}else{
	setGeneric("st_crs", def=sf::st_crs, package="sf")
}

#' Projection change a \code{\link[genarray:SfArray-class]{SfArray}} object
#' 
#' The method is inherited from the \code{\link[sf:sf]{sf}} class.
#' 
#' @param x a \code{\link[sf:sf-class]{sf}}-class object.
#' @param ... arguments passed to the \code{\link[raster]{extent}} function.
#' 
#' @return An \code{\link[chronosphere:RasterArray-class]{RasterArray}} class object.
#' @examples
#' data(dems)
#' agg <- ext(dems)
#' @rdname st_transform
#' @name st_transform
if(!requireNamespace("sf", quietly=TRUE)){
	setGeneric("st_transform", function(x) standardGeneric("st_transform"))
}else{
	setGeneric("st_transform", def=sf::st_transform, package="sf")
}

#' Bounding box of a \code{\link[genarray:SfArray-class]{SfArray}} object
#' 
#' The method is inherited from the \code{\link[sf:sf]{sf}} class.
#' 
#' @param x a \code{\link[sf:sf-class]{sf}}-class object.
#' @param ... arguments passed to the \code{\link[raster]{extent}} function.
#' 
#' @return An \code{\link[chronosphere:RasterArray-class]{RasterArray}} class object.
#' @examples
#' data(dems)
#' agg <- ext(dems)
#' @rdname st_bbox
#' @name st_bbox
if(!requireNamespace("sf", quietly=TRUE)){
	setGeneric("st_bbox", function(x) standardGeneric("st_transform"))
}else{
	setGeneric("st_bbox", def=sf::st_transform, package="sf")
}
