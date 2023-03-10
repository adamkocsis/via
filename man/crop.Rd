% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R, R/RasterArray-rast.R
\name{crop}
\alias{crop}
\alias{crop,RasterArray-method}
\title{Cropping}
\usage{
\S4method{crop}{RasterArray}(x, y, ...)
}
\arguments{
\item{x}{a \code{RasterArray} class object.}

\item{y}{an xtent object, or any object from which an Extent object can be extracted (see Details)}

\item{...}{arguments passed to the \code{\link[terra]{crop}} function.}
}
\value{
A cropped \code{RasterArray} class object.
}
\description{
The method is inherited from the \code{RasterStack} class.
}
\examples{
ex <- rastex()
# crop to a specific area
if(requireNamespace("terra", quietly=TRUE)){
 ext <- terra::ext(c(                
   xmin = 106.58,
   xmax = 157.82,
   ymin = -45.23,
   ymax = 1.14 
 )) 
 # cropping all 
 au<- crop(ex, ext)
}

}
