% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SpatialImage.R
\name{SpatialImage-class}
\alias{SpatialImage-class}
\alias{SpatialImage}
\title{The SpatialImage class}
\usage{
SpatialImage(grob = NULL, path = NULL, url = NULL)
}
\arguments{
\item{grob}{an object of class \code{rastergrob}}

\item{path}{image file name (.png, .jpg or .tif)}

\item{url}{URL to source the image from}
}
\value{
a \code{SpatialImage}
}
\description{
The SpatialImage class
}
\examples{
dir <- file.path("extdata", "10xVisium", "section1", "spatial")
fnm <- file.path(dir, "tissue_lowres_image.png")
imgPath <- system.file(fnm, package = "SpatialExperiment")
SpatialImage(path = imgPath)

}
\seealso{
\itemize{
\item{accessors: \code{\link{imgGrob}}, \code{\link{imgPath}}, \code{\link{imgUrl}}}
\item{setters: \code{\link{imgGrob<-}}, \code{\link{imgPath<-}}, \code{\link{imgUrl<-}}}
\item{methods: \code{\link{loadImg}}, \code{\link{unloadImg}}}
}
}
\author{
Helena L. Crowell
}