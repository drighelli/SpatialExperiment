#' @name SpatialImage-class
#' @title The SpatialImage class
#' 
#' @param grob an object of class \code{rastergrob}
#' @param path image file name (.png, .jpg or .tif)
#' @param url URL to source the image from
#' 
#' @return a \code{SpatialImage}
#'   
#' @author Helena L. Crowell
#' 
#' @seealso 
#' \describe{
#' \item{accessors:}{ \code{\link{imgGrob}}, \code{\link{imgPath}}, \code{\link{imgUrl}}}
#' \item{setters:}{ \code{\link{imgGrob<-}}, \code{\link{imgPath<-}}, \code{\link{imgUrl<-}}}
#' \item{methods:}{ \code{\link{loadImg}}, \code{\link{unloadImg}}}
#' }
#' @examples
#' dir <- file.path("extdata", "10xVisium", "section1", "spatial")
#' fnm <- file.path(dir, "tissue_lowres_image.png")
#' imgPath <- system.file(fnm, package = "SpatialExperiment")
#' SpatialImage(path = imgPath)
#' 
#' @importFrom methods new
#' @export
SpatialImage <- function(grob=NULL, path=NULL, url=NULL) 
    new("SpatialImage", grob=grob, path=path, url=url)
