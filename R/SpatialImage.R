#' @name SpatialImage-class
#' @title The SpatialImage class
#' 
#' @aliases
#' SpatialImage
#' SpatialImage-class
#' LoadedSpatialImage-class
#' StoredSpatialImage-class
#' RemoteSpatialImage-class
#' dim,SpatialImage-method
#' imgRaster 
#' imgRaster,LoadedSpatialImage-method
#' imgRaster,StoredSpatialImage-method
#' imgRaster,RemoteSpatialImage-method
#' imgRaster<- 
#' imgRaster<-,LoadedSpatialImage-method 
#' imgSource 
#' imgSource,LoadedSpatialImage-method
#' imgSource,StoredSpatialImage-method
#' imgSource,RemoteSpatialImage-method
#' imgSource<-
#' imgSource<-,StoredSpatialImage,character-method
#' imgSource<-,RemoteSpatialImage,character-method
#' coerce,SpatialImage,LoadedSpatialImage-method
#' coerce,RemoteSpatialImage,StoredSpatialImage-method
#' 
#' @description 
#' The \code{SpatialImage} class hierarchy provides representations of images
#' from a variety of sources. It is used by the \linkS4class{SpatialExperiment}
#' class to manage the loading of images across multiple studies.
#' 
#' @section Constructor:
#' \code{SpatialImage(x, is.url)} will return a SpatialImage object.
#' The class of the object depends on the type of \code{x}:
#' \itemize{
#' \item If \code{x} is a raster object, a LoadedSpatialImage is returned.
#'   This represents an image that is fully realized into memory, 
#'   where the raster representation is stored inside the output object.
#' \item If \code{x} is a string and \code{is.url=TRUE} or it starts with 
#'   \code{"http://"}, \code{"http://"} or \code{"ftp://"}, 
#'   a RemoteSpatialImage is returned. This represents an image 
#'   that is remotely hosted and retrieved only on request.
#' \item If \code{x} is a string and \code{is.url=TRUE} or it does not 
#'   start with a URL-like prefix, a StoredSpatialImage is returned.
#'   This represents an image that is stored in a local file 
#'   and is loaded into memory only on request.
#' }
#'
#' @section Getting the raster image:
#' For a SpatialImage object \code{x}, \code{imgRaster(x, ...)} 
#' will return a raster object (see \code{?\link{as.raster}}).
#' This is effectively a matrix of RGB colors for each pixel in the image.
#'
#' For a StoredSpatialImage object \code{x}, additional arguments 
#' in \code{...} are passed to \code{\link{image_read}}.
#' This controls how the image is read into memory.
#'
#' For a RemoteSpatialImage object \code{x}, the image file is first
#' downloaded before the raster is returned. Here, \code{...} may contain
#' an extra \code{cache} argument, which should be a BiocFileCache object 
#' (from the \pkg{BiocFileCache} package) specifying the file cache location. 
#' The default location is determined by 
#' \code{options("SpatialExperiment.remote.cache.path")},
#' otherwise it defaults to a subdirectory in the R temporary directory.
#' Any further named arguments in \code{...} are passed to \code{image_read}.
#'
#' \code{as.raster(x, ...)} is the same as \code{imgRaster(x, ...)}.
#'
#' @section In-memory caching:
#' For StoredSpatialImage and RemoteSpatialImage objects, 
#' loading the image with \code{imgRaster} will automatically 
#' store the loaded raster object in an in-memory cache.
#' Any subsequent \code{imgRaster} call will retrieve the raster 
#' from the cache, avoiding costly retrieval from the file system.
#'
#' The cache policy is to evict the least recently used images when 
#' a new image would be added that exceeds the maximum cache size.
#' If the new image by itself exceeds the maximum cache size, all images are 
#' evicted from the cache to trigger garbage collection and free up memory.
#'
#' By default, the maximum size of the cache is 4 GB. This can be 
#' modified by setting \code{options("SpatialExperiment.cache.size")} 
#' to some number of bytes, e.g., \code{2^32}.
#' 
#' @section Other methods:
#' \code{dim(x)} will return an integer vector of length 2, 
#' containing the width and height of the image in pixels.
#' Note that this calls \code{imgRaster} under the hood and thus 
#' may interact with the file and memory caches as described above.
#'
#' For any SpatialImage \code{x}, \code{as(x, "LoadedSpatialImage")} 
#' will create a LoadedSpatialImage containing an in-memory raster object.
#'
#' For a RemoteSpatialImage \code{x}, \code{as(x, "StoredSpatialImage")} 
#' will create a StoredSpatialImage pointing to the file cache location.
#'
#' @author Aaron Lun
#'
#' @examples
#' path <- system.file(
#'   "extdata", "10xVisium", "section1", "spatial", 
#'   "tissue_lowres_image.png", package="SpatialExperiment")
#'
#' spi <- SpatialImage(path)
#' plot(imgRaster(spi))
#'
#' # the following operations all use the cache 
#' # so there is no need to reload the image
#' nrow(spi)
#' ncol(spi)
#' plot(as.raster(spi)) 
#'
#' # coercing to an explicitly in-memory raster
#' spi <- as(spi, "LoadedSpatialImage")
#' plot(as.raster(spi))
NULL

#' @importFrom grDevices is.raster
#' @export
SpatialImage <- function(x, is.url=NULL) {
    if (is(x, "SpatialImage")) {
        x
    } else if (is.raster(x)) {
        new("LoadedSpatialImage", image=x)
    } else if (is.character(x)) {
        if (is.null(is.url)) {
            is.url <- grepl("^(https?|ftp)://", x)
        }
        if (is.url) {
            new("RemoteSpatialImage", url=x)
        } else {
            new("StoredSpatialImage", path=normalizePath(x))
        }
    } else {
        stop("unknown input type for 'x'")
    }
}

# getters ----------------------------------------------------------------------

#' @export
setMethod("imgRaster", "LoadedSpatialImage", function(x) x@image)

#' @export
#' @importFrom magick image_read image_destroy
setMethod("imgRaster", 
    "StoredSpatialImage", 
    function(x, ...) {
        path <- normalizePath(x@path)
        FUN <- function() {
            img <- image_read(path, ...)
            on.exit(image_destroy(img))
            as.raster(img)
        }        
        # adding file:// to protect against the hypothetical
        # case where a file is named after a URL
        .get_from_cache(paste0("file://", path), FUN)
    })

#' @export
#' @importFrom magick image_read image_destroy
setMethod("imgRaster", 
    "RemoteSpatialImage", 
    function(x, cache=NULL, ...) {
        URL <- x@url
        FUN <- function() {
            path <- .remote_file_cache(URL, cache)
            img <- image_read(path, ...)
            on.exit(image_destroy(img))
            as.raster(img)
        }        
        .get_from_cache(URL, FUN)
    })

#' @export
setMethod("imgSource", "LoadedSpatialImage", function(x) NA_character_)

#' @export
setMethod("imgSource", "StoredSpatialImage", function(x) x@path)

#' @export
setMethod("imgSource", "RemoteSpatialImage", function(x) x@url)

#' @export
setMethod("dim", "SpatialImage", function(x) dim(imgRaster(x)))

# setters ----------------------------------------------------------------------

#' @export
setReplaceMethod("imgRaster",
    c("LoadedSpatialImage", "ANY"),
    function(x, value) {
        x@image <- value
        return(x)
    })

#' @export
setReplaceMethod("imgSource", 
    c("StoredSpatialImage", "character"), 
    function(x, value) { 
        .path_validity(value)
        x@path <- value
        return(x) 
    })

#' @export
setReplaceMethod("imgSource", 
    c("RemoteSpatialImage", "character"), 
    function(x, value) { 
        .url_validity(value)
        x@url <- value
        return(x) 
    })

# coercion ---------------------------------------------------------------------

#' @export
#' @method as.raster SpatialImage
as.raster.SpatialImage <- function(x, ...) imgRaster(x, ...)

setAs("SpatialImage", 
    "LoadedSpatialImage", 
    function(from) {
        img <- imgRaster(from)
        new("LoadedSpatialImage", image=img)
    }
)

setAs("RemoteSpatialImage", 
    "StoredSpatialImage", 
    function(from) {
        path <- .remote_file_cache(from@url, cache=NULL)
        new("StoredSpatialImage", path=path)
    }
)
