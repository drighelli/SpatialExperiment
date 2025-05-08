#' @name imgData-methods
#' @title Methods for handling image-related data
#' 
#' @aliases 
#' getImg addImg rmvImg
#' getImg,SpatialExperiment-method
#' addImg,SpatialExperiment-method
#' rmvImg,SpatialExperiment-method
#' imgRaster,SpatialExperiment-method
#' imgSource,SpatialExperiment-method
#' rotateImg,SpatialExperiment-method
#' mirrorImg,SpatialExperiment-method
#' 
#' @description 
#' The set of functions described below is designed to handle 
#' the image-related data stored inside a \code{SpatialExperiment}'s 
#' \code{imgData} \code{int_metadata} field. These include:
#' 
#' \itemize{
#' \item \code{getImg}, \code{addImg}, \code{rmvImg}
#'   to retrieve/add/remove an image entry to/from 
#'   the \code{imgData} \code{DataFrame}
#' \item \code{imgSource}, \code{imgRaster}
#'   to retrieve the path/URL and \code{raster} object,
#'   respectively, associated with an image or set of images
#' }
#' 
#' @param x a \code{\link{SpatialExperiment}}
#' @param sample_id character string, \code{TRUE} or \code{NULL} specifying 
#'   sample/image identifier(s); here, \code{TRUE} is equivalent to all 
#'   samples/images and \code{NULL} specifies the first available entry (see details)
#' @param image_id see \code{sample_id}
#' @param imageSource 
#'   a character string specifying an image file name 
#'   (.png, .jpg or .tif) or URL to source the image from
#' @param scaleFactor 
#'   single numeric scale factor used to rescale spatial 
#'   coordinates according to the image's resolution
#' @param load logical; should the image(s) be 
#'   loaded into memory as a \code{raster} object?
#'   if \code{FALSE}, will store the path/URL instead
#' @param degrees single numeric 
#'   in +/-[0,90,...,360] specifying how many degrees to rotate.
#'   A negative/positive value corresponds to counter-/clockwise rotation
#' @param axis character string specifying whether to mirror 
#'   horizontally (\code{"h"}) or vertically (\code{"v"})
#' @param path logical; for \code{RemoteSpatialImage}s, TRUE 
#'   returns the path to the image's cached file, and FALSE its URL. 
#'   For \code{Stored/LoadedSpatialImage}s, a path/NA is returned, 
#'   irrespective of \code{path}.
#' @param ... Arguments for user-defined columns in \code{\link{imgData}}.
#' 
#' @return 
#' \code{getImg()} returns a single or list of \code{SpatialImage}(s).
#' 
#' \code{add/rmvImg()} return a \code{\link{SpatialExperiment}} 
#' with modified \code{imgData}; specifically, they create/remove 
#' an image entry (row) in the \code{imgData} \code{DataFrame}.
#' 
#' \code{imgRaster/Source()} access relevant data in the 
#' \code{SpatialImage}(s) stored inside the \code{imgData}'s \code{data} 
#' field. Depending on whether or not multiple entries are accessed,
#' a character string or vector is returned by \code{imgSource()}, and a 
#' single or list of \code{raster} object(s) is returned by \code{imgRaster()}.
#' 
#' \code{rotate/mirrorImg()} return a \code{Loaded\link{SpatialImage}}
#' with modified a \code{raster} matrix.
#'   
#' @examples
#' example(SpatialExperiment)
#' 
#' # 'SpatialImage' accession
#' (spi <- getImg(spe))
#' plot(imgRaster(spi))
#' 
#' # remove an image
#' imgData(spe)
#' spe <- rmvImg(spe,
#'   sample_id = "section1",
#'   image_id = "lowres")
#' imgData(spe)
#' 
#' # add an image
#' url <- "https://i.redd.it/3pw5uah7xo041.jpg"
#' spe <- addImg(spe,
#'   sample_id = "section1",
#'   image_id = "pomeranian",
#'   imageSource = url,
#'   scaleFactor = NA_real_,
#'   load = FALSE)
#' 
#' # extract image
#' img <- imgRaster(spe,
#'   sample_id = "section1",
#'   image_id = "pomeranian")
#' plot(img)
#' 
#' ###################
#' # transformations #
#' ###################
#' 
#' # clockwise rotation
#' spe1 <- rotateImg(spe, 
#'   degrees = 90) # first image
#'   
#' spe2 <- rotateImg(spe, 
#'   sample_id = TRUE,
#'   image_id = TRUE, 
#'   degrees = 90) # all images
#' 
#' par(mfrow = c(1, 3))
#' plot(imgRaster(spe))
#' plot(imgRaster(spe1))
#' plot(imgRaster(spe2))
#' 
#' # horizontal/vertical mirroring
#' spe1 <- mirrorImg(spe, axis = "h")
#' spe2 <- mirrorImg(spe, axis = "v")
#' 
#' par(mfrow = c(1, 3))
#' plot(imgRaster(spe))
#' plot(imgRaster(spe1))
#' plot(imgRaster(spe2))
#'   
#' @author Helena L. Crowell
NULL

# getImg -----------------------------------------------------------------------

#' @rdname imgData-methods
#' @importFrom S4Vectors isEmpty
#' @export
setMethod("getImg", "SpatialExperiment",
    function(x, sample_id=NULL, image_id=NULL) {
        if (isEmpty(imgData(x))) return(NULL)
        spi <- imgData(x)$data
        idx <- .get_img_idx(x, sample_id, image_id)
        if (length(idx) == 1) spi[[idx]] else spi[idx]
    })

# addImg -----------------------------------------------------------------------

#' @rdname imgData-methods
#' @export
setMethod("addImg", "SpatialExperiment",
    function(x, imageSource, scaleFactor, sample_id, image_id, load=TRUE, ...) {
        # check validity of input arguments
        stopifnot(
            is.numeric(scaleFactor), 
            length(scaleFactor) == 1,
            
            is.character(sample_id), 
            length(sample_id) == 1, 
            sample_id %in% x$sample_id,
            
            is.character(image_id), 
            length(image_id) == 1,
            
            is.logical(load), 
            length(load) == 1)
        
        is_path <- tryCatch(
            error=function(e) e, 
            .path_validity(imageSource))
        
        is_url <- tryCatch(
            error=function(e) e, 
            .url_validity(imageSource))
        
        if (!(isTRUE(is_path) || isTRUE(is_url)))
            stop("Invalid 'imageSource'; should be an",
                " image file name (.png or .jpg) or", 
                " URL to source the image from")
        
        # check that image entry doesn't already exist
        idx <- tryCatch(
            error=function(e) e,
            .get_img_idx(x, sample_id, image_id))
        
        if (!inherits(idx, "error"))
            stop("'imgData' already contains an entry with", 
                sprintf(
                    " 'image_id = %s' and 'sample_id = %s'", 
                    dQuote(image_id), dQuote(sample_id)))
        
        # current 'imgData' entry
        img_data <- imgData(x)
        
        # get an 'imgData' entry
        df <- .get_imgData(imageSource, scaleFactor, sample_id, image_id, load, ...)
        
        # check: same columns for both 'imgData' entries
        if (!is.null(img_data) && prod(dim(img_data)) > 0) {
            stopifnot(
                ncol(img_data) == ncol(df),
                identical(sort(colnames(img_data)), sort(colnames(df))))
        }
        
        # add to 'imgData' entry
        imgData(x) <- rbind(img_data, df)
        
        return(x)
    })

# rmvImg -----------------------------------------------------------------------

#' @rdname imgData-methods
#' @export
setMethod("rmvImg", "SpatialExperiment",
    function(x, sample_id=NULL, image_id=NULL) {
        idx <- .get_img_idx(x, sample_id, image_id)
        imgData(x) <- imgData(x)[-idx, , drop=FALSE]
        return(x)
    })

# getters ----------------------------------------------------------------------

#' @rdname imgData-methods
#' @export
setMethod("imgSource", "SpatialExperiment", 
    function(x, sample_id=NULL, image_id=NULL, path=FALSE) {
        spi <- getImg(x, sample_id, image_id)
        if (is.null(spi)) {
            NULL
        } else {
            if (!is.list(spi)) spi <- list(spi)
            vapply(spi, \(.) imgSource(., path), character(1)) 
        }
    })

#' @rdname imgData-methods
#' @export
setMethod("imgRaster", "SpatialExperiment", 
    function(x, sample_id=NULL, image_id=NULL) {
        spi <- getImg(x, sample_id, image_id)
        if (is.null(spi)) {
            NULL
        } else if (is.list(spi)) {
            lapply(spi, imgRaster) 
        } else {
            imgRaster(spi)
        }
    })

# transformations --------------------------------------------------------------

#' @rdname imgData-methods
#' @export
setMethod("rotateImg", "SpatialExperiment",
    function(x, sample_id=NULL, image_id=NULL, degrees=90) {
        old <- getImg(x, sample_id, image_id)
        if (!is.null(old)) {
            if (!is.list(old)) old <- list(old)
            new <- lapply(old, rotateImg, degrees=degrees) 
            idx <- .get_img_idx(x, sample_id, image_id)
            imgData(x)$data[idx] <- new
        }
        return(x)
    })

#' @rdname imgData-methods
#' @export
setMethod("mirrorImg", "SpatialExperiment",
    function(x, sample_id=NULL, image_id=NULL, axis=c("h", "v")) {
        old <- getImg(x, sample_id, image_id)
        if (!is.null(old)) {
            if (!is.list(old)) old <- list(old)
            new <- lapply(old, mirrorImg, axis=axis) 
            idx <- .get_img_idx(x, sample_id, image_id)
            imgData(x)$data[idx] <- new
        }
        return(x)
    })
