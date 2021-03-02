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
#'   if FALSE, will store the path/URL instead
#' 
#' @return 
#' \code{getImg()} returns a single or list of \code{SpatialImage}(s).
#' 
#' \code{add/rmvImg()} return a \code{\link{SpatialExperiment}} 
#' with modified \code{imgData}; specifically, they create/remove 
#' an image entry (row) in the \code{imgData} \code{DataFrame}.
#' 
#' \code{imgRaster/Source()} access relevant data in the \code{SpatialImage}(s)
#' stored inside the \code{imgData}'s \code{data} field. 
#' Depending on whether or not multiple entries are accesses,
#' a character string or vector is returned by \code{imgSource()}, and a 
#' single or list of \code{raster} object(s) is returned by \code{imgRaster()}.
#'   
#' @examples
#' example(read10xVisium)
#' 
#' # 'SpatialImage' accession
#' (spi <- getImg(ve))
#' plot(imgRaster(spi))
#' 
#' # remove an image
#' imgData(ve)
#' ve <- rmvImg(ve,
#'   sample_id = "section1",
#'   image_id = "lowres")
#' imgData(ve)
#' 
#' # add an image
#' url <- "https://i.redd.it/3pw5uah7xo041.jpg"
#' ve <- addImg(ve,
#'   sample_id = "section1",
#'   image_id = "pomeranian",
#'   imageSource = url,
#'   scaleFactor = NA_real_,
#'   load = FALSE)
#' 
#' # extract image
#' img <- imgRaster(ve,
#'   sample_id = "section1",
#'   image_id = "pomeranian")
#' plot(img)
#'   
#' @author Helena L. Crowell
NULL

# getImg -----------------------------------------------------------------------

#' @rdname imgData-methods
#' @export
setMethod("getImg", "SpatialExperiment",
    function(x, sample_id=NULL, image_id=NULL) 
    {
        spi <- imgData(x)$data
        idx <- .get_img_idx(x, sample_id, image_id)
        if (length(idx) == 1) spi[[idx]] else spi[idx]
    })

# addImg -----------------------------------------------------------------------

#' @rdname imgData-methods
#' @export
setMethod("addImg", "SpatialExperiment",
    function(x, imageSource, scaleFactor, sample_id, image_id, load=TRUE) 
    {
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
        
        # get & add valid 'imgData' entry
        df <- .get_imgData(imageSource, scaleFactor, sample_id, image_id, load)
        imgData(x) <- rbind(imgData(x), df)
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
    function(x, sample_id=NULL, image_id=NULL) 
    {
        spi <- getImg(x, sample_id, image_id)
        if (is.list(spi)) {
            vapply(spi, imgSource, character(1)) 
        } else imgSource(spi)
    })

#' @rdname imgData-methods
#' @export
setMethod("imgRaster", "SpatialExperiment", 
    function(x, sample_id=NULL, image_id=NULL) 
    {
        spi <- getImg(x, sample_id, image_id)
        if (is.list(spi)) {
            lapply(spi, imgRaster) 
        } else imgRaster(spi)
    })
