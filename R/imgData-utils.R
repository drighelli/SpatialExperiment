# helper to load an image from a 'SpatialImage',
# giving precedence to path over URL &
# caching when loading from the latter
#' @importFrom BiocFileCache BiocFileCache bfcadd
#' @importFrom magick image_read image_info
#' @importFrom S4Vectors DataFrame
.load_img <- function(x) 
{
    # pass if image is already loaded
    if (!is.null(imgGrob(x))) { return(x) }
    
    # otherwise, path overrules URL
    if (is.null(path <- imgPath(x)))
        if (!is.null(url <- imgUrl(x))) {
            bfc <- BiocFileCache(ask = FALSE)
            path <- bfcadd(bfc, url)
        } else stop(
            "'SpatialImage' does not contain",
            " a path or URL to load from")
    
    img <- image_read(path)
    ii <- image_info(img)
    grb <- rasterGrob(img,
        width=unit(1, "npc"),
        height=unit(1, "npc"))
    imgPath(x) <- path
    imgGrob(x) <- grb
    DataFrame(
        data=I(list(x)),
        width=ii$width,
        height=ii$height)
}

# helper to get the row index/indices of image(s) with matching
# 'sample_id' & 'image_id' from the 'imgData' field in 'int_metadata'
# - TRUE returns all available entries
# - NULL return first available entry
# - character string returns matching entry(ies)
.get_img_idx <- function(x, sample_id=NULL, image_id=NULL)
{
    img <- imgData(x)
    if (is.character(sample_id) && is.character(image_id)) {
        sid <- img$sample_id == sample_id
        iid <- img$image_id == image_id
    } else if (isTRUE(sample_id) && isTRUE(image_id)) {
        sid <- iid <- !logical(nrow(img))
    } else if (is.null(sample_id) && is.null(image_id)) {
        sid <- iid <- diag(nrow(img))[1, ]
    } else if (is.character(sample_id) && isTRUE(image_id)) {
        sid <- img$sample_id == sample_id
        iid <- !logical(nrow(img))
    } else if (is.character(image_id) && isTRUE(sample_id)) {
        iid <- img$image_id == image_id
        sid <- !logical(nrow(img))
    } else if (is.character(sample_id) && is.null(image_id)) {
        sid <- img$sample_id == sample_id
        iid <- diag(nrow(img))[which(sid)[1], ]
    } else if (is.character(image_id) && is.null(sample_id)) {
        iid <- img$image_id == image_id
        sid <- diag(nrow(img))[which(iid)[1], ]
    } else if (isTRUE(sample_id) && is.null(image_id)) {
        iid <- match(unique(img$sample_id), img$sample_id)
        iid <- colSums(diag(nrow(img))[iid, , drop=FALSE])
        sid <- !logical(nrow(img))
    } else if (isTRUE(image_id) && is.null(sample_id)) {
        sid <- match(unique(img$image_id), img$image_id)
        sid <- colSums(diag(nrow(img))[sid, , drop=FALSE])
        iid <- !logical(nrow(img))
    }
    if (!any(idx <- sid & iid)) 
        stop("No 'imgData' entry(ies) matched the specified", sprintf(
            " 'image_id' and 'sample_id'", dQuote(c(image_id, sample_id))))
    return(which(idx))
}

#' @importFrom grid rasterGrob unit
#' @importFrom magick image_read image_info
#' @importFrom S4Vectors DataFrame
.get_imgData <- function(imageSource, scaleFactor, sample_id, image_id, load=TRUE)
{
    is_path <- tryCatch(error = function(e) e, .path_validity(imageSource))
    is_url <- tryCatch(error = function(e) e, .url_validity(imageSource))
    if (isTRUE(is_path)) {
        path <- imageSource; url <- NULL
    } else if (isTRUE(is_url)) {
        path <- NULL; url <- imageSource
    } else {
        stop("Image should be supplied as a length-one character",
            " string specifiying an image file (.png or .jpg),",
            " or a valid URL to source from")
    }
    if (load) {
        img <- image_read(imageSource)
        grob <- rasterGrob(img,
            width=unit(1, "npc"),
            height=unit(1, "npc"))
        si <- SpatialImage(grob, path, url)
        ii <- image_info(img)
    } else {
        si <- SpatialImage(NULL, path, url)
        ii <- list(width=NA_integer_, height=NA_integer_)
    }
    DataFrame(
        sample_id, 
        image_id,
        data=I(list(si)),
        width=ii$width,
        height=ii$height,
        scaleFactor=scaleFactor)
}
