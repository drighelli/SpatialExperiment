# helper to get the row index/indices of image(s) with matching
# 'sample_id' & 'image_id' from the 'imgData' field in 'int_metadata'
# - TRUE returns all available entries
# - NULL return first available entry
# - character string returns matching entry(ies)
.get_img_idx <- function(x, sample_id=NULL, image_id=NULL) {
    img <- imgData(x)
    for (i in c("sample_id", "image_id")) {
        j <- get(i)
        if (is.factor(j) || is.numeric(j))
            assign(i, as.character(j))
        if (!(is.null(j) || j %in% img[[i]] ||
            length(j) == 1 && is.logical(j)))
            stop(sprintf(c(
                "'%s' invalid; should be NULL, TRUE/FALSE,",
                " or matching entries in imgData(.)$%s"), i))
    }
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
        stop("No 'imgData' entry(ies) matched the specified", 
            sprintf(" 'image_id = %s' and 'sample_id = %s'", 
                dQuote(image_id), dQuote(sample_id)))
    return(which(idx))
}

#' @importFrom grDevices as.raster
#' @importFrom magick image_read
#' @importFrom S4Vectors DataFrame
.get_imgData <- function(img, scaleFactor, sample_id, image_id, load=TRUE, ...) {
    is_path <- tryCatch(
        error=function(e) e, 
        .path_validity(img))
    
    is_url <- tryCatch(
        error=function(e) e, 
        .url_validity(img))
    
    if (!(isTRUE(is_path) || isTRUE(is_url)))
        stop("Image should be supplied as a length-one character",
            " string specifiying an image file (.png or .jpg),",
            " or a valid URL to source from")
    
    if (load) {
        img <- image_read(img)
        img <- as.raster(img)
    } 
    spi <- SpatialImage(img)
    DataFrame(
        sample_id, 
        image_id,
        data=I(list(spi)),
        scaleFactor=scaleFactor,
        ...
    )
}
