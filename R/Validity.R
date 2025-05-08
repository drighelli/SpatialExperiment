# SpatialExperiment validity ---------------------------------------------------

#' @importFrom SingleCellExperiment int_colData
.spatialCoords_validity <- function(x, msg=NULL) {
    y <- int_colData(x)$spatialCoords
    if (is.null(y)) {
        msg <- c(msg, "no 'spatialCoords' field in 'int_colData'")
    } else if (!(is.matrix(y) && is.numeric(y))) {
        msg <- c(msg, paste(
            "'spatialCoords' field in 'int_colData'",
            "should be a numeric matrix"))
    }
    return(msg)
}

.colData_validity <- function(obj, msg=NULL) {
    df <- colData(obj)
    if (is.null(df$sample_id)) { 
        msg <- c(msg, "no 'sample_id' field in 'colData'")
    } else {
        if (!is.null(imgData(obj))) {
            sids <- unique(df$sample_id)
            isids <- unique(imgData(obj)$sample_id)
            # it's allowed to have samples with missing images but not 
            # vice versa, i.e. all imgData samples must be in colData
            if (!all(isids %in% sids)) {
                msg <- c(msg, paste(
                    "sample_id(s) don't match",
                    "between colData and imgData"))
            }
        }
    }
    return(msg)
}

#' @importFrom methods is
.imgData_validity <- function(obj, msg=NULL) {
    if (is(obj, "SpatialExperiment")) {
        df <- int_metadata(obj)$imgData
    } else {
        df <- obj
    }
    
    if (is.null(df)) {
        msg <- c(msg, "no 'imgData' field in 'int_metadata'")
    } else if (!is(df, "DFrame")) {
        msg <- c(msg, "'imgData' field in 'int_metadata' should be a 'DFrame'")
    }
    
    if (isEmpty(df))
        return(msg)
    
    nms <- c("sample_id", "image_id", "data", "scaleFactor")
    if (!identical(nms, names(df)))
        msg <- c(msg, paste(
            "'imgData' field in 'int_metadata' should have columns:",
            paste(sQuote(nms), collapse = ", ")))
    
    is_list <- is.list(df$data)
    is_vsi <- \(.) is(., "VirtualSpatialImage")
    all_vsi <- all(vapply(df$data, is_vsi, logical(1)))
    if (!(is_list && all_vsi))
        msg <- c(msg, paste(
            "'data' field in 'int_metadata' field 'imgData'",
            "should be a list of 'SpatialImage' objects"))
    
    if (!is.numeric(df$scaleFactor))
        msg <- c(msg, paste(
            "'scaleFactor' column in 'int_metadata'",
            "field 'imgData' should be 'numeric'"))
    
    return(msg)
}

.spe_validity <- function(object) {
    msg <- NULL
    msg <- .colData_validity(object, msg)
    msg <- .imgData_validity(object, msg)
    msg <- .spatialCoords_validity(object, msg)
    if (length(msg)) return(msg)
    return(TRUE)
}

#' @importFrom S4Vectors setValidity2
setValidity2("SpatialExperiment", .spe_validity)

# SpatialImage validity --------------------------------------------------------

.path_validity <- function(x) {
    is_valid <- all(c(
        length(x) == 1,
        is.character(x),
        file.exists(x),
        # TODO: any other possible image formats? 
        grepl("(\\.png|\\.jpg|\\.tif)$", x)))
    
    if (!is_valid)
        stop("'path' should be a length-one character",
            " string specifiying an image file (.png, .jpg or .tif)")
    return(TRUE)
}

# TODO: better way to check for URL validity without extra dependencies?

.url_validity <- function(x, t=1) {
    pat <- "^(www\\.|http(s)?://)"
    if (!grepl(pat, x))
        return(FALSE)
    
    suppressWarnings({
        con <- url(x)
        err <- try(open.connection(con, open="rt", timeout=t), silent=TRUE)
        try(close.connection(con), silent=TRUE)
        if (is.null(err))
            return(TRUE)
        stop("cannot connect to specified 'url'")
    })
}
