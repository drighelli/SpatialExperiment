# SpatialExperiment validity ---------------------------------------------------

.colData_sample_id_validity <- function(x, msg=NULL) 
{
    if ( !is.character(x$sample_id) )
        msg <- c(msg, "'sample_id' field in 'colData' should be 'character'")
    
    if ( !is.null(img <- imgData(x)) ) 
    {
        sids <- unique(x$sample_id)
        if ( !all(img$sample_id %in% sids) )
            msg <- c(msg, "all 'sample_id's in 'imgData'",
                " should be in the 'colData's 'sample_id' field")
    }
}

.spatialData_validity <- function(df, coordnames, msg=NULL) 
{
    is_valid <- all( apply(df[, coordnames], 2, is.numeric), 
                    TRUE)
    if (!is_valid)
        msg <- c(msg, paste("'spatialData' fields aren't valid"))
    return(msg)
}

.colData_validity <- function(obj, msg=NULL)
{
    df <- colData(obj)
    if ( is.null(df$sample_id) )  
    {
        msg <- c(msg, "no 'sample_id' field in 'colData'")
    } else {
        if ( !is.null(imgData(obj)) ) 
        {
            sids <- unique(df$sample_id)
            isids <- unique(imgData(obj)$sample_id)
            if ( any( !(sids %in% isids), !(isids %in% sids) ) )
            {
                msg <- c(msg, "sample_id(s) don't match between ",
                         "imgData and colData")
            }
        }
    }
    return(msg)
}

#' @importFrom methods is
.imgData_validity <- function(df, msg=NULL)
{
    if (is.null(df))
        return(msg)
    
    if (!is(df, "DFrame")) 
        msg <- c(msg, "'imgData' field in 'int_metadata' should be a 'DFrame'")
    nms <- c("sample_id", "image_id", "data", "width", "height", "scaleFactor")
    if (!identical(nms, names(df)))
        msg <- c(msg, paste(
            "'imgData' field in 'int_metadata' should have columns:",
            paste(sQuote(nms), collapse = ", ")))
    
    is_list <- is.list(df$data)
    all_sis <- all(vapply(df$data, is, class2="SpatialImage", logical(1)))
    if (!(is_list && all_sis))
        msg <- c(msg, paste(
            "'data' field in 'int_metadata' field 'imgData'",
            "should be a list of 'SpatialImage' objects"))
    
    if (!all(vapply(list(df$width, df$height), is.integer, logical(1))))
        msg <- c(msg, paste(
            "'width' and 'height' columns in 'int_metadata'",
            "field 'imgData' should be 'integer'"))
    
    if (!is.numeric(df$scaleFactor))
        msg <- c(msg, paste(
            "'scaleFactor' column in 'int_metadata'",
            "field 'imgData' should be 'numeric'"))
    
    return(msg)
}

.spe_validity <- function(object)
{
    msg <- NULL
    msg <- .colData_validity(object, msg)
    msg <- .spatialData_validity(object@spatialData, object@spaCoordsNms, msg)
    msg <- .imgData_validity(imgData(object), msg)
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
