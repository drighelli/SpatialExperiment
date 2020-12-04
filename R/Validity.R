# SpatialExperiment validity ---------------------------------------------------

.colData_sample_id_validity <- function(x, msg=NULL) 
{
    if (!is.character(x$sample_id))
        msg <- c(msg, "'sample_id' field in 'colData' should be 'character'")
    
    if (!is.null(img <- imgData(x))) {
        sids <- unique(x$sample_id)
        if (!all(img$sample_id %in% sids))
            msg <- c(msg, "all 'sample_id's in 'imgData'",
                " should be in the 'colData's 'sample_id' field")
    }
}

.colData_inTissue_validity <- function(x, msg=NULL) 
{
    is_valid <- is.logical(x)
    if (!is_valid) 
        msg <- c(msg, "'inTissue' field in 'colData' should be 'logical'")
    return(msg)
}

.colData_xyzData_validity <- function(x, msg=NULL) 
{ ##### to change
    # allow 2 or 3 columns to support z-coordinate
    #is_valid <- all(c(is.matrix(x), ncol(x) %in% c(2, 3), is.numeric(x)))
    is_valid <- all(c(ncol(x) %in% c(2, 3), is.numeric(x)))
    if (!is_valid)
        msg <- c(msg, paste(
            "'xyzData' field in 'colData'",
            "should be a two- or three-column numeric matrix"))
    return(msg)
}

.colData_validity <- function(df, msg=NULL)
{
    if (is.null(df$sample_id)) {
        msg <- c(msg, "no 'sample_id' field in 'colData'")
    } else {
        # TODO: check validity of 'sample_id's wrt 'imgData'
    }
    
    # if (is.null(df$inTissue)) {
    #     msg <- c(msg, "no 'inTissue' field in 'colData'")
    # } else if (!is.logical(df$inTissue))
    #     msg <- .colData_inTissue_validity(df$inTissue, msg)
    # 
    # if (is.null(df$xyzData)) {
    #     msg <- c(msg, "no 'xyzData' field in 'colData'")
    # } else {
    #     msg <- .colData_xyzData_validity(df$xyzData, msg)
    # }
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
    msg <- .colData_validity(colData(object), msg)
    msg <- .imgData_validity(imgData(object), msg)
    if (length(msg)) 
        return(msg)
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

# VisiumExperiment validity ----------------------------------------------------

#' .ve_validity <- function(object)
#' {
#'     msg <- NULL
#'     if( sum(c("inTissue", "array_row", "array_col",
#'         "pxl_col_in_fullres", "pxl_row_in_fullres")
#'         %in% colnames(spatialCoords(object))) != 5 ) 
#'     {
#'         msg <- c(msg, paste0("Please use the 10x Visium colnames for the",
#'             " spatial coordinates. (Defaults are 'inTissue, 'array_row'", 
#'             " 'array_col', 'pxl_col_in_fullres', 'pxl_row_in_fullres')"))
#'     }
#'     
#'     if( sum(c("spot_diameter_fullres", "tissue_hires_scalef",
#'         "fiducial_diameter_fullres", "tissue_lowres_scalef")
#'         %in% names(scaleFactors(object))) != 4 )
#'     {
#'         msg <- c(msg, paste0("Please use the 10x Visium names for the",
#'             " scale factors. (Required are 'spot_diameter_fullres', 
#'             ' tissue_hires_scalef', 'fiducial_diameter_fullres', 
#'             ' tissue_lowres_scalef')"))
#'     }
#'     
#'     if(length(msg)) { return(msg) }
#'     
#'     return(TRUE)
#' }
#' 
#' #' @importFrom S4Vectors setValidity2
#' setValidity2("VisiumExperiment", .ve_validity)
