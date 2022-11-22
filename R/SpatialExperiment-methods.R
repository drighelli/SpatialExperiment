#' @name SpatialExperiment-methods
#' 
#' @title Methods for spatial attributes
#' 
#' @aliases
#' spatialData spatialData<- 
#' spatialDataNames spatialDataNames<- 
#' spatialCoords spatialCoords<- 
#' spatialCoordsNames spatialCoordsNames<- 
#' imgData imgData<- 
#' scaleFactors
#' 
#' @description
#' The \code{\link{SpatialExperiment}} class provides a family of methods to get
#' and set spatial data attributes in \code{\link{SpatialExperiment}} objects.
#' Spatial attributes include \code{spatialCoords}, \code{imgData}, and
#' \code{scaleFactors}.
#' 
#' @param x A \code{\link{SpatialExperiment}} object.
#' @param value Replacement value for replacement methods.
#' @param sample_id Logical value or character vector specifying sample
#'   identifier(s) for \code{scaleFactors}. Default = \code{TRUE} (all samples).
#' @param image_id Logical value or character vector specifying image
#'   identifier(s) for \code{scaleFactors}. Default = \code{TRUE} (all images).
#' @param withDimnames Logical value indicating whether dimnames of the
#'   \code{spatialExperiment} should be applied or checked against.
#'   If \code{withDimnames=TRUE}, non-\code{NULL} \code{rownames(value)} 
#'   are checked against \code{colnames(x)}, and an error occurs if these
#'   don't match. Else, discrepancies in rownames are ignored.
#'   (see also \code{\link[SingleCellExperiment]{reducedDims}})
#' @param name The name of the \code{colData} column to extract.
#' 
#' @details
#' Additional details for each type of data attribute are provided below.
#' 
#' Note: \code{\link{spatialData}} and \code{\link{spatialDataNames}}
#' (previously used to store a subset of columns from \code{\link{colData}})
#' have been deprecated. All columns should be stored in either
#' \code{\link{spatialCoords}} (numeric matrix containing spatial coordinates)
#' or \code{\link{colData}} (all other columns). The
#' \code{spatialData}/\code{spatialDataNames} functionality has been retained
#' for backward compatibility but may be removed in the future.
#' 
#' @section spatialData and spatialCoords methods:
#' \describe{
#' \item{\code{spatialData(x) <- value}: }{
#'   The \code{spatialData} setter expects a \code{DataFrame}. 
#'   If the input does not contain an \code{in_tissue} column, 
#'   this will be included with a default value of \code{1}.}
#' \item{\code{spatialCoords(x)}: }{
#'   Getter for numeric matrix of spatial coordinates.}
#' \item{\code{spatialCoords(x) <- value}: }{
#'   Setter for numeric matrix of spatial coordinates.}
#' }
#' 
#' @section spatialDataNames and spatialCoordsNames methods:
#' \describe{
#' \item{\code{spatialDataNames(x)}: }{
#'   Returns the names of the \code{colData} associated with the 
#'   spatial information, which are stored in the \code{int_metadata}.}
#' \item{\code{spatialDataNames(x) <- value}: }{
#'   Setter to replace column names
#'   in the \code{spatialData} \code{DataFrame}.}
#' \item{\code{spatialCoordsNames(x)}: }{
#'   Returns the defined names of the
#'   spatial coordinates (e.g. \code{c("x", "y")}).}
#' \item{\code{spatialCoordsNames(x) <- value}: }{
#'   Setter to define the names of the spatial coordinate columns.}
#' }
#' 
#' @section imgData methods:
#' \describe{
#' \item{\code{imgData(x)}: }{
#'   Getter to return the \code{imgData} \code{DataFrame}.}
#' \item{\code{imgData(x) <- value}: }{
#'   Setter to provide a \code{DataFrame} object as 
#'   \code{imgData} of the \code{SpatialExperiment} object.}
#' }
#' 
#' @section Other methods:
#' \describe{
#' \item{\code{scaleFactors(x, sample_id, image_id)}: }{
#'   Getter to return the scale factors associated with the 
#'   \code{sample_id}(s) and \code{image_id}(s) provided. 
#'   This is related to the stored image(s) in the \code{SpatialExperiment} 
#'   \code{imgData} structure. See argument descriptions for further details.}
#' }
#' 
#' @return Return value varies depending on method, as described below.
#' 
#' @examples
#' example(read10xVisium)
#' 
#' # spatialCoords returns a numeric matrix
#' head(spatialCoords(spe))
#' 
#' # change spatial coordinate names
#' spatialCoordsNames(spe)
#' spatialCoordsNames(spe) <- c("x", "y")
#' head(spatialCoords(spe))
#' 
#' # imgData and scale factors
#' imgData(spe)
#' scaleFactors(spe)
#' 
#' # tabulate number of spots mapped to tissue
#' cd <- colData(spe)
#' table(
#'   in_tissue = cd$in_tissue, 
#'   sample_id = cd$sample_id)
NULL

# spatialData ------------------------------------------------------------------

#' @rdname SpatialExperiment-methods
#' @importFrom SummarizedExperiment colData
#' @export
setMethod("spatialData", 
    "SpatialExperiment",
    function(x) {
        colData(x)[spatialDataNames(x)]
    }
)

#' @rdname SpatialExperiment-methods
#' @importFrom SummarizedExperiment colData colData<-
#' @export
setReplaceMethod("spatialData", 
    c("SpatialExperiment", "DFrame"),
    function(x, value) {
        stopifnot(nrow(value) == ncol(x))
        out <- colData(x)
        old <- names(out)
        new <- names(value)
        dup <- intersect(old, new)
        if (length(dup) > 0) {
            out[dup] <- value
            value <- value[setdiff(new, dup)]
        }
        out <- cbind(out, value)
        colData(x) <- out
        spatialDataNames(x) <- new
        return(x)
    }
)

#' @rdname SpatialExperiment-methods
#' @importFrom S4Vectors make_zero_col_DFrame
#' @export
setReplaceMethod("spatialData", 
    c("SpatialExperiment", "NULL"),
    function(x, value) {
        `spatialDataNames<-`(x, value)
    }
)

# spatialDataNames -------------------------------------------------------------

#' @rdname SpatialExperiment-methods
#' @importFrom S4Vectors metadata
#' @export
setMethod("spatialDataNames", 
    "SpatialExperiment", 
    function(x) {
        .msg_spatialData()
        int_metadata(x)$spatialDataNames
    }
)

#' @rdname SpatialExperiment-methods
#' @importFrom SummarizedExperiment colData
#' @importFrom SingleCellExperiment int_metadata<-
#' @export
setReplaceMethod("spatialDataNames", 
    c("SpatialExperiment", "character"),
    function(x, value) {
        stopifnot(value %in% names(colData(x)))
        int_metadata(x)$spatialDataNames <- value
        return(x)
    }
)

#' @rdname SpatialExperiment-methods
#' @export
setReplaceMethod("spatialDataNames",
    c("SpatialExperiment", "NULL"),
    function(x, value) {
        value <- character()
        `spatialDataNames<-`(x, value)
    }
)

# scaleFactors -----------------------------------------------------------------

#' @rdname SpatialExperiment-methods
#' @export
setMethod("scaleFactors", 
    "SpatialExperiment",
    function(x, sample_id=TRUE, image_id=TRUE) {
        stopifnot(!is.null(imgData(x)))
        idx <- .get_img_idx(x, sample_id, image_id)
        imgData(x)$scaleFactor[idx]
    }
)

# utils ------------------------------------------------------------------------

# message for deprecation of spatialData/Names
.msg_spatialData <- function() {
    message(paste0(
        "Note: spatialData and spatialDataNames have been deprecated; all ", 
        "columns should be stored in colData and spatialCoords"))
}

#' @export
#' @importFrom utils .DollarNames
.DollarNames.SpatialExperiment <- function(x, pattern = "")
    grep(pattern, names(colData(x)), value = TRUE)

#' @rdname SpatialExperiment-methods
#' @aliases $,SpatialExperiment-method
#' @exportMethod $
setMethod("$", "SpatialExperiment", function(x, name) {
    colData(x)[[name]]
})
