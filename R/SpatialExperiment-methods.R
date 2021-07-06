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
#' Spatial attributes include \code{spatialData}, \code{spatialCoords},
#' \code{imgData}, and \code{scaleFactors}.
#' 
#' @param x A \code{\link{SpatialExperiment}} object.
#' @param spatialData Logical specifying whether to include columns from
#'   \code{spatialData} in the output \code{\link{DataFrame}} from
#'   \code{colData}. Default = \code{FALSE}
#' @param spatialCoords Logical specifying whether to include columns from
#'   \code{spatialCoords} in the output \code{\link{DataFrame}} from
#'   \code{spatialData}. Default = \code{FALSE}.
#' @param value Replacement value for replacement methods.
#' @param sample_id Logical value or character vector specifying sample
#'   identifier(s) for \code{scaleFactors}. Default = \code{TRUE} (all samples).
#' @param image_id Logical value or character vector specifying image
#'   identifier(s) for \code{scaleFactors}. Default = \code{TRUE} (all images).
#' 
#' @details
#' Additional details for each type of data attribute are provided below.
#' 
#' \code{\link{spatialData}} and \code{\link{spatialCoords}} are distinguished
#' as follows: \code{spatialData} is a \code{DataFrame} containing all the data
#' associated with the spatial information (optionally including spatial
#' coordinates from \code{spatialCoords}), while \code{spatialCoords} is a
#' numeric matrix containing only the defined spatial coordinates (e.g. columns
#' \code{x} and \code{y}).
#' 
#' @section spatialData and spatialCoords methods:
#' \describe{
#' \item{\code{spatialData(x)}: }{The \code{spatialData} getter provides the 
#' optional argument \code{spatialCoords}, which can be used to include the 
#' columns of spatial coordinates (\code{spatialCoords}) in the output 
#' \code{DataFrame}.}
#' \item{\code{spatialData(x) <- value}: }{The \code{spatialData} setter 
#' expects a \code{DataFrame} with the defined column names for the spatial coordinates. 
#' Spatial coordinate names can be set with the \code{spatialCoordNames} setter, 
#' and are set as \code{c("x", "y")} by default by the 
#' \code{\link{SpatialExperiment}} constructor. If the input does not
#' contain an \code{in_tissue} column, this will be included with a default
#' value of \code{1}.}
#' \item{\code{spatialCoords(x)}: }{Getter for numeric matrix of spatial
#' coordinates.}
#' \item{\code{spatialCoords(x) <- value}: }{Setter for numeric matrix of
#' spatial coordinates.}
#' }
#' 
#' @section spatialDataNames and spatialCoordsNames methods:
#' \describe{
#' \item{\code{spatialDataNames(x)}: }{Returns the column names of the
#' \code{spatialData} \code{DataFrame}.}
#' \item{\code{spatialDataNames(x) <- value}: }{Setter to replace column names
#' in the \code{spatialData} \code{DataFrame}.}
#' \item{\code{spatialCoordsNames(x)}: }{Returns the defined names of the
#' spatial coordinates (e.g. \code{c("x", "y")}).}
#' \item{\code{spatialCoordsNames(x) <- value}: }{Setter to define the names of
#' the spatial coordinate columns.}
#' }
#' 
#' @section imgData methods:
#' \describe{
#' \item{\code{imgData(x)}: }{Getter to return the \code{imgData} \code{DataFrame}.}
#' \item{\code{imgData(x) <- value}: }{Setter to provide a \code{DataFrame}
#' object as \code{imgData} of the \code{SpatialExperiment} object.}
#' }
#' 
#' @section Other methods:
#' \describe{
#' \item{\code{scaleFactors(x, sample_id, image_id)}: }{Getter to return the
#' scale factors associated with the \code{sample_id}(s) and \code{image_id}(s)
#' provided. This is related to the stored image(s) in the
#' \code{SpatialExperiment} \code{imgData} structure. See argument descriptions
#' for further details.}
#' }
#' 
#' @return Return value varies depending on method, as described below.
#' 
#' @examples
#' example(read10xVisium)
#' 
#' # spatialData returns a DataFrame
#' spatialData(spe)
#' 
#' # spatialCoords returns a numeric matrix
#' head(spatialCoords(spe))
#' 
#' # spatialData replacement method
#' spdata <- spatialData(spe)
#' spdata$array_col <- spdata$array_row
#' spatialData(spe) <- spdata
#' 
#' # return additional columns for spatialData
#' spatialData(spe, spatialCoords=TRUE)
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
#' cd <- colData(spe, spatialData = TRUE)
#' table(
#'   in_tissue = cd$in_tissue, 
#'   sample_id = cd$sample_id)
#' 
#' @importFrom SingleCellExperiment int_colData int_colData<-
#' @importFrom S4Vectors nrow SimpleList isEmpty DataFrame
#' @importFrom methods is
NULL

# spatialData ------------------------------------------------------------------

#' @rdname SpatialExperiment-methods
#' @importFrom SingleCellExperiment int_colData
#' @export
setMethod("spatialData", 
    "SpatialExperiment",
    function(x, spatialCoords = FALSE) {
        stopifnot(
            is.logical(spatialCoords), 
            length(spatialCoords) == 1)
        out <- int_colData(x)$spatialData
        if (spatialCoords) {
            out <- cbind(out, spatialCoords(x))
        }
        return(out)
    }
)

#' @rdname SpatialExperiment-methods
#' @export
setReplaceMethod("spatialData", 
    c("SpatialExperiment", "DFrame"),
    function(x, value) {
        stopifnot(nrow(value) == ncol(x))
        int_colData(x)$spatialData <- value
        return(x)
    }
)

#' @rdname SpatialExperiment-methods
#' @importFrom S4Vectors make_zero_col_DFrame
#' @export
setReplaceMethod("spatialData", 
    c("SpatialExperiment", "NULL"),
    function(x, value) {
        value <- make_zero_col_DFrame(ncol(x))
        rownames(value) <- rownames(spatialData(x))
        `spatialData<-`(x, value)
    }
)

# spatialDataNames -------------------------------------------------------------

#' @rdname SpatialExperiment-methods
#' @importFrom SingleCellExperiment int_metadata
#' @export
setMethod("spatialDataNames", 
    "SpatialExperiment", 
    function(x) names(spatialData(x)))

#' @rdname SpatialExperiment-methods
#' @export
setReplaceMethod("spatialDataNames", 
    c("SpatialExperiment", "character"),
    function(x, value) {
        names(int_colData(x)$spatialData) <- value
        return(x)
    }
)

#' #' @rdname SpatialExperiment-methods
#' #' @export
#' setReplaceMethod("spatialDataNames", 
#'     c("SpatialExperiment", "NULL"),
#'     function(x, value) {
#'         spatialDataNames(x) <- character()
#'         return(x)
#'     }
#' )

# spatialCoords ----------------------------------------------------------------

#' @rdname SpatialExperiment-methods
#' @export
setMethod("spatialCoords", 
    "SpatialExperiment",
    function(x) int_colData(x)$spatialCoords)

#' @rdname SpatialExperiment-methods
#' @export
setReplaceMethod("spatialCoords", 
    c("SpatialExperiment", "matrix"),
    function(x, value) {
        stopifnot(
            is.numeric(value),
            nrow(value) == ncol(x))
        int_colData(x)$spatialCoords <- value
        return(x)
    }
)

#' @rdname SpatialExperiment-methods
#' @export
setReplaceMethod("spatialCoords", 
    c("SpatialExperiment", "NULL"),
    function(x, value) {
        value <- matrix(numeric(), ncol(x), 0)
        `spatialCoords<-`(x, value)
    }
)

# spatialCoordsNames -----------------------------------------------------------

#' @rdname SpatialExperiment-methods
#' @importFrom SingleCellExperiment int_colData
#' @export
setMethod("spatialCoordsNames", 
    "SpatialExperiment", 
    function(x) colnames(int_colData(x)$spatialCoords))

#' @rdname SpatialExperiment-methods
#' @export
setReplaceMethod("spatialCoordsNames", 
    c("SpatialExperiment", "character"), 
    function(x, value) {
        colnames(int_colData(x)$spatialCoords) <- value
        return(x)
    }
)

#' @rdname SpatialExperiment-methods
#' @export
setReplaceMethod("spatialCoordsNames", 
    c("SpatialExperiment", "NULL"), 
    function(x, value) {
        value <- character()
        `spatialCoordsNames<-`(x, value)
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
