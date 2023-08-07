#' @name SpatialExperiment-methods
#' 
#' @title Methods for spatial attributes
#' 
#' @aliases
#' spatialData spatialData<-
#' spatialDataNames spatialDataNames<-
#' spatialCoords spatialCoords<-
#' rotateCoords
#' mirrorCoords
#' rotateObject
#' mirrorObject
#' spatialCoordsNames spatialCoordsNames<-
#' imgData imgData<-
#' scaleFactors
#' 
#' @description
#' The \code{\link{SpatialExperiment}} class provides a family of methods to get
#' and set spatial data attributes in \code{\link{SpatialExperiment}} objects.
#' Spatial attributes include \code{spatialCoords}, \code{imgData}, and
#' \code{scaleFactors}. It also provides methods to rotate and mirror
#' \code{\link{SpatialExperiment}} objects and their \code{spatialCoords}.
#' 
#' @param x A \code{\link{SpatialExperiment}} object.
#' @param value Replacement value for replacement methods.
#' @param sample_id Logical value or character vector specifying sample
#'   identifier(s) for \code{scaleFactors}. Default = \code{TRUE} (all samples).
#' @param image_id Logical value or character vector specifying image
#'   identifier(s) for \code{scaleFactors}. Default = \code{TRUE} (all images).
#' @param degrees single numeric 
#'   in +/-[0,90,...,360] specifying how many degrees to rotate.
#'   A negative/positive value corresponds to counter-/clockwise rotation.
#'   Applicable for \code{rotateCoords} and \code{rotateObject} methods.
#' @param axis character string specifying whether to mirror 
#'   horizontally (\code{"h"}) or vertically (\code{"v"}). Applicable for
#'   \code{mirrorCoords} and \code{mirrorObject} methods.
#' @inheritParams imgData-methods
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
#' @section spatialCoords transformation methods:
#' \describe{
#' \item{\code{rotateCoords(x, sample_id, degrees)}: }{
#'   Apply a rotation to the \code{spatialCoords} of \code{x}, potentially
#'   subsetted to sample \code{sample_id} (or without subsetting if
#'   \code{sample_id} is \code{NULL}), by the specified number of \code{degrees}
#'   clockwise.}
#' \item{\code{mirrorCoords(x, sample_id, axis)}: }{
#'   Reflect the \code{spatialCoords} of \code{x} across either the horizontal
#'   or vertical axis, specified by supplying "h" or "v" to the \code{axis}
#'   argument, respectively. Subset \code{x} to just the sample
#'   \code{sample_id}, if not \code{NULL}.}
#' }
#'
#' @section SpatialExperiment transformation wrapper methods:
#' \describe{
#' \item{\code{rotateObject(x, sample_id, image_id, degrees)}: }{
#'   Apply a rotation to the \code{\link{spatialCoords}} and
#'   \code{\link{imgData}} of
#'   \code{x}, potentially subsetted to sample \code{sample_id} (or without
#'   subsetting if \code{sample_id} is \code{NULL}), by the specified number of
#'   \code{degrees} clockwise. Wrapper around \code{rotateCoords} and
#'   \code{rotateImg}.}
#' \item{\code{mirrorObject(x, sample_id, image_id, axis)}: }{
#'   Reflect the \code{\link{spatialCoords}} and \code{\link{imgData}} of \code{x} across
#'   either the horizontal or vertical axis, specified by supplying "h" or "v"
#'   to the \code{axis} argument, respectively. Subset \code{x} to just the
#'   sample \code{sample_id}, if not \code{NULL}. Wrapper around
#'   \code{mirrorCoords} and \code{mirrorImg}.}
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
#' 
#' # rotateCoords(), mirrorCoords(), rotateObject(), and mirrorObject() return a
#' # SpatialExperiment, potentially subsetted by sample.
#'
#' # Subset to just "section1"; rotate coordinates 90 degrees clockwise followed
#' # by a reflection across the vertical axis
#' spe_coords <- rotateCoords(spe, sample_id = "section1", degrees = 90)
#' spe_coords <- mirrorCoords(spe_coords, axis = "v")
#'
#' # Subset to just "section2"; transform both the imgData() and spatialCoords()
#' # by a 180-degree rotation then reflection across the vertical axis
#' spe_wrapper <- rotateObject(spe, sample_id = "section2", degrees = 180)
#' spe_wrapper <- mirrorObject(spe_wrapper, axis = "v")
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

# spatialCoords ----------------------------------------------------------------

#' @rdname SpatialExperiment-methods
#' @importFrom SingleCellExperiment int_colData<-
#' @export
setMethod("spatialCoords", 
    "SpatialExperiment",
    function(x) int_colData(x)$spatialCoords)

#' @rdname SpatialExperiment-methods
setMethod(
    "rotateCoords",
    "SpatialExperiment",
    function(x, sample_id = NULL, degrees = 90) {
        # 'degrees' should be a single numeric divisible by 90
        stopifnot(
            length(degrees) == 1,
            is.numeric(degrees),
            degrees %% 90 == 0
        )
        
        #   Subset by 'sample_id', if supplied
        if (is.null(sample_id)) {
            #   Ensure that we only have one scaleFactor across the whole
            #   SpatialExperiment
            if (length(unique(scaleFactors(x))) != 1) {
                stop(
                    paste0(
                        "Rotations of spatialCoords of multiple samples with ",
                        "different 'scaleFactors' is not currently supported."
                    )
                )
            }
        } else {
            #   Ensure the sample is present in the object
            if (!(sample_id %in% colData(x)$sample_id)) {
                stop(
                    paste0(
                        "'", sample_id, "' is not a sample in",
                        "'colData(x)$sample_id': cannot subset."
                    )
                )
            }
            
            #   Subset
            x <- x[, colData(x)$sample_id == sample_id]
        }
        
        #   Convert degrees to radians. Note that positive angles represent
        #   counter-clockwise rotations
        radians <- degrees * pi / 180
        
        #   Determine the matrix by which left-multiplication represents
        #   rotation. Then apply rotation about the origin
        rotation_mat <- matrix(
            c(
                cos(radians), sin(radians), -1 * sin(radians), cos(radians)
            ),
            nrow = 2
        )
        new_coords <- rotation_mat %*% t(spatialCoords(x))
        
        #   Get the dimensions of the "rectangle" containing the set of
        #   spatialCoords within the object
        dim_max <- dim(imgRaster(x)) / scaleFactors(x)[1]
        
        #   Return the "rectangle" such that its top-left corner is at the spot
        #   where its previous top-left corner was
        if (degrees %% 360 == 90) {
            new_coords <- new_coords + c(dim_max[1], 0)
        } else if (degrees %% 360 == 180) {
            new_coords <- new_coords + rev(dim_max)
        } else if (degrees %% 360 == 270) {
            new_coords <- new_coords + c(0, dim_max[2])
        }
        
        #   Ensure we have integer values for coordinates, and the correct
        #   dimnames
        new_coords <- matrix(
            as.integer(round(t(new_coords))),
            ncol = 2,
            dimnames = dimnames(spatialCoords(x))
        )
        
        #   Return a copy of the SpatialExperiment with the new coordinates
        spatialCoords(x) <- new_coords
        return(x)
    }
)

#' @rdname SpatialExperiment-methods
setMethod(
    "mirrorCoords",
    "SpatialExperiment",
    function(x, sample_id = NULL, axis = c("h", "v")) {
        #   Subset by 'sample_id', if supplied
        if (!is.null(sample_id)) {
            #   Ensure the sample is present in the object
            if (!(sample_id %in% colData(x)$sample_id)) {
                stop(
                    paste0(
                        "'", sample_id, "' is not a sample in",
                        "'colData(x)$sample_id': cannot subset."
                    )
                )
            }
            
            #   Subset
            x <- x[, colData(x)$sample_id == sample_id]
        }
        
        #   Parse 'axis' into a numeric vector, since the mirror operation will
        #   be performed by multiplication
        if (axis == "h") {
            refl_vec <- c(1, -1)
        } else if (axis == "v") {
            refl_vec <- c(-1, 1)
        } else if (is.null(axis)) {
            refl_vec <- c(1, 1)
        } else {
            stop(paste("Invalid 'axis' argument:", axis))
        }
        
        #   Perform mirror
        new_coords <- refl_vec * t(spatialCoords(x))
        
        #   Return "rectangle" to its original location depending on the axis
        if (axis == "v") {
            new_coords <- new_coords + c(dim_max[2], 0)
        } else if (axis == "h") {
            new_coords <- new_coords + c(0, dim_max[1])
        }
        
        new_coords <- t(new_coords)
        
        #   Add names to spatialCoords of the new object
        colnames(new_coords) <- colnames(spatialCoords(x))
        
        #   Return a copy of the SpatialExperiment with the new coordinates
        spatialCoords(x) <- round(new_coords)
        return(x)
    }
)

#' @rdname SpatialExperiment-methods
#' @export
setMethod(
    "rotateObject",
    "SpatialExperiment",
    function(x, sample_id = NULL, image_id = NULL, degrees = 90) {
        #   imgData
        x <- rotateImg(x, sample_id, image_id, degrees)
        
        #   spatialCoords
        x <- rotateCoords(x, sample_id, degrees)
        
        return(x)
    }
)

#' @rdname SpatialExperiment-methods
#' @export
setMethod(
    "mirrorObject",
    "SpatialExperiment",
    function(x, sample_id = NULL, image_id = NULL, axis = c("h", "v")) {
        #   imgData
        x <- mirrorImg(x, sample_id, image_id, axis)
        
        #   spatialCoords
        x <- mirrorCoords(x, sample_id, axis)
        
        return(x)
    }
)

#' @rdname SpatialExperiment-methods
#' @importFrom SingleCellExperiment int_colData<-
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
#' @importFrom SingleCellExperiment int_colData<-
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
