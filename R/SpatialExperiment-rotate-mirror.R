#' @name SpatialExperiment-rotate-mirror
#' 
#' @inherit SpatialExperiment-methods title
#' 
#' @aliases
#' rotateCoords
#' mirrorCoords
#' rotateObject
#' mirrorObject
#' 
#' @description
#' The \code{\link{SpatialExperiment}} class provides methods to rotate and
#' mirror \code{\link{SpatialExperiment}} objects and their
#' \code{spatialCoords}.
#' 
#' @param degrees single numeric 
#'   in +/-[0,90,...,360] specifying how many degrees to rotate.
#'   A negative/positive value corresponds to counter-/clockwise rotation.
#'   Applicable for \code{rotateCoords} and \code{rotateObject} methods.
#' @param axis character string specifying whether to mirror 
#'   horizontally (\code{"h"}) or vertically (\code{"v"}). Applicable for
#'   \code{mirrorCoords} and \code{mirrorObject} methods.
#' @param warn Logical value indicating whether to print a warning about
#'   mismatches between coordinates and images, possible with the spatialCoords
#'   transformation methods \code{rotateCoords} and \code{mirrorCoords}.
#' @inheritParams SpatialExperiment-methods
#' 
#' @details
#' Additional details for each type of data attribute are provided below.
#' 
#' @section spatialCoords transformation methods:
#' \describe{
#' \item{\code{rotateCoords(x, sample_id, degrees, warn)}: }{
#'   Apply a rotation to the \code{spatialCoords} of \code{x}, potentially
#'   subsetted to sample \code{sample_id} (or without subsetting if
#'   \code{sample_id} is \code{NULL}), by the specified number of \code{degrees}
#'   clockwise. Warn about mismatches with images if \code{warn}.}
#' \item{\code{mirrorCoords(x, sample_id, axis, warn)}: }{
#'   Reflect the \code{spatialCoords} of \code{x} across either the horizontal
#'   or vertical axis, specified by supplying "h" or "v" to the \code{axis}
#'   argument, respectively. Subset \code{x} to just the sample
#'   \code{sample_id}, if not \code{NULL}. Warn about mismatches with images if
#'   \code{warn}.}
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
#' @inherit SpatialExperiment-methods return
#' 
#' @author Nicholas J. Eagles
#' 
#' @examples
#' example(read10xVisium)
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

# rotate/mirrorCoords ----------------------------------------------------------

#' @rdname SpatialExperiment-rotate-mirror
#' @export
setMethod(
    "rotateCoords",
    "SpatialExperiment",
    function(x, sample_id = NULL, degrees = 90, warn = TRUE) {
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
        
        if (warn) {
            warning(
                paste0(
                    "Invoking 'rotateCoords' or 'rotateImg' independently may ",
                    "result in mismatches between coordinates and images. ",
                    "Please use 'rotateObject' to ensure consistency."
                )
            )
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

#' @rdname SpatialExperiment-rotate-mirror
#' @export
setMethod(
    "mirrorCoords",
    "SpatialExperiment",
    function(x, sample_id = NULL, axis = c("h", "v"), warn = TRUE) {
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

        if (warn) {
            warning(
                paste0(
                    "Invoking 'mirrorCoords' or 'mirrorImg' independently may ",
                    "result in mismatches between coordinates and images. ",
                    "Please use 'mirrorObject' to ensure consistency."
                )
            )
        }
        
        #   Get the dimensions of the "rectangle" containing the set of
        #   spatialCoords within the object
        dim_max <- dim(imgRaster(x)) / scaleFactors(x)[1]

        #   Perform mirror
        new_coords <- refl_vec * t(spatialCoords(x))
        
        #   Return "rectangle" to its original location depending on the axis
        if (axis == "v") {
            new_coords <- new_coords + c(dim_max[2], 0)
        } else if (axis == "h") {
            new_coords <- new_coords + c(0, dim_max[1])
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

# rotate/mirrorObject ----------------------------------------------------------

#' @rdname SpatialExperiment-rotate-mirror
#' @export
setMethod(
    "rotateObject",
    "SpatialExperiment",
    function(x, sample_id = NULL, image_id = NULL, degrees = 90) {
        #   imgData
        x <- rotateImg(x, sample_id, image_id, degrees)
        
        #   spatialCoords
        x <- rotateCoords(x, sample_id, degrees, FALSE)
        
        return(x)
    }
)

#' @rdname SpatialExperiment-rotate-mirror
#' @export
setMethod(
    "mirrorObject",
    "SpatialExperiment",
    function(x, sample_id = NULL, image_id = NULL, axis = c("h", "v")) {
        #   imgData
        x <- mirrorImg(x, sample_id, image_id, axis)
        
        #   spatialCoords
        x <- mirrorCoords(x, sample_id, axis, FALSE)
        
        return(x)
    }
)
