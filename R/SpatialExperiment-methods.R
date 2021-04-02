#' @title SpatialExperiment Methods
#' @name SpatialExperiment-methods
#' @rdname SpatialExperiment-methods
#' 
#' @aliases 
#' spatialData spatialData<- 
#' spatialDataNames spatialDataNames<-
#' spatialCoords spatialCoords<- 
#' spatialCoordsNames spatialCoordsNames<-
#' imgData imgData<- scaleFactors
#' 
#' @description 
#' The SpatialExperiment class provides a family of getter/setter methods 
#' to get/replace the spatial data attributes.
#' It is always suggested to use these accessors to facilitate future maintenance
#' of the code.
#' We distinguish the spatial data from the spatial coordinates as described in 
#' this paragraph.
#' With spatial data we refer to the general data structure containing the 
#' spatial coordinates, while we refer for spatial coordinates only to the 
#' specific spatial coordinates columns (as x, y, z, ...).
#' 
#' @param x a SpatialExperiment class object
#' @param value it varies depending on the invoked method see methods details.
#' @param sample_id character string specifying sample 
#' identifier(s); \code{TRUE} is equivalent to all samples.
#' @param image_id character string indicating the image identifiers(s), 
#' \code{TRUE} is equivalent to all images.
#' @param spatialCoords logical specifying whether to include 
#'   \code{spatialCoords} in the output \code{DataFrame}.
#' @param colData logical or character vector specifying whether or 
#'   which \code{colData} columns to include in the output \code{DataFrame}.
#' 
#' @section spatialData Methods:
#' All the methods described in this section takes as input the \code{sample_id}
#' argument.
#' See the parameter description for additional information.
#' \describe{
#' \item{\code{spatialData(x) <- value}:}{ The spatialData setter expects a 
#' data.frame/DataFrame object named with the
#' previously defined colnames for the spatial coordinates.
#' It is possible to get/set the spatial coordinates names with the 
#' spatialCoordsNames getter/setter, by default they are set by the 
#' SpatialExperiment constructor to c("x", "y").
#' If the input spatial data structure doesn't contain an in_tissue column, 
#' a default in_tissue column is set to 1.
#' If NULL is passed as value the all the related data structures are set 
#' accordingly.}
#' \item{\code{spatialData(x)}:}{ The spatialData getter gives the possibility 
#' to retrieve also additional columns from the
#' colData structure by the aid of the \code{cd_bind} argument.
#' It returns by default a DataFrame, but it is possible to retrieve a 
#' matrix by setting \code{as_df=FALSE}.}
#' \item{\code{spatialCoords(x)}:}{ The \code{spatialCoords} getter returns the 
#' spatial coordinates in a matrix-like form, but it is possible to retrieve 
#' them in a DataFrame form by setting \code{as_df=TRUE}.}
#' }
#'
#' @section spatialNames Methods:
#' \describe{
#' \item{\code{spatialDataNames(x)}:}{ Returns the names of the spatialData 
#' structure.}
#' \item{\code{spatialCoordsNames(x)<-value}:}{ Sets the names of the spatial 
#' coordinates accordingly with the assigned value.}
#' \item{\code{spatialCoordsNames(x)}:}{ Returns the stored names of the spatial 
#' coordinates.}
#' }
#' 
#' @section imgData Methods:
#' \describe{
#' \item{\code{imgData(x)<-value}:}{ This setter allows to set a DataFrame 
#' object as imgData of the SpatialExperiment object class.}
#' \item{\code{imgData(x)}:}{ This getter returns the imgData DataFrame.}
#' }
#' 
#' @section Other Methods:
#' \describe{
#' \item{\code{scaleFactors(x, sample_id, image_id)}:}{ This getter returns 
#' the scale factors associated to the \code{sample_id}(s) 
#' and \code{image_id}(s) passed as input.
#' This is related to the stored image(s) into the SpatialExperiment.
#' \code{imgData} structure.
#' See the \code{sample_id} and \code{image_id} section description for further 
#' details.}
#' }
#' 
#' @return see methods details.
#' 
#' @examples
#' example(SpatialExperiment)
#' ## The spatialData methods
#' fakeSpData <- spatialData(spe)
#' fakeSpData$array_col <- fakeSpData$array_row
#' spatialData(spe) <- fakeSpData
#' 
#' # spatialData returns a DataFrame
#' head(spatialData(spe))
#' 
#' # spatialCoords returns a matrix of coordinates
#' head(spatialCoords(spe))
#' 
#' # combine spatialData with spatialCoords & colData of interest
#' head(spatialData(spe, spatialCoords=TRUE, colData="sample_id"))
#' 
#' # changing spatial coordinates names
#' spatialCoordsNames(spe)
#' spatialCoordsNames(spe) <- c("x","y")
#' spatialCoordsNames(spe)
#' head(spatialCoords(spe))
#' 
#' # imgData
#' imgData(spe)
#' scaleFactors(spe)
#' 
#' # tabulate number of spots mapped to tissue
#' table(
#'   in_tissue = spe$in_tissue, 
#'   sample_id = spe$sample_id)
#' 
#' @importFrom SingleCellExperiment int_colData int_colData<-
#' @importFrom S4Vectors nrow SimpleList isEmpty DataFrame
#' @importFrom methods is
NULL

# spatialData ------------------------------------------------------------------

#' @rdname SpatialExperiment-methods
#' @export
setMethod("spatialData", 
    "SpatialExperiment",
    function(x, spatialCoords = FALSE, colData = FALSE) {
        stopifnot(
            is.logical(spatialCoords), 
            length(spatialCoords) == 1)
        
        nms <- spatialDataNames(x)
        
        if (is.character(colData)) {
            stopifnot(colData %in% names(colData(x)))
            nms <- c(nms, colData)
        } else {
            stopifnot(is.logical(colData), length(colData) == 1)
            if (colData) {
                nms <- names(colData(x))
            }
        }
        spd <- colData(x)[nms]
        if (spatialCoords) {
            spd <- cbind(spd, spatialCoords(x))
        }
        return(spd)
    }
)

#' @rdname SpatialExperiment-methods
#' @export
setReplaceMethod("spatialData", 
    c("SpatialExperiment", "DFrame"),
    function(x, value) {
        stopifnot(nrow(value) == ncol(x))
        
        new <- names(value)
        old <- spatialDataNames(x)
        
        spatialDataNames(x) <- new
        cd_keep <- setdiff(names(colData(x)), old)
        colData(x) <- cbind(colData(x)[cd_keep], value)
 
        return(x)
    }
)

#' @rdname SpatialExperiment-methods
#' @export
setReplaceMethod("spatialData", 
    c("SpatialExperiment", "NULL"),
    function(x, value) {
        # @<- is required here to enforce replacement because
        # spatialDataNames are protected during colData replacement
        i <- spatialDataNames(x)
        x@colData[i] <- NULL
        spatialDataNames(x) <- NULL
        return(x)
    }
)

# spatialDataNames -------------------------------------------------------------

#' @rdname SpatialExperiment-methods
#' @importFrom SingleCellExperiment int_metadata
#' @export
setMethod("spatialDataNames", 
    "SpatialExperiment", 
    function(x) int_metadata(x)$spatialDataNames)

#' @rdname SpatialExperiment-methods
#' @export
setReplaceMethod("spatialDataNames", 
    c("SpatialExperiment", "character"),
    function(x, value) {
        int_metadata(x)$spatialDataNames <- value
        return(x)
    }
)

#' @rdname SpatialExperiment-methods
#' @export
setReplaceMethod("spatialDataNames", 
    c("SpatialExperiment", "NULL"),
    function(x, value) {
        spatialDataNames(x) <- character()
        return(x)
    }
)

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
