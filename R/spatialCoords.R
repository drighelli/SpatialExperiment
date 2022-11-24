
# spatialCoords ----------------------------------------------------------------

#' @rdname SpatialExperiment-methods
#' @importFrom SingleCellExperiment int_colData<-
#' @export
setMethod("spatialCoords", 
    "SpatialExperiment",
    function(x, withDimnames=TRUE, ...) {
        out <- int_colData(x)$spatialCoords
        if (withDimnames) 
            rownames(out) <- colnames(x)
        return(out)
    })

#' @rdname SpatialExperiment-methods
#' @importFrom SingleCellExperiment int_colData<-
#' @export
setReplaceMethod("spatialCoords", 
    c("SpatialExperiment", "matrix"),
    function(x, withDimnames=TRUE, ..., value) {
        stopifnot(
            is.numeric(value),
            nrow(value) == ncol(x))
        new <- rownames(value)
        if (!is.null(new) && withDimnames) {
            if (!identical(new, colnames(x))) {
                stop("Non-NULL 'rownames(value)' should be the",
                    " same as 'colnames(x)' for 'spatialCoords<-'.",
                    " Use 'withDimnames=FALSE' to force replacement.")
            }
        }
        int_colData(x)$spatialCoords <- value
        return(x)
    }
)

#' @rdname SpatialExperiment-methods
#' @export
setReplaceMethod("spatialCoords", 
    c("SpatialExperiment", "NULL"),
    function(x, withDimnames=TRUE, ..., value) {
        `spatialCoords<-`(x, 
            withDimnames=withDimnames, ..., 
            value=matrix(numeric(), ncol(x), 0))
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
