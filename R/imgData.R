#' @rdname SpatialExperiment-methods
#' @importFrom SingleCellExperiment int_metadata
#' @export
setMethod("imgData", "SpatialExperiment", function(x) int_metadata(x)$imgData)

#' @rdname SpatialExperiment-methods
#' @importFrom S4Vectors DataFrame
#' @importFrom SingleCellExperiment int_metadata<-
#' @export
setReplaceMethod("imgData",
    c("SpatialExperiment", "DataFrame"),
    function(x, value) {
        if (!isEmpty(value)) {
            msg <- .imgData_validity(value)
            if (!is.null(msg))
                stop(msg)
        }
        int_metadata(x)$imgData <- value
        return(x)
    })

#' @rdname SpatialExperiment-methods
#' @importFrom S4Vectors DataFrame
#' @export
setReplaceMethod("imgData",
    c("SpatialExperiment", "NULL"),
    function(x, value) {
        value <- DataFrame()
        `imgData<-`(x, value)
    })
