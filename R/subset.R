# we overwrite the default subsetting method
# to assure that the 'imgData' and 'spatialData' are subsetted if columns/rows 
# are dropped
#' @title SpatialExperiment subset
#' @name SpatialExperiment-subset
#' @description The subset method ensures that all the subsetted rows/cols 
#' are updated across the colData and spatialData objects
#' @section subset:
#' \describe{
#' \item{\code{[}:}{ subsetting method}
#' }
#' @aliases [,SpatialExperiment,ANY,ANY,ANY-method
#' @param x a SpatialExperiment object
#' @param i the row indexes to subset
#' @param j the col indexes to subset
#' 
#' @return a SpatialExperiment class object
#' @examples 
#' example(SpatialExperiment)
#' idx <- sample(ncol(spe), 10)
#' sub <- spe[, idx]
#' dim(sub)
#' spatialData(sub, 
#'   spatialCoords = TRUE, 
#'   colData = TRUE)
NULL

#' @importFrom methods callNextMethod
#' @export
setMethod("[",
    c("SpatialExperiment", "ANY", "ANY"),
    function(x, i, j, ..., drop=FALSE) {
        if (missing(i)) i <- TRUE
        if (missing(j)) j <- TRUE
        x <- callNextMethod()
        if ( !isEmpty(imgData(x)) ) {
            keep <- imgData(x)$sample_id %in% unique(x$sample_id)
            imgData(x) <- imgData(x)[keep, ]
        }
        return(x)
    }
)


