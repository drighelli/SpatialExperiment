# we overwrite the default subsetting method
# to assure that the 'imgData' and 'spatialData' are subsetted if columns/rows 
# are dropped
#' @title SpatialExperiment subset
#' @name SpatialExperiment-subset
#' @description The subset method ensures that all the subsetted rows/cols 
#' are updated across the colData and spatialData objects
#' @section subset:
#' \describe{
#' \itemize{
#' \item{\code{[}:} subsetting method
#' }
#' }
#' @aliases [,SpatialExperiment,ANY,ANY,ANY-method
#' @param x a SpatialExperiment object
#' @param i the row indexes to subset
#' @param j the col indexes to subset
#' @importFrom methods callNextMethod
#' @export
#' @examples 
#' example(SpatialExperiment)
#' se1 <- se[,1:10]
#' colData(se1)
#' spatialData(se1)
setMethod("[",
    c("SpatialExperiment", "ANY", "ANY"),
    function(x, i, j) 
    {
        if (missing(i)) i <- TRUE
        if (missing(j)) j <- TRUE
        x <- callNextMethod()
        x@spatialData <- x@spatialData[j,,drop=FALSE]
        keep <- imgData(x)$sample_id %in% unique(x$sample_id)
        imgData(x) <- imgData(x)[keep, ]
        return(x)
    }
)


