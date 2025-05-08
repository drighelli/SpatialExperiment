#' @name SpatialExperiment-subset
#' 
#' @title Subsetting SpatialExperiment objects
#' 
#' @aliases [,SpatialExperiment,ANY,ANY,ANY-method
#' 
#' @description
#' The subsetting method for \code{\link{SpatialExperiment}} objects ensures
#' that spatial data attributes (\code{\link{spatialCoords}} and
#' \code{\link{imgData}}) are subsetted correctly to match rows and columns with
#' the remainder of the object.
#' 
#' @section subset:
#' \describe{
#' \item{\code{[}:}{ subsetting method}
#' }
#' 
#' @param x a \code{\link{SpatialExperiment}} object
#' @param i row indices for subsetting
#' @param j column indices for subsetting
#' 
#' @return a \code{\link{SpatialExperiment}} object
#' 
#' @examples
#' example(SpatialExperiment)
#' 
#' dim(spe)
#' 
#' set.seed(123)
#' idx <- sample(ncol(spe), 10)
#' sub <- spe[, idx]
#' dim(sub)
#' colData(sub)
#' spatialCoords(sub)
NULL

# we overwrite the default subsetting method
# to assure that the 'imgData' are subsetted
# if columns/rows are dropped

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


