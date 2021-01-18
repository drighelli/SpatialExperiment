# we overwrite the default subsetting method
# to assure that the 'imgData' and 'spatialData' are subsetted if samples/rows 
# are dropped
#' @importFrom methods callNextMethod
#' @export
setMethod("[", 
    c("SpatialExperiment", "ANY", "ANY"), 
    function(x, i, j) 
    {
        if (missing(i)) i <- TRUE
        if (missing(j)) j <- TRUE
        x@spatialData <- x@spatialData[i,,drop=FALSE]
        x <- callNextMethod()
        keep <- imgData(x)$sample_id %in% unique(x$sample_id)
        imgData(x) <- imgData(x)[keep, ]
        return(x)
    }
)


