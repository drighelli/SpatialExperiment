#' @name SpatialExperiment-colData
#' 
#' @title SpatialExperiment colData
#' 
#' @aliases colData colData<- 
#' 
#' @description
#' The \code{\link{SpatialExperiment}} class provides a modified \code{colData} setter, which ensures 
#' that the \code{SpatialExperiment} object remains valid.
#' 
#' @details
#' The \code{colData} setter expects a \code{\link{DataFrame}} with a
#' \code{sample_id} column reflecting the already existing \code{sample_id}(s)
#' present in the \code{SpatialExperiment} object. An additional check is
#' performed against the \code{\link{imgData}} data structure. If a \code{NULL}
#' value is provided, the \code{colData} is dropped.
#' 
#' @param x a \code{\link{SpatialExperiment}}
#' @param value a \code{\link[S4Vectors]{DataFrame}}
#' 
#' @return a \code{\link{SpatialExperiment}} object with updated \code{colData}
#' 
#' @examples
#' example(read10xVisium)
#' colData(spe) <- NULL
NULL

# the following overwrites 'SummarizedExperiment's 'colData' 
# replacement method to assure the 'SpatialExperiment' remains valid
# - in case of an invalid replacement, we throw a warning 
#   (not an error) and return the (unmodified) input object
# - in case of replacement by NULL

#' @rdname SpatialExperiment-colData
#' @importFrom S4Vectors DataFrame
#' @importFrom SummarizedExperiment colData colData<-
#' @export
setReplaceMethod("colData",
    c("SpatialExperiment", "DataFrame"),
    function(x, value) {
        # store original 'colData'
        old <- colData(x)
        
        # do the replacement
        se <- as(x, "SummarizedExperiment")
        colData(se) <- value
        new <- colData(se)
        
        if (!is.null(new$sample_id)) {
            # check that 'sample_id's remain valid & update 'imgData' accordingly
            ns_old <- length(sids_old <- unique(old$sample_id))
            ns_new <- length(sids_new <- unique(new$sample_id))
            if (ns_old != ns_new) {
                stop(sprintf(
                    "Number of unique 'sample_id's is %s, but %s %s provided.\n",
                    ns_old, ns_new, ifelse(ns_new > 1, "were", "was")))
            } else if (sum(table(old$sample_id, new$sample_id) != 0) != ns_old) {
                stop("New 'sample_id's must map uniquely")
            } else if (!is.null(imgData(x))) {
                m <- match(imgData(x)$sample_id, sids_old)
                imgData(x)$sample_id <- sids_new[m]
            }    
        } else {
            # if none provided, retain original sample_id field
            value$sample_id <- old$sample_id
        }

        # protect spatialData from being replaced
        spd <- spatialData(x, 
            spatialCoords=FALSE, 
            colData=FALSE)
        value <- cbind(value, spd)
        BiocGenerics:::replaceSlots(x, colData=value, check=FALSE)
    }
)

#' @rdname SpatialExperiment-colData
#' @importFrom SummarizedExperiment colData colData<-
#' @export
setReplaceMethod("colData",
    c("SpatialExperiment", "NULL"),
    function(x, value) {
        # replacement by NULL keeps 
        # sample_id & spatialDataNames
        ids <- colData(x)["sample_id"]
        spd <- spatialData(x, 
            spatialCoords=FALSE, 
            colData=FALSE)
        value <- cbind(ids, spd)
        colData(x) <- value
        return(x)
    }
)
