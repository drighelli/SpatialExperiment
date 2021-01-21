#' @name SpatialExperiment-colData
#' @rdname SpatialExperiment-colData
#' @title SpatialExperiment colData
#' @aliases colData colData<-  
#' @description 
#' The colData setter expects a DataFrame with a \code{sample_id} column reflecting the
#' already existing \code{sample_id}(s) present in the object.
#' An additional check is made on the \code{imgData} data structure.
#' In case a \code{NULL} value is passed, the \code{colData} are dropped.
#' 
#' @param x a \code{\link{SpatialExperiment}}
#' @param value a \code{\link[S4Vectors]{DataFrame}}
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
    function(x, value) 
    {
         # store original 'colData'
         old <- colData(x)
         
         # do the replacement
         se <- as(x, "SummarizedExperiment")
         colData(se) <- value
         new <- colData(se)
         
         # check that 'sample_id's remain valid & update 'imgData' accordingly
         ns_old <- length(sids_old <- unique(old$sample_id))
         ns_new <- length(sids_new <- unique(new$sample_id))
         if (ns_old != ns_new) 
         {
            warning(sprintf(
                "Number of unique 'sample_id's is %s, but %s %s provided.\n",
                ns_old, ns_new, ifelse(ns_new > 1, "were", "was")))
         } else if (sum(table(old$sample_id, new$sample_id) != 0) != ns_old) {
            warning("New 'sample_id's must map uniquely")
         } else if (!is.null(imgData(x))) 
         {
            m <- match(imgData(x)$sample_id, sids_old)
            imgData(x)$sample_id <- sids_new[m]
         }
         BiocGenerics:::replaceSlots(x, colData=value, check=FALSE)
    }
)

#' @rdname SpatialExperiment-colData
#' @importFrom S4Vectors DataFrame
#' @importFrom SummarizedExperiment colData colData<-
#' @export
setReplaceMethod("colData",
    c("SpatialExperiment", "NULL"),
    function(x, value) 
    {
        warning("Dropping colData could break imgData and spatialData functionality.")
        BiocGenerics:::replaceSlots(x, colData=value, check=FALSE)
    }
)
