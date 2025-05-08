#' @name SpatialExperiment-colData
#' 
#' @title SpatialExperiment colData
#' 
#' @aliases colData colData<- 
#' 
#' @description
#' The \code{\link{SpatialExperiment}} class provides a modified \code{colData}
#' setter, which ensures that the \code{SpatialExperiment} object remains valid.
#' 
#' @details
#' The \code{colData} setter performs several checks to ensure validity. If the
#' replacement \code{colData} does not contain a \code{sample_id} column, the
#' existing \code{sample_id}s will be retained. If the replacement
#' \code{colData} contains \code{sample_id}s, a check is performed to ensure the
#' number of unique \code{sample_id}s is the same, i.e. a one-to-one mapping is
#' possible. If the replacement is \code{NULL}, the \code{sample_id}s are
#' retained. In addition, checks are performed against the \code{sample_id}s in
#' \code{\link{imgData}}.
#' 
#' @param x a \code{\link{SpatialExperiment}}
#' @param value a \code{\link[S4Vectors]{DataFrame}}
#' 
#' @return a \code{\link{SpatialExperiment}} object with updated \code{colData}
#' 
#' @examples
#' example(SpatialExperiment)
#' 
#' # empty replacement retains sample identifiers
#' colData(spe) <- NULL
#' names(colData(spe))
#' 
#' # replacement of sample identifiers
#' # requires one-to-one mapping
#' 
#' ## invalid replacement
#' 
#' tryCatch(
#'   spe$sample_id <- seq(ncol(spe)),
#'   error = function(e) message(e)) 
#'   
#' ## valid replacement  
#'   
#' old <- c("section1", "section2")
#' new <- c("sample_A", "sample_B")
#' idx <- match(spe$sample_id, old)
#' 
#' tmp <- spe
#' tmp$sample_id <- new[idx]
#' table(spe$sample_id, tmp$sample_id)
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
        BiocGenerics:::replaceSlots(x, colData=value, check=FALSE)
    }
)

#' @rdname SpatialExperiment-colData
#' @importFrom SummarizedExperiment colData colData<-
#' @export
setReplaceMethod("colData",
    c("SpatialExperiment", "NULL"),
    function(x, value) {
        value <- colData(x)["sample_id"]
        colData(x) <- value
        return(x)
    }
)
