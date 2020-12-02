#' @name SpatialExperiment-methods
#' @title SpatialExperiment methods
#' @aliases imgData imgData<- colData<- scaleFactors
#' 
#' @param x a \code{\link{SpatialExperiment}}
#' @param i,j indices specifying elements to extract or replace
#' @param value a \code{\link[S4Vectors]{DataFrame}}
#' @param sample_id,image_id 
#'   character string, \code{TRUE} or \code{NULL} specifying sample/image 
#'   identifier(s); here, \code{TRUE} is equivalent to all samples/images 
#'   and \code{NULL} specifies the first available entry (see details)

# the following overwrites 'SummarizedExperiment's 'colData' 
# replacement method to assure the 'SpatialExperiment' remains valid
# - in case of an invalid replacement, we throw a warning 
#   (not an error) and return the (unmodified) input object
# - in case of replacement by NULL, we retain required fields only

# TODO: should invalid replacement return a warning or message?

#' @rdname SpatialExperiment-methods
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
        
        # these 'colData' columns should remain existent & valid
        nms <- c("sample_id", "in_tissue", "xy_coords")
        
        # check if any of 'nms' are being renamed
        if (ncol(new) == ncol(old) 
            && !setequal(names(old), names(new))
            && any(nms %in% setdiff(names(old), names(new)))) {
                warning(
                    "cannot rename 'colData' fields ",
                    paste(sQuote(nms), collapse = ", "))
                return(x)
        }
        
        # check that 'nms' still exist
        # (this is not handled by the check above)
        if (!all(nms %in% names(new))) {
            warning(
                "cannot drop 'colData' fields", 
                paste(sQuote(nms), collapse = ", "))
            return(x)
        }
        
        # check that 'sample_id's remain valid & update 'imgData' accordingly
        ns_old <- length(sids_old <- unique(old$sample_id))
        ns_new <- length(sids_new <- unique(new$sample_id))
        if (ns_old != ns_new) {
            warning(sprintf(
                "Number of unique 'sample_id's is %s, but %s %s provided", 
                ns_old, ns_new, ifelse(ns_new > 1, "were", "was")))
            return(x)
        } else if (sum(table(old$sample_id, new$sample_id) != 0) != ns_old) {
            warning("New 'sample_id's must map uniquely")
            return(x)
        } else if (!is.null(imgData(x))) {
            m <- match(imgData(x)$sample_id, sids_old)
            imgData(x)$sample_id <- sids_new[m]
        }
        
        # check that 'in_tissue' & 'xy_coords' remain valid
        msg <- .colData_in_tissue_validity(new$in_tissue)
        if (length(msg)) { warning(msg); return(x) }
        
        msg <- .colData_xy_coords_validity(new$xy_coords)
        if (length(msg)) { warning(msg); return(x) }

        # overwrite 'colData' if all checks pasted
        BiocGenerics:::replaceSlots(x, colData=value, check=FALSE)
    })

#' @rdname SpatialExperiment-methods
#' @importFrom S4Vectors DataFrame
#' @importFrom SummarizedExperiment colData colData<-
#' @export
setReplaceMethod("colData",
    c("SpatialExperiment", "NULL"),
    function(x, value) {
        # keep required fields only
        cd <- colData(x)[, c("sample_id", "in_tissue", "xy_coords")]
        BiocGenerics:::replaceSlots(x, colData=cd, check=FALSE)
    })

# similar to the above, we overwrite the default subsetting method
# to assure that the 'imgData' is subsetted if samples are dropped
#' @rdname SpatialExperiment-methods
#' @importFrom methods callNextMethod
#' @export
setMethod("[", 
    c("SpatialExperiment", "ANY", "ANY"), 
    function(x, i, j) 
{
    if (missing(i)) i <- TRUE
    if (missing(j)) j <- TRUE
    x <- callNextMethod()
    keep <- imgData(x)$sample_id %in% unique(x$sample_id)
    imgData(x) <- imgData(x)[keep, ]
    return(x)
})

# getters ----------------------------------------------------------------------

#' @rdname SpatialExperiment-methods
#' @export
setMethod("scaleFactors", "SpatialExperiment",
    function(x, sample_id=TRUE, image_id=TRUE)
    {
        idx <- .get_img_idx(x, sample_id, image_id)
        imgData(x)$scaleFactor[idx]
    })


#' #' spatialCoords-getter
#' #' @description a getter method which returns the spatial coordinates previously
#' #' stored in a SpatialExperiment class object.
#' #' @param x A SpatialExperiment class object.
#' #'
#' #' @return a DataFrame within the spatial coordinates.
#' #'
#' #' @export
#' #' @aliases spatialCoords
#' #' @examples
#' #' example(SpatialExperiment)
#' #' spatialCoords(se)
#' setMethod(f="spatialCoords", signature="SpatialExperiment", function(x)
#' {
#'     return(int_colData(x)$spatial)
#'     
#' })
#' 
#' #' spatialCoords-setter
#' #' @description a setter method which sets/replaces the spatial coordinate in a
#' #' SpatialExperiment class object.
#' #' @param x a SpatialExperiment class object
#' #' @param value a DataFrame with the new spatial coordinates to set.
#' #' @return none
#' #' @importFrom SingleCellExperiment int_colData int_colData<-
#' #' @importFrom S4Vectors nrow SimpleList
#' #' @importFrom methods is
#' #' @aliases spatialCoords<-
#' #' @export
#' #' @examples
#' #' example(SpatialExperiment)
#' #' fakeFishCoords <- cbind(fishCoordinates[,c(1:3)], fishCoordinates[,3])
#' #'         colnames(fakeFishCoords) <- c("MyCell_ID", "Irrelevant", "x", "y")
#' #' spatialCoords(se) <- fakeFishCoords
#' #' spatialCoords(se)
#' setReplaceMethod(f="spatialCoords", signature="SpatialExperiment", function(x, value=DataFrame())
#' {
#'     # stopifnot(!sum(S4Vectors::isEmpty(value)))
#'     if(!is(value, "DataFrame")) 
#'     {
#'         value <- DataFrame(value)
#'     }
#'     if(sum(dim(value)==c(0,0))<2)
#'     {
#'         int_colData(x)$spatial <- value
#'     } else {
#'         int_colData(x)$spatial <- DataFrame(row.names=colnames(x))
#'     }
#'     
#' 
#'     return(x)
#' })
#' 
#' 
#' #' spatialCoordsNames-getter
#' #' @description getter method for the spatial coordinates names in a
#' #' SpatialExperiment class object.
#' #' @param x a SpatialExperiment class object.
#' #'
#' #' @return a vector with the colnames of the spatial coordinates.
#' #' @export
#' #' @aliases spatialCoordsNames
#' #' @examples
#' #' example(SpatialExperiment)
#' #' spatialCoordsNames(se)
#' setMethod(f="spatialCoordsNames", signature="SpatialExperiment", function(x)
#' {
#'     return(colnames(int_colData(x)$spatial))
#' })
