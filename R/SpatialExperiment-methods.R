#' @name SpatialExperiment-methods
#' @title SpatialExperiment methods
#' @aliases imgData imgData<- colData<- scaleFactors
#' 
#' @param x a \code{\link{SpatialExperiment}}
#' @param i,j indices specifying elements to extract or replace
#' @param value a \code{\link[S4Vectors]{DataFrame}}
#' @param sample_id, image_id 
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
# setReplaceMethod("colData",
#     c("SpatialExperiment", "DataFrame"),
#     function(x, value) {
#         # store original 'colData'
#         old <- colData(x)
#         
#         # do the replacement
#         se <- as(x, "SummarizedExperiment")
#         colData(se) <- value
#         new <- colData(se)
#         
#         # these 'colData' columns should remain existent & valid
#         nms <- c("sample_id", "inTissue", "x_coord", "y_coord")#, "z_coord")
#         
#         # check if any of 'nms' are being renamed
#         if (ncol(new) == ncol(old) 
#             && !setequal(names(old), names(new))
#             && any(nms %in% setdiff(names(old), names(new)))) {
#                 warning(
#                     "cannot rename 'colData' fields ",
#                     paste(sQuote(nms), collapse = ", "))
#                 return(x)
#         }
#         
#         # check that 'nms' still exist
#         # (this is not handled by the check above)
#         if (!all(nms %in% names(new))) {
#             warning(
#                 "cannot drop 'colData' fields", 
#                 paste(sQuote(nms), collapse = ", "))
#             return(x)
#         }
#         
#         # check that 'sample_id's remain valid & update 'imgData' accordingly
#         ns_old <- length(sids_old <- unique(old$sample_id))
#         ns_new <- length(sids_new <- unique(new$sample_id))
#         if (ns_old != ns_new) {
#             warning(sprintf(
#                 "Number of unique 'sample_id's is %s, but %s %s provided", 
#                 ns_old, ns_new, ifelse(ns_new > 1, "were", "was")))
#             return(x)
#         } else if (sum(table(old$sample_id, new$sample_id) != 0) != ns_old) {
#             warning("New 'sample_id's must map uniquely")
#             return(x)
#         } else if (!is.null(imgData(x))) {
#             m <- match(imgData(x)$sample_id, sids_old)
#             imgData(x)$sample_id <- sids_new[m]
#         }
# 
#         # check that 'inTissue' & 'xyzData' remain valid
#         msg <- .colData_inTissue_validity(new$in_tissue)
#         if (length(msg)) { warning(msg); return(x) }
# 
#         # msg <- .colData_xyzData_validity(new$xyzData)
#         # if (length(msg)) { warning(msg); return(x) }
# 
#         # overwrite 'colData' if all checks pasted
#         BiocGenerics:::replaceSlots(x, colData=value, check=FALSE)
#     })

#' @rdname SpatialExperiment-methods
#' @importFrom S4Vectors DataFrame
#' @importFrom SummarizedExperiment colData colData<-
#' @export
setReplaceMethod("colData",
    c("SpatialExperiment", "NULL"),
    function(x, value) {
        # keep required fields only
        cd <- colData(x)[, c("sample_id", "inTissue", "xyzData")] ####################
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
    }
)


#' spatialCoords-getter
#' @description a getter method which returns the spatial coordinates previously
#' stored in a SpatialExperiment class object.
#' @param se A SpatialExperiment class object.
#' @param sample_id 
#' @return a DataFrame within the spatial coordinates.
#'
#' @export
#' @aliases spatialCoords
#' @examples
#' example(SpatialExperiment)
#' spatialCoords(se)
setMethod(f="spatialCoords", signature="SpatialExperiment",
    function(se, sample_id=TRUE)
{
    samplesIdx <- 1:nrow(colData(se))
    if(!isTRUE(sample_id)) samplesIdx <- which(se$sample_id %in% sample_id)
    if( !isEmpty(samplesIdx) )
    {
        coords <- colData(se)[samplesIdx, se@spaCoordsNms]
    } else {
        stop("Provided sample_id is not valid.")
    }
        # z_idx <- grep("z_coord", colnames(colData(se)))
        #
        # if(length(z_idx) != 0)
        # {
        #     coords <- cbind(colData(se)[samplesIdx,"x_coord", drop=FALSE],
        #                     colData(se)[samplesIdx,"y_coord", drop=FALSE],
        #                     colData(se)[samplesIdx,"z_coord", drop=FALSE])
        # } else {
        #     coords <- cbind(colData(se)[samplesIdx,"x_coord", drop=FALSE],
        #                     colData(se)[samplesIdx,"y_coord", drop=FALSE])
        # }

    return(coords)
})

#' spatialCoordsMtx-getter
#' @description a getter method which returns the spatial coordinates previously
#' stored in a SpatialExperiment class object.
#' @param se A SpatialExperiment class object.
#' @param sample_id
#' @return a matrix object within the spatial coordinates.
#'
#' @export
#' @examples
#' example(SpatialExperiment)
#' spatialCoordsMtx(se)
setMethod(f="spatialCoordsMtx", signature="SpatialExperiment",
    function(se, sample_id=TRUE)
{
    samplesIdx <- 1:nrow(colData(se))
    if(!isTRUE(sample_id)) samplesIdx <- which(se$sample_id %in% sample_id)
    z_idx <- grep("z_coord", colnames(colData(se)))
    if(length(z_idx) != 0)
    {
        coords <- cbind(colData(se)[samplesIdx,"x_coord", drop=FALSE],
                        colData(se)[samplesIdx,"y_coord", drop=FALSE],
                        colData(se)[samplesIdx,"z_coord", drop=FALSE])
    } else {
        coords <- cbind(colData(se)[samplesIdx,"x_coord", drop=FALSE],
                        colData(se)[samplesIdx,"y_coord", drop=FALSE])
    }
    return(as.matrix(coords))
})




# setters ----------------------------------------------------------------------
# 
#' #' @rdname SpatialExperiment-methods
#' #' @export
#' setReplaceMethod("scaleFactors", 
#'                  c("SpatialExperiment", "list"),
#'                  function(x, sample_id=TRUE, image_id=TRUE, 
#'                           scaleFactors=as.list(rep(1,1)))
#'                  {
#'                      idx <- .get_img_idx(x, sample_id, image_id)
#'                      imgData(x)$scaleFactor[idx] <- scaleFactors
#'                  }
#' )




.setCoord <- function(df, coordName, coordinates)
{
    stopifnot(dim(df)[1] == length(coordinates))
    df[[coordName]] <- coordinates
    return(df)
}

#' spatialCoords-setter
#' @description a setter method which sets/replaces the spatial coordinate in a
#' SpatialExperiment class object.
#' @param se a SpatialExperiment class object
#' @param coords a DataFrame with the new spatial coordinates to set.
#' @param sample_id 
#' @return none
#' @importFrom SingleCellExperiment int_colData int_colData<-
#' @importFrom S4Vectors nrow SimpleList isEmpty
#' @importFrom methods is
#' @aliases spatialCoords<-
#' @export
#' @examples
#' example(SpatialExperiment)
#' fakeFishCoords <- cbind(fishCoordinates[,c(1:3)], fishCoordinates[,3])
#'         colnames(fakeFishCoords) <- c("MyCell_ID", "Irrelevant", "x", "y")
#' spatialCoords(se) <- fakeFishCoords
#' spatialCoords(se)
setReplaceMethod(f="spatialCoords", signature="SpatialExperiment", 
    function(x, value=DataFrame())#, sample_id=TRUE)
{
    stopifnot(dim(value)[1]==dim(colData(x))[1])
    if(!is(value, "DataFrame")){ value <- DataFrame(value) }
    samplesIdx <- 1:nrow(colData(x))
    # if(!isTRUE(sample_id)) samplesIdx <- which(se$sample_id %in% sample_id)
    i=1
    dfexprs <- rbind(EXPRSNAMES, SPATDATANAMES)
    spaCoords <- character()
    for(i in 1:dim(dfexprs)[2]) 
    {
        idx <- grep(dfexprs[1,i], colnames(value))
        if( !isEmpty(idx) )
        {
            colData(x) <- .setCoord(colData(x)[samplesIdx,], dfexprs[2,i], value[[idx]])
            if(isEmpty(x@spaCoordsNms)) spaCoords <- c(spaCoords, dfexprs[2,i])
        }
    }
    if(isEmpty(x@spaCoordsNms)) x@spaCoordsNms <- spaCoords
    names(x@spaCoordsNms) <- NULL
    ## write new validity xyz
    # if (length(msg)) { warning(msg); return(spe) }
    return(x) 
})

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
