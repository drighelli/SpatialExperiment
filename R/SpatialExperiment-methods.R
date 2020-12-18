# setters ----------------------------------------------------------------------
# #' @name SpatialExperiment-methods
#' @title SpatialExperiment methods
#' @aliases imgData imgData<- colData<- scaleFactors  
#' 
#' @param x a \code{\link{SpatialExperiment}}
#' @param value a \code{\link[S4Vectors]{DataFrame}}
#' @param sample_id character string, \code{TRUE} or \code{NULL} specifying sample/image 
#'   identifier(s); here, \code{TRUE} is equivalent to all samples/images 
#'   and \code{NULL} specifies the first available entry (see details)
#' @param image_id character string, \code{TRUE} or \code{NULL} specifying sample/image 
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
    function(x, value, sample_id, image_id) {
        # store original 'colData'
        old <- colData(x)

        # do the replacement
        se <- as(x, "SummarizedExperiment")
        colData(se) <- value
        new <- colData(se)

        # these 'colData' columns should remain existent & valid
        # nms <- c("sample_id", "in_tissue", "x_coord", "y_coord")#, "z_coord")
        if(!isEmpty(x@spaCoordsNms)) ## during initialization of the object the validity not works
        {
            nms <- x@spaCoordsNms
            
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
                    "Number of unique 'sample_id's is %s, but %s %s provided.\nOverwriting",
                    ns_old, ns_new, ifelse(ns_new > 1, "were", "was"))) 
                return(x) 
            } else if (sum(table(old$sample_id, new$sample_id) != 0) != ns_old) {
                warning("New 'sample_id's must map uniquely")
                return(x)
            } else if (!is.null(imgData(x))) {
                m <- match(imgData(x)$sample_id, sids_old)
                imgData(x)$sample_id <- sids_new[m]
            }
    
            # check that 'inTissue' & 'xyzData' remain valid
            msg <- .colData_inTissue_validity(new$in_tissue)
            if (length(msg)) { warning(msg); return(x) }
    
            msg <- .colData_spatialCoords_validity(x)
            if (length(msg)) { warning(msg); return(x) }
            # overwrite 'colData' if all checks pasted
           
        }
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
        cd <- colData(x)[, c("sample_id", x@spaCoordsNms)] 
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



#' spatialData<-
#' @description 
#' a setter method which sets/replaces the spatial data in a
#' SpatialExperiment class object.
#' They are always stored as \code{x_coord}, \code{y_coord} and when found 
#' \code{in_tissue}, \code{array_row} and \code{array_col} (see details).
#' @param x a SpatialExperiment class object
#' @param value a DataFrame with the new spatial data to set (see details).
#' @param sample_id 
#' character string, \code{TRUE} or \code{NULL} specifying sample 
#' identifier(s); here, \code{TRUE} is equivalent to all samples 
#' and \code{NULL} specifies the first available entry (see details)
#' @return none
#' @details The method automatically recognizes any kind of colnames with
#' \code{x}, \code{y}, \code{z}, \code{x_coord}, \code{y_coord}, \code{z_coord} and, 
#' additionally, for the 10x Visium, it recognizes the 
#' \code{pxl_row_in_fullres} and \code{pxl_col_in_fullres} storing them respectively as 
#' \code{y_coord} and \code{x_coord}.
#' While \code{in_tissue} \code{array_row} \code{array_col} are stored as they are.
#' NB \code{in_tissue} have to be in logical form.
#' @importFrom SingleCellExperiment int_colData int_colData<-
#' @importFrom S4Vectors nrow SimpleList isEmpty
#' @importFrom methods is
#' @export
#' @examples
#' example(SpatialExperiment)
#' fakeCoords <- cbind(spatialData(se)$x_coord, spatialData(se)$x_coord)
#' colnames(fakeCoords) <- c("x", "y")
#' spatialData(se) <- fakeCoords
#' spatialData(se)
#' oneCoord <- cbind(spatialData(se)$y_coord)
#' colnames(oneCoord) <- c("pxl_row_in_fullres") # assigns it to the y_coord
#' spatialData(se) <- oneCoord
#' spatialData(se)
setReplaceMethod(f="spatialData", signature="SpatialExperiment", 
    function(x, value=NULL, sample_id=TRUE)
 {

    stopifnot(dim(value)[1]==dim(colData(x))[1])
    if(!is(value, "DataFrame")){ value <- DataFrame(value) }
    if(!is.null(value))
    {
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
        
        if(isEmpty(x@spaCoordsNms)) 
        {
            names(spaCoords) <- NULL
            x@spaCoordsNms <- spaCoords
            # names(x@spaCoordsNms) <- NULL
        }
    }
    # msg <- .colData_spatialCoords_validity(x)
    # if (length(msg)) { warning(msg); return(x) }
    return(x) 
})

#' 
#' #' @rdname SpatialExperiment-methods
#' #' @export
#' setReplaceMethod("scaleFactors", "SpatialExperiment",
#'           function(x, value, sample_id=TRUE, image_id=TRUE)
#'           {
#'               stopifnot(exists("int_metadata(se)$imgData"))
#'               idx <- .get_img_idx(x, sample_id, image_id)
#'               imgData(x)$scaleFactor[idx] <- value
#'           }
#' )


# getters ----------------------------------------------------------------------

#' @rdname SpatialExperiment-methods
#' @export
setMethod("scaleFactors", "SpatialExperiment",
    function(x, sample_id=TRUE, image_id=TRUE)
    {
        stopifnot(!is.null(int_metadata(x)$imgData))
        idx <- .get_img_idx(x, sample_id, image_id)
        imgData(x)$scaleFactor[idx]
    }
)


#' spatialData
#' @description a getter method which returns the spatial coordinates 
#' data structure previously stored in a SpatialExperiment class object.
#' @param se A SpatialExperiment class object.
#' @param colDataCols a character vector indicating additional columns to return 
#' that can be retrieved from the \code{colData} structure.
#' @param sample_id character string, \code{TRUE} or \code{NULL} specifying sample 
#' identifier(s); here, \code{TRUE} is equivalent to all samples.
#' @param as.df logical indicating if the returned structure has to be a 
#' data.frame (default is FALSE).
#' and \code{NULL} specifies the first available entry (see details)
#' @return a DataFrame within the spatial coordinates.
#'
#' @export
#' @examples
#' example(SpatialExperiment)
#' spatialData(se)
setMethod(f="spatialData", signature="SpatialExperiment",
    function(se, colDataCols=NULL, sample_id=TRUE, as.df=FALSE)
{
    samplesIdx <- 1:nrow(colData(se))
    if ( !isTRUE( sample_id ) ) samplesIdx <- which(se$sample_id %in% sample_id)
    if ( !isEmpty(samplesIdx) )
    {
        coords <- colData(se)[samplesIdx, se@spaCoordsNms]
    } else {
        stop("Provided sample_id is not valid.")
    }
    if(!is.null(colDataCols)) 
    {
        stopifnot( all( colDataCols %in% colnames(colData(se)) ) )
        nms <- colnames(coords)
        coords <- cbind(coords, colData(se)[[colDataCols]])
        colnames(coords) <- c(nms, colDataCols)
    }
    if ( as.df ) return(as.data.frame(coords))
    return(coords)
})


#' spatialCoords
#' @description a getter method which returns the spatial coordinates previously
#' stored in a SpatialExperiment class object.
#' @param se A SpatialExperiment class object.
#' @param colDataCols a character vector indicating additional columns to return 
#' that can be retrieved from the \code{colData} structure.
#' @param sample_id character string, \code{TRUE} or \code{NULL} specifying sample 
#' identifier(s); here, \code{TRUE} is equivalent to all samples.
#' @param as.df logical indicating if the returned structure has to be a 
#' data.frame (default is FALSE).
#' and \code{NULL} specifies the first available entry (see details)
#' @return by default returns a matrix object within the spatial coordinates.
#'
#' @export
#' @examples
#' example(SpatialExperiment)
#' spatialCoords(se)
setMethod(f="spatialCoords", signature="SpatialExperiment",
    function(se, colDataCols=NULL, sample_id=TRUE, as.df=FALSE)
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
    if(!is.null(colDataCols)) 
    {
        stopifnot( all( colDataCols %in% colnames(colData(se)) ) )
        nms <- colnames(coords)
        coords <- cbind(coords, colData(se)[[colDataCols]])
        colnames(coords) <- c(nms, colDataCols)
    }
    if (as.df) return(as.data.frame(coords))
    else return(as.matrix(coords))
})


#' spatialDataNames
#' @description getter method for the spatial coordinates names in a
#' SpatialExperiment class object.
#' @param x a SpatialExperiment class object.
#'
#' @return a vector with the colnames of the spatial coordinates.
#' @export
#' @examples
#' example(SpatialExperiment)
#' spatialDataNames(se)
setMethod(f="spatialDataNames", signature="SpatialExperiment", function(x)
{
    return(x@spaCoordsNms)
})


#' isInTissue
#' @description returns a mask of TRUE/FALSE Barcodes spots, indicating which
#' ones are in tissue and which ones are not.
#' @param x  a VisiumExperiment class object.
#' @param sample_id character string, \code{TRUE} or \code{NULL} specifying sample 
#' identifier(s); here, \code{TRUE} is equivalent to all samples 
#' and \code{NULL} specifies the first available entry (see details)
#' @return a TRUE/FALSE mask.
#' @export
#' @examples
#' data(ve)
#' isInTissue(ve)
#' sum(isInTissue(ve))
#' ve[isInTissue(ve),]
setMethod(f="isInTissue", signature="SpatialExperiment", function(x, sample_id=TRUE)
{
    if(!("in_tissue" %in% x@spaCoordsNms)) stop("No tissue mask loaded!")
    samplesIdx <- 1:nrow(colData(x))
    if(!isTRUE(sample_id)) samplesIdx <- which(x$sample_id %in% sample_id)
    return( x$in_tissue[samplesIdx] == 1 ) 
})


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
