# setters ----------------------------------------------------------------------
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
    function(x, value, sample_id, image_id) 
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

#' @rdname SpatialExperiment-methods
#' @importFrom S4Vectors DataFrame
#' @importFrom SummarizedExperiment colData colData<-
#' @export
setReplaceMethod("colData",
    c("SpatialExperiment", "NULL"),
    function(x, value) 
    {
        BiocGenerics:::replaceSlots(x, colData=value, check=FALSE)
    }
)

#' @rdname SpatialExperiment-methods
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
#' NB \code{in_tissue} has to be in logical form.
#' @importFrom SingleCellExperiment int_colData int_colData<-
#' @importFrom S4Vectors nrow SimpleList isEmpty
#' @importFrom methods is
#' @export
#' @examples
#' example(SpatialExperiment)
#' fakeSpData <- spatialData(se)
#' fakeSpData$array_col <- fakeSpData$array_row
#' spatialData(se) <- as.data.frame(fakeSpData)
#' spatialData(se)
setReplaceMethod(f="spatialData", 
    c("SpatialExperiment", "ANY"),
    function(x, value)
    {
        stopifnot( is.data.frame(value) || class(value)=="DataFrame")
        if ( is.data.frame(value) ) value <- DataFrame(value)
        stopifnot(dim(value)[1]==dim(colData(x))[1])
        spd <- value
        if ( !("in_tissue" %in% colnames(value)) )
        {
            spd <- cbind(value, 1)
            colnames(spd) <- c(colnames(value), "in_tissue")
        }
        msg <- .spatialData_validity(spd, x@spaCoordsNms)
        if (!is.null(msg)) stop(msg)
        x@spatialData <- spd
        return(x) 
    }
)

setReplaceMethod(f="spatialData", 
    c("SpatialExperiment", "NULL"),
    function(x, value)
    {
        x@spatialData <- value
        x@spaCoordsNms <- value
    }
)

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
    function(se, cd_keep=NULL, sample_id=TRUE, as_df=FALSE)
    {
    
        samplesIdx <- 1:nrow(se@spatialData)
        if ( !isTRUE( sample_id ) ) samplesIdx <- which(se$sample_id %in% sample_id)
        if ( !isEmpty( samplesIdx ) )
        {
            coords <- se@spatialData[samplesIdx, ]
        } else {
            stop("Not valid sample_id.")
        }
        if ( !is.null(cd_keep) )
        {
            stopifnot( all( cd_keep %in% colnames(colData(se)) ) )
            nms <- colnames(coords)
            coords <- cbind(coords, colData(se)[[cd_keep]])
            colnames(coords) <- c(nms, cd_keep)
        }
        if ( as_df ) return(as.data.frame(coords))
        return(coords)
    }
)


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


#' @rdname SpatialExperiment-methods
#' @description a getter method which returns the spatial coordinates previously
#' stored in a SpatialExperiment class object.
#' @param se A SpatialExperiment class object.
#' @param sample_id character string, \code{TRUE} or \code{NULL} specifying sample 
#' identifier(s); here, \code{TRUE} is equivalent to all samples.
#' @param as_df logical indicating if the returned structure has to be a 
#' data.frame (default is FALSE).
#' and \code{NULL} specifies the first available entry (see details)
#' @return by default returns a matrix object within the spatial coordinates.
#'
#' @export
#' @examples
#' example(SpatialExperiment)
#' spatialCoords(se)
setMethod(f="spatialCoords", signature="SpatialExperiment",
    function(se, sample_id=TRUE, as_df=FALSE)
    {
        samplesIdx <- 1:nrow(colData(se))
        if ( !isTRUE(sample_id) ) samplesIdx <- which(se$sample_id %in% sample_id)
        
        if ( !isEmpty( samplesIdx ) )
        {
            coords <- spatialData(se)[samplesIdx, se@spaCoordsNms]
        } else {
            stop("Not valid sample_id.")
        }
        
        if (as_df) return(as.data.frame(coords))
        else return(as.matrix(coords))
    }
)


#' @rdname SpatialExperiment-methods
#' @name spatialDataNames
#' @description getter method for the spatial coordinates names in a
#' SpatialExperiment class object.
#' @param x a SpatialExperiment class object.
#'
#' @return SpatialDataNames: a vector with the colnames of the spatial coordinates.
#' @export
#' @examples
#' example(SpatialExperiment)
#' spatialDataNames(se)
setMethod(f="spatialDataNames", signature="SpatialExperiment", function(x)
    {
        return(colnames(spatialData(x)))
    }
)


#' @rdname SpatialExperiment-methods
#' @title isInTissue
#' @description isInTissue: returns a mask of TRUE/FALSE Barcodes spots, 
#' indicating which ones are in tissue and which ones are not.
#' @param x  a VisiumExperiment class object.
#' @param sample_id character string, \code{TRUE} or \code{NULL} specifying sample 
#' identifier(s); here, \code{TRUE} is equivalent to all samples 
#' and \code{NULL} specifies the first available entry (see details)
#' @return isInTissue: a TRUE/FALSE mask.
#' @export
#' @examples
#' data(ve)
#' inTissue(ve)
#' sum(inTissue(ve))
#' ve[inTissue(ve),]
setMethod(f="inTissue", 
    signature="SpatialExperiment", 
    function(x, sample_id=TRUE)
    {
        if ( !("in_tissue" %in% colnames(spatialData(x))) ) 
            stop("No tissue mask loaded!")
        samplesIdx <- 1:nrow(colData(x))
        if ( !isTRUE(sample_id) ) samplesIdx <- which(x$sample_id %in% sample_id)
        return( spatialData(x)$in_tissue[samplesIdx] == 1 )
    }
)


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
