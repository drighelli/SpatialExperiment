#' @name SpatialExperiment-methods
#' @rdname SpatialExperiment-methods
#' @title SpatialExperiment Methods
#' @description 
#' The SpatialExperiment class provides a family of getter/setter methods 
#' to get/replace the spatial data attributes.
#' It is always suggested to use these accessors to facilitate future maintenance
#' of the code.
#' We distinguish the spatial data from the spatial coordinates as described in 
#' this paragraph.
#' With spatial data we refer to the general data structure containing the 
#' spatial coordinates, while we refer for spatial coordinates only to the 
#' specific spatial coordinates columns (as x, y, z, ...).
#' 
#' @section spatialData Methods:
#' All the methods described in this section takes as input the \code{sample_id}
#' argument.
#' See the parameter description for additional information.
#'  \describe{
#' \item{\code{spatialData(x) <- value}:}{The spatialData setter expects a 
#' data.frame/DataFrame object named with the
#' previously defined colnames for the spatial coordinates.
#' It is possible to get/set the spatial coordinates names with the 
#' spatialCoordsNames getter/setter, by default they are set by the 
#' SpatialExperiment constructor to c("x", "y").
#' If the input spatial data structure doesn't contain an in_tissue column, 
#' a default in_tissue column is set to 1.
#' If NULL is passed as value the all the related data structures are set 
#' accordingly.}
#' \item{\code{spatialData(x)}:}{The spatialData getter gives the possibility 
#' to retrieve also additional columns from the
#' colData structure by the aid of the cd_keep argument.
#' It returns be default a DataFrame, but it is possible to retrieve a 
#' data.frame by setting \code{as_df=TRUE}.}
#' \item{\code{spatialCoords(x)}:}{The \code{spatialCoords} getter returns the 
#' spatial coordinates in a matrix-like form, but it is possible to retrieve 
#' them in a data.frame form by setting \code{as_df=TRUE}.}
#'}
#' @section spatialNames Methods:
#' 
#' \describe{
#' \item{\code{spatialDataNames(x)}:}{Returns the names of the spatialData 
#' structure.}
#' \item{\code{spatialCoordsNames(x)<-value}:}{Sets the names of the spatial 
#' coordinates accordingly with the assigned value.}
#' \item{\code{spatialCoordsNames(x)}:}{Returns the stored names of the spatial 
#' coordinates.}
#' }
#' @section Other Methods:
#' \describe{
#' \item{\code{scaleFactors(x, sample_id, image_id)}:}{This getter returns 
#' the scale factors associated to the \code{sample_id}(s) 
#' and \code{image_id}(s) passed as input.
#' This is related to the stored image(s) into the SpatialExperiment.
#' \code{imgData} structure.
#' See the \code{sample_id} and \code{image_id} section description for further 
#' details.}
#' \item{\code{inTissue(x, sample_id)}:}{This getter return a TRUE/FALSE mask
#' associated to the samples that are on a tissue.
#' This is typical related to 10x Visium experiments.
#' See the \code{sample_id} section description for further details.}
#' }
#' 
#' @param x a SpatialExperiment class object
#' @param value it varies depending on the invoked method see methods details.
#' @param sample_id character string, \code{TRUE} specifying sample 
#' identifier(s); \code{TRUE} is equivalent to all samples.
#' @param cd_keep a character vector indicating additional columns to return 
#' that can be retrieved from the \code{colData} structure.
#' @param as_df logical indicating if the returned structure has to be a 
#' data.frame (default is FALSE).
#' 
#' @return see methods details.
#' 
#' @importFrom SingleCellExperiment int_colData int_colData<-
#' @importFrom S4Vectors nrow SimpleList isEmpty
#' @importFrom methods is
#' @examples
#' example(SpatialExperiment)
#' ## The spatialData methods
#' fakeSpData <- spatialData(se)
#' fakeSpData$array_col <- fakeSpData$array_row
#' spatialData(se) <- as.data.frame(fakeSpData)
#' 
#' spatialData(se)
#' 
#' spatialCoords(se)
#' 
#' spatialCoordsNames(se)
#' spatialCoordsNames(se) <- c("x","y")
#' spatialCoordsNames(se)
#' spatialData(se)
#' 
#' 
#' scaleFactors(se)
#' 
#' inTissue(se)
#' sum(inTissue(se))
#' se[inTissue(se),]
NULL


### spatialData-methods --------------------------------------------------------
#' @export
setReplaceMethod(f="spatialData", 
    c("SpatialExperiment", "ANY"),
    function(x, value)
    {
        stopifnot( is.data.frame(value) || class(value) == "DataFrame" || 
                  class(value) == "DFrame")
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



#' @export
setReplaceMethod(f="spatialData", 
    c("SpatialExperiment", "NULL"),
    function(x, value)
    {
        x@spatialData <- DataFrame()
        x@spaCoordsNms <- character()
        return(x)
    }
)

#' @export
setMethod(f="spatialData", 
    signature="SpatialExperiment",
    function(se, cd_keep=NULL, sample_id=TRUE, as_df=FALSE)
    {
        if ( isEmpty(se@spatialData) ) return(se@spatialData)
        
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

#' @export
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

#' @export
setMethod(f="spatialCoordsNames", 
    signature="SpatialExperiment", 
    function(x)
    {
        return(x@spaCoordsNms)
    }
)

#' @export
setReplaceMethod(f="spatialCoordsNames", 
    signature=c("SpatialExperiment", "character"), 
    function(x, value)
    {
        if ( !isEmpty(x@spaCoordsNms) ) 
        {
            idx <- which(spatialDataNames(x) %in% x@spaCoordsNms)
            spatialData(x)[,idx] <- value
        }
        x@spaCoordsNms  <- value
        return(x)
    }
)


#' @export
setMethod(f="spatialDataNames", signature="SpatialExperiment", function(x)
    {
        if(!isEmpty(spatialData(x))) return(colnames(spatialData(x)))
        else return(character())
    }
)


### scaleFactors-methods -------------------------------------------------------
#' @export
setMethod("scaleFactors", "SpatialExperiment",
    function(x, sample_id=TRUE, image_id=TRUE)
    {
        stopifnot(!is.null(int_metadata(x)$imgData))
        idx <- .get_img_idx(x, sample_id, image_id)
        imgData(x)$scaleFactor[idx]
    }
)


#' @export
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

