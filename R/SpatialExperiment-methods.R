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
#'  \itemize{
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
#'}
#'
#' @section spatialNames Methods:
#' \describe{
#' \itemize{
#' \item{\code{spatialDataNames(x)}:}{Returns the names of the spatialData 
#' structure.}
#' \item{\code{spatialCoordsNames(x)<-value}:}{Sets the names of the spatial 
#' coordinates accordingly with the assigned value.}
#' \item{\code{spatialCoordsNames(x)}:}{Returns the stored names of the spatial 
#' coordinates.}
#' }
#' }
#' 
#' @section imgData Methods:
#' \describe{
#' \itemize{
#' \item{\code{imgData(x)<-value}:}{This setter allows to set a DataFrame 
#' object as imgData of the SpatialExperiment object class.}
#' \item{\code{imgData(x)}:}{This getter returns the imgData DataFrame.}
#' }
#' }
#' 
#' 
#' @section Other Methods:
#' \describe{
#' \itemize{
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
#' }
#' 
#' @param x a SpatialExperiment class object
#' @param value it varies depending on the invoked method see methods details.
#' @param sample_id character string specifying sample 
#' identifier(s); \code{TRUE} is equivalent to all samples.
#' @param image_id character string indicating the image identifiers(s), 
#' \code{TRUE} is equivalent to all images.
#' @param cd_bind a character vector indicating additional columns to return 
#' that can be retrieved from the \code{colData} structure.
#' @param as_df logical indicating if the returned structure has to be a 
#' data.frame (default is FALSE).
#' 
#' @return see methods details.
#' 
#' @aliases spatialData spatialData<- spatialCoords spatialCoordsNames 
#' spatialCoordsNames<- scaleFactors inTissue spatialDataNames imgData imgData<-
#' 
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
#' # spatialData returns a DataFrame
#' head(spatialData(se))
#' 
#' # for combining spatialData with colData use cd_bind 
#' # use as_df for spatialData to return a data.frame 
#' head(spatialData(se, cd_bind="sample_id", as_df=TRUE))
#'  
#' # spatialCoords returns a matrix of coordinates
#' head(spatialCoords(se))
#' 
#' # changing spatial coordinates Names
#' spatialCoordsNames(se)
#' spatialCoordsNames(se) <- c("x","y")
#' spatialCoordsNames(se)
#' head(spatialData(se))
#' 
#' # imgData
#' imgData(se)
#' 
#' scaleFactors(se)
#' 
#' inTissue(se)
#' sum(inTissue(se))
#' se[inTissue(se),]
NULL


### spatialData-methods --------------------------------------------------------
#' @rdname SpatialExperiment-methods
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


#' @rdname SpatialExperiment-methods
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

#' @rdname SpatialExperiment-methods
#' @export
setMethod(f="spatialData", 
    signature="SpatialExperiment",
    function(x, cd_bind=NULL, sample_id=TRUE, as_df=FALSE)
    {
        if ( isEmpty(x@spatialData) ) return(x@spatialData)
        
        samplesIdx <- 1:nrow(x@spatialData)
        if ( !isTRUE( sample_id ) ) samplesIdx <- which(x$sample_id %in% sample_id)
        if ( !isEmpty( samplesIdx ) )
        {
            coords <- x@spatialData[samplesIdx, ]
        } else {
            stop("Not valid sample_id.")
        }
        if ( !is.null(cd_bind) )
        {
            stopifnot( all( cd_bind %in% colnames(colData(x)) ) )
            nms <- colnames(coords)
            coords <- cbind(coords, colData(x)[[cd_bind]])
            colnames(coords) <- c(nms, cd_bind)
        }
        if ( as_df ) return(as.data.frame(coords))
        return(coords)
    }
)

#' @rdname SpatialExperiment-methods
#' @export
setMethod(f="spatialCoords", signature="SpatialExperiment",
    function(x, sample_id=TRUE, as_df=FALSE)
    {
        samplesIdx <- 1:nrow(colData(x))
        if ( !isTRUE(sample_id) ) samplesIdx <- which(x$sample_id %in% sample_id)
      
        if ( !isEmpty( samplesIdx ) )
        {
            coords <- spatialData(x)[samplesIdx, x@spaCoordsNms]
        } else {
            stop("Not valid sample_id.")
        }
      
        if (as_df) return(as.data.frame(coords))
        else return(as.matrix(coords))
    }
)

### spatialNames-methods -------------------------------------------------------

#' @rdname SpatialExperiment-methods
#' @export
setMethod(f="spatialCoordsNames", 
    signature="SpatialExperiment", 
    function(x)
    {
        return(x@spaCoordsNms)
    }
)

#' @rdname SpatialExperiment-methods
#' @export
setReplaceMethod(f="spatialCoordsNames", 
    signature=c("SpatialExperiment", "character"), 
    function(x, value)
    {
        if ( !isEmpty(x@spaCoordsNms) ) 
        {
            idx <- which(spatialDataNames(x) %in% x@spaCoordsNms)
            colnames(x@spatialData)[idx] <- value
        }
        x@spaCoordsNms  <- value
        return(x)
    }
)

#' @rdname SpatialExperiment-methods
#' @export
setMethod(f="spatialDataNames", signature="SpatialExperiment", function(x)
    {
        if(!isEmpty(spatialData(x))) return(colnames(spatialData(x)))
        else return(character())
    }
)


### scaleFactors-methods -------------------------------------------------------
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

