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
#' \describe{
#' \item{\code{spatialData(x) <- value}:}{ The spatialData setter expects a 
#' data.frame/DataFrame object named with the
#' previously defined colnames for the spatial coordinates.
#' It is possible to get/set the spatial coordinates names with the 
#' spatialCoordsNames getter/setter, by default they are set by the 
#' SpatialExperiment constructor to c("x", "y").
#' If the input spatial data structure doesn't contain an in_tissue column, 
#' a default in_tissue column is set to 1.
#' If NULL is passed as value the all the related data structures are set 
#' accordingly.}
#' \item{\code{spatialData(x)}:}{ The spatialData getter gives the possibility 
#' to retrieve also additional columns from the
#' colData structure by the aid of the \code{cd_bind} argument.
#' It returns be default a DataFrame, but it is possible to retrieve a 
#' data.frame by setting \code{as_df=TRUE}.}
#' \item{\code{spatialCoords(x)}:}{ The \code{spatialCoords} getter returns the 
#' spatial coordinates in a matrix-like form, but it is possible to retrieve 
#' them in a data.frame form by setting \code{as_df=TRUE}.}
#' }
#'
#' @section spatialNames Methods:
#' \describe{
#' \item{\code{spatialDataNames(x)}:}{ Returns the names of the spatialData 
#' structure.}
#' \item{\code{spatialCoordsNames(x)<-value}:}{ Sets the names of the spatial 
#' coordinates accordingly with the assigned value.}
#' \item{\code{spatialCoordsNames(x)}:}{ Returns the stored names of the spatial 
#' coordinates.}
#' }
#' 
#' @section imgData Methods:
#' \describe{
#' \item{\code{imgData(x)<-value}:}{ This setter allows to set a DataFrame 
#' object as imgData of the SpatialExperiment object class.}
#' \item{\code{imgData(x)}:}{ This getter returns the imgData DataFrame.}
#' }
#' 
#' 
#' @section Other Methods:
#' \describe{
#' \item{\code{scaleFactors(x, sample_id, image_id)}:}{ This getter returns 
#' the scale factors associated to the \code{sample_id}(s) 
#' and \code{image_id}(s) passed as input.
#' This is related to the stored image(s) into the SpatialExperiment.
#' \code{imgData} structure.
#' See the \code{sample_id} and \code{image_id} section description for further 
#' details.}
#' \item{\code{inTissue(x, sample_id)}:}{ This getter return a TRUE/FALSE mask
#' associated to the samples that are on a tissue.
#' This is typical related to 10x Visium experiments.
#' See the \code{sample_id} section description for further details.}
#' }
#' 
#' @param x a SpatialExperiment class object
#' @param value it varies depending on the invoked method see methods details.
#' @param sample_id character string specifying sample 
#' identifier(s); \code{TRUE} is equivalent to all samples.
#' @param image_id character string indicating the image identifiers(s), 
#' \code{TRUE} is equivalent to all images.
#' @param cd_bind a character vector indicating additional columns to return 
#' that can be retrieved from the \code{colData} structure. If this is TRUE all
#' the colData are binded to the spatialData.
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

# spatialData ------------------------------------------------------------------

#' @rdname SpatialExperiment-methods
#' @export
setMethod("spatialData", "SpatialExperiment",
    function(x, cd_bind=NULL, sample_id=TRUE, as_df=TRUE)
        # default as_df should be TRUE;
        # any non-numeric columns will convert everything to character
        # returning a matrix is only useful for spatialCoords()
        # that may be an input to spatstats
    {
        if (isEmpty(x@spatialData)) 
            return(x@spatialData)

        if (!isTRUE(sample_id)) {
            stopifnot(
                is.character(sample_id), 
                sample_id %in% x$sample_id)
            x <- x[, x$sample_id %in% sample_id]
        }
        spd <- x@spatialData
        if (!is.null(cd_bind)) {
            if (isTRUE(cd_bind)) {
                cd_bind <- colnames(colData(x))
            } else {
                stopifnot( 
                    is.character(cd_bind),
                    cd_bind %in% colnames(colData(x)))
            }
            nms <- colnames(spd)
            cd <- colData(x)[cd_bind]
            spd <- cbind(spd, cd)
            colnames(spd) <- c(nms, cd_bind)
        }
        if (as_df)
            return(as.data.frame(spd))
        as.matrix(spd)
    }
)

#' @rdname SpatialExperiment-methods
#' @export
setReplaceMethod(f="spatialData", 
    c("SpatialExperiment", "ANY"),
    function(x, value)
    {
        stopifnot(is.matrix(value)
            || is.data.frame(value) 
            || is(value, "DFrame"))
        if (!is(value, "DFrame")) 
            value <- DataFrame(value)
        stopifnot(nrow(value) == ncol(x))
        if (is.null(value$in_tissue))
            value$in_tissue <- 1
        msg <- .spatialData_validity(value, x@spatialCoordsNames)
        if (!is.null(msg)) stop(msg)
        x@spatialData <- value
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
        x@spatialCoordsNames <- character()
        return(x)
    }
)

#' @rdname SpatialExperiment-methods
#' @export
setMethod(f="spatialDataNames", 
    signature="SpatialExperiment", 
    function(x) colnames(spatialData(x)))

# spatialCoords ----------------------------------------------------------------

#' @rdname SpatialExperiment-methods
#' @export
setMethod(f="spatialCoords", 
    signature="SpatialExperiment",
    function(x, sample_id=TRUE, as_df=FALSE)
    {
        if (isTRUE(sample_id)) {
            idx <- seq_len(nrow(spatialData(x)))
        } else {
            stopifnot(
                is.character(sample_id), 
                sample_id %in% x$sample_id)
            idx <- which(x$sample_id %in% sample_id)
        }
        coords <- spatialData(x)[idx, spatialCoordsNames(x)]
        if (as_df)
            return(as.data.frame(coords))
        as.matrix(coords)
    }
)

#' @rdname SpatialExperiment-methods
#' @export
setMethod(f="spatialCoordsNames", 
    signature="SpatialExperiment", 
    function(x)
    {
        return(x@spatialCoordsNames)
    }
)

#' @rdname SpatialExperiment-methods
#' @export
setReplaceMethod(f="spatialCoordsNames", 
    signature=c("SpatialExperiment", "character"), 
    function(x, value)
    {
        if ( !isEmpty(x@spatialCoordsNames) ) 
        {
            if (length(unique(value)) != length(x@spatialCoordsNames))
                stop("Number of unique replacement values", 
                    " should be ", length(x@spatialCoordsNames))
            idx <- spatialDataNames(x) %in% x@spatialCoordsNames
            colnames(x@spatialData)[idx] <- value
        }
        x@spatialCoordsNames <- value
        return(x)
    }
)

# scaleFactors -----------------------------------------------------------------

#' @rdname SpatialExperiment-methods
#' @export
setMethod("scaleFactors", "SpatialExperiment",
    function(x, sample_id=TRUE, image_id=TRUE)
    {
        stopifnot(!is.null(imgData(x)))
        idx <- .get_img_idx(x, sample_id, image_id)
        imgData(x)$scaleFactor[idx]
    }
)

# TODO: do we really need this function? 
# it's easy enough for anyone to just check e.g. 
# table(spatialData(x)$in_tissue, x$sample_id)
#' @rdname SpatialExperiment-methods
#' @export
setMethod(f="inTissue", 
    signature="SpatialExperiment", 
    function(x, sample_id=TRUE)
    {
        if (!("in_tissue" %in% colnames(spatialData(x)))) 
            stop("No tissue mask loaded!")
        if (isTRUE(sample_id)) {
            idx <- seq_len(nrow(spatialData(x)))
        } else {
            stopifnot(
                is.character(sample_id), 
                sample_id %in% x$sample_id)
            idx <- which(x$sample_id %in% sample_id)
        }
        return(spatialData(x)[idx, "in_tissue"] == 1)
    }
)

