#' @title SpatialExperiment Methods
#' @name SpatialExperiment-methods
#' @rdname SpatialExperiment-methods
#' 
#' @aliases spatialData spatialData<- spatialCoords spatialCoordsNames 
#' spatialCoordsNames<- scaleFactors inTissue spatialDataNames imgData imgData<-
#' 
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
#' DataFrame (default is FALSE).
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
#' It returns by default a DataFrame, but it is possible to retrieve a 
#' matrix by setting \code{as_df=FALSE}.}
#' \item{\code{spatialCoords(x)}:}{ The \code{spatialCoords} getter returns the 
#' spatial coordinates in a matrix-like form, but it is possible to retrieve 
#' them in a DataFrame form by setting \code{as_df=TRUE}.}
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
#' @return see methods details.
#' 
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
#' # use as_df for spatialData to return a DataFrame 
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
#' 
#' @importFrom SingleCellExperiment int_colData int_colData<-
#' @importFrom S4Vectors nrow SimpleList isEmpty DataFrame
#' @importFrom methods is
NULL

# spatialData ------------------------------------------------------------------

#' @rdname SpatialExperiment-methods
#' @export
setMethod("spatialData", 
    "SpatialExperiment",
    function(x, cd_bind=NULL, sample_id=TRUE, as_df=TRUE) {
        # default as_df should be TRUE;
        # any non-numeric columns will convert everything to character
        # returning a matrix is only useful for spatialCoords()
        # that may be an input to spatstats
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
            return(DataFrame(spd))
        as.matrix(spd)
    }
)

#' @rdname SpatialExperiment-methods
#' @export
setReplaceMethod("spatialData", 
    c("SpatialExperiment", "ANY"),
    function(x, value) {
        stopifnot(
            is.matrix(value)
            || is.data.frame(value) 
            || is(value, "DFrame"),
            nrow(value) == ncol(x))
        
        if (!is(value, "DFrame")) {
            nms <- rownames(spatialData(x))
            value <- DataFrame(value, row.names = nms)
        }
        
        if (is.null(value$in_tissue))
            value$in_tissue <- 1
        
        nms <- spatialCoordsNames(x)
        msg <- .spatialData_validity(value, nms)
        if (!is.null(msg)) stop(msg)
        
        x@spatialData <- value
        return(x)
    }
)

#' @rdname SpatialExperiment-methods
#' @export
setReplaceMethod("spatialData", 
    c("SpatialExperiment", "NULL"),
    function(x, value) {
        x@spatialData <- DataFrame()
        x@spatialCoordsNames <- character()
        return(x)
    }
)

#' @rdname SpatialExperiment-methods
#' @export
setMethod("spatialDataNames", 
    "SpatialExperiment", 
    function(x) colnames(spatialData(x)))

# spatialCoords ----------------------------------------------------------------

#' @rdname SpatialExperiment-methods
#' @export
setMethod("spatialCoords", 
    "SpatialExperiment",
    function(x, sample_id=TRUE, as_df=FALSE) {
        if (isTRUE(sample_id)) {
            idx <- TRUE
        } else {
            stopifnot(
                is.character(sample_id), 
                sample_id %in% x$sample_id)
            idx <- which(x$sample_id %in% sample_id)
        }
        nms <- spatialCoordsNames(x)
        coords <- spatialData(x)[idx, nms]
        
        if (as_df)
            return(DataFrame(coords))
        as.matrix(coords)
    }
)

#' @rdname SpatialExperiment-methods
#' @export
setMethod("spatialCoordsNames", 
    "SpatialExperiment", 
    function(x) x@spatialCoordsNames)

#' @rdname SpatialExperiment-methods
#' @export
setReplaceMethod("spatialCoordsNames", 
    c("SpatialExperiment", "character"), 
    function(x, value) {
        old <- spatialCoordsNames(x)
        x@spatialCoordsNames <- value
        if (!isEmpty(old)) {
            if (length(unique(value)) != length(old))
                stop("Number of unique replacement values", 
                    " should be ", length(old))
            idx <- spatialDataNames(x) %in% old
            names(spatialData(x))[idx] <- value
        }
        return(x)
    }
)

# scaleFactors -----------------------------------------------------------------

#' @rdname SpatialExperiment-methods
#' @export
setMethod("scaleFactors", 
    "SpatialExperiment",
    function(x, sample_id=TRUE, image_id=TRUE) {
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
setMethod("inTissue", 
    "SpatialExperiment", 
    function(x, sample_id=TRUE) {
        spd <- spatialData(x)
        if (!"in_tissue" %in% colnames(spd))
            stop("No tissue mask loaded!")
        if (isTRUE(sample_id)) {
            idx <- TRUE
        } else {
            stopifnot(
                is.character(sample_id), 
                sample_id %in% x$sample_id)
            idx <- which(x$sample_id %in% sample_id)
        }
        return(spd[idx, "in_tissue"] == 1)
    }
)

