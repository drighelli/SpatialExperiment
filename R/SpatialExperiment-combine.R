#' @title Combining SpatialExperiment objects
#' @name SpatialExperiment-combine
#' @rdname SpatialExperiment-combine
#' @docType methods
#' @aliases cbind,SingleCellExperiment-method
#' 
#' @description
#' An overview of methods to combine multiple \linkS4class{SpatialExperiment} 
#' objects by column.
#' This method is useful for ensuring that all data fields remain 
#' synchronized multiple SpatialExperiment objects are combined together, 
#' such as in case of multiple samples.
#'
#' @section Combining:
#' The \code{...} argument is thought to contain one or more 
#' \linkS4class{SpatialExperiment} objects.
#' 
#' \describe{
#' \item{\code{cbind(..., deparse.level=1)}:}{ 
#' Returns a SpatialExperiment where all objects in \code{...}
#' are combined column-wise, i.e., columns in successive objects 
#' are appended to the first object.
#' 
#' Each \code{SpatialExperiment} object in \code{...} must have the same 
#' \code{colData} (with same \code{spatialCoordinates}).
#' In case of equal sample_id the method will proceed them providing new 
#' unique ones. 
#' Additionally the \code{imgData} across all the object are \code{rbind}ed. 
#'
#' Refer to \code{?"\link{cbind,SingleCellExperiment-method}"} 
#' for details on how metadata and others inherited attributes 
#' are combined in the output object.
#' 
#' Refer to \code{?\link[base]{cbind}} 
#' for the interpretation of \code{deparse.level}.
#' }
#' }
#' 
#' @param ... a list of SpatialExperiment objects
#' @param deparse.level Refer to \code{?\link[base]{rbind}} 
#' 
#' @return a combined SpatialExperiment object
#'
#' @author
#' Dario Righelli
#'
#' @examples
#' example(read10xVisium, echo = FALSE)
#'
#' # merging with duplicated 'sample_id's
#' # will automatically make them unique
#' spe1 <- spe2 <- spe
#' spe3 <- cbind(spe1, spe2)
#' unique(spe3$sample_id)
#'
#' # make sample identifiers unique
#' spe1 <- spe2 <- spe
#' spe1$sample_id <- paste(spe1$sample_id, "sample1", sep = ".")
#' spe2$sample_id <- paste(spe2$sample_id, "sample2", sep = ".")
#' 
#' # combine into single object
#' spe <- cbind(spe1, spe2)
#' 
#' # view joint 'imgData'
#' imgData(spe)
#' 
#' # tabulate number of spots mapped to tissue
#' table(
#'   in_tissue = spe$in_tissue, 
#'   sample_id = spe$sample_id)
NULL

#' @rdname SpatialExperiment-combine
#' @importFrom BiocGenerics rbind cbind
setMethod("cbind", "SpatialExperiment", function(..., deparse.level=1) {

    old <- S4Vectors:::disableValidity()
    if (!isTRUE(old)) {
        S4Vectors:::disableValidity(TRUE)
        on.exit(S4Vectors:::disableValidity(old))
    }
    args <- list(...)
    
    # check that 'sample_id's are unique;
    # otherwise make them by appending 
    # '.n' where n = sample number
    sids <- unlist(lapply(args, function(.) unique(.$sample_id)))
    if (length(sids) != length(unique(sids))) {
        message(
            "'sample_id's are duplicated across",
            " 'SpatialExperiment' objects to cbind;",
            " appending sample indices.")
        idx <- c(0, cumsum(vapply(args, ncol, numeric(1))))
        for (i in seq_along(args)) {
            old <- args[[i]]$sample_id
            new <- paste(old, i, sep = ".")
            args[[i]]$sample_id <- new
        }
    }
    
    # bind SPEs
    out <- do.call(
        callNextMethod, 
        c(args, list(deparse.level=1)))
    
    # merge 'imgData' from multiple samples
    if (!is.null(imgData(args[[1]]))) { 
        newimgdata <- do.call(rbind, lapply(args, imgData))
        int_metadata(out)[names(int_metadata(out)) == "imgData"] <- NULL
        int_metadata(out)$imgData <- newimgdata
    } 
        
    return(out)
})
