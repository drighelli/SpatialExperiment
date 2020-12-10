#' @title Combining SpatialExperiment objects
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
#' \describe{
#' \item{\code{cbind(..., deparse.level=1)}:}{
#' Returns a SpatialExperiment where
#' all objects in \code{...} are combined column-wise,
#' i.e., columns in successive objects are appended to the first object.
#' 
#' Each \code{SpatialExperiment} object in \code{...} must have the same 
#' \code{colData} (with same \code{spatialCoordinates}).
#' In case of equal sample_id the method will proceed them providing new 
#' unique ones.
#' Additionally the \code{imgData} across all the object are \code{rbind}ed. 
#'
#' Refer to \code{?"\link{cbind,SingleCellExperiment-method}"} for details on 
#' how metadata and others inherited attributes are combined in the output 
#' object.
#' Refer to \code{?\link[base]{cbind}} for the interpretation of \code{deparse.level}.
#' }
#' }
#'
#' @author
#' Dario Righelli
#'
#' @examples
#' example(SpatialExperiment, echo=FALSE) # using the class example
#'
#' # Combining:
#' se1 <- se
#' se1$sample_id <- "foo1"
#' cbind(se, se1)
#'
#' @docType methods
#' @aliases
#' cbind,SingleCellExperiment-method
#'
#' @name SPE-combine
#' @rdname combine
NULL

#' @importFrom BiocGenerics rbind cbind
setMethod("cbind", "SpatialExperiment", function(..., deparse.level=1) 
{
    old <- S4Vectors:::disableValidity()
    if (!isTRUE(old)) {
        S4Vectors:::disableValidity(TRUE)
        on.exit(S4Vectors:::disableValidity(old))
    }
    out <- callNextMethod()
    args <- list(...)
    save(out, file="out.rda")
    ################################# keeping sample_id unique
    sampleids <- .createSampleIds(args)
    colData(out)$sample_id <- rep(names(sampleids), times=sampleids)
    
    ############################## creating new imgData
    if(!is.null(imgData(args[[1]]))){ ## handle imgData across multiple samples
        newimgdata <- do.call(rbind, lapply(args, imgData))
        int_metadata(out)[names(int_metadata(out)) %in% "imgData"] <- NULL
        int_metadata(out)$imgData <- newimgdata
        imgids <- .getIdsTable(args, imgData)
        imgData(out)$sample_id <- rep(names(sampleids), imgids) 
    } 
    return(out)
})

.getIdsTable <- function(args, speFUN)
{
    idsTab <- unlist(lapply(args, function(spE)
    {
        sids <- table(speFUN(spE)$sample_id)
        sids <- sids[order(unique(speFUN(spE)$sample_id))]
        return(sids)
    }))
    return(idsTab)
}

.createSampleIds <- function(args)
{
    sampleids <- .getIdsTable(args, colData)
    dups <- duplicated(names(sampleids))
    # names(sampleids)[!dups] <- lapply(names(sampleids)[!dups], function(x) paste0(x, 0))
    i <- 1
    while( sum(dups) != 0 )
    {
        names(sampleids)[dups] <- lapply(names(sampleids)[dups], function(x) paste0(x, i)) 
        dups <- duplicated(names(sampleids))
        i <- i+1 
    }
    return(sampleids)
}
