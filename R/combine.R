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

    ################################# keeping sample_id unique
    sampleids <- .createSampleIds(args)
    colData(out)$sample_id <- rep(names(sampleids), sampleids)
    
    ############################## creating new imgData
    newimgdata <- do.call(rbind, lapply(args, imgData))
    int_metadata(out)[names(int_metadata(out)) %in% "imgData"] <- NULL
    int_metadata(out)$imgData <- newimgdata
    imgids <- .getIdsTable(args, imgData)
    imgData(out)$sample_id <- rep(names(sampleids), imgids) 
    
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
    names(sampleids)[!dups] <- lapply(names(sampleids)[!dups], function(x) paste0(x, 0))
    i <- 1
    while( sum(dups) != 0 )
    {
        names(sampleids)[dups] <- lapply(names(sampleids)[dups], function(x) paste0(x, i)) 
        dups <- duplicated(names(sampleids))
        i <- i+1 
    }
    return(sampleids)
}
