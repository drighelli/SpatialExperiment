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
    # stopifnot(any(isClass("SpatialExperiment", args)))

    ################################# keeping sample_id unique
    ## to change with something:
    ## faster 
    ## avoid ids like 12,123,1234
    samplesids <- unlist(lapply(args, function(spE)
    {
        return(table(colData(spE)$sample_id))
    }))
    dups <- duplicated(names(samplesids))
    names(samplesids)[!dups] <- lapply(names(samplesids)[!dups], function(x) paste0(x, 0))
    i <- 1
    while( sum(dups) != 0 )
    {
        names(samplesids)[dups] <- lapply(names(samplesids)[dups], function(x) paste0(x, i)) 
        dups <- duplicated(names(samplesids))
        i <- i+1 
        
    }
    colData(out)$sample_id <- rep(names(samplesids), samplesids)
    
    ############################## creating new imgData
    newimgdata <- do.call(rbind, lapply(args, imgData))
    int_metadata(out)[names(int_metadata(out)) %in% "imgData"] <- NULL
    int_metadata(out)$imgData <- newimgdata
    
    
    return(out)
})

