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
    samplesids <- unlist(lapply(args, function(spE) return(unique(colData(spE)$sample_id))))
    dsid <- duplicated(samplesids)
    samplesids <- paste0(samplesids, c(1:length(dsid)))
    
    
    
    # stopifnot(any())
    # 
    # 
    # 
    # 
    # 
    # BiocGenerics:::replaceSlots(out)#, int_metadata=int_m, check=FALSE)
    out
})

