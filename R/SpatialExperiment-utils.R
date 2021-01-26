
.setCoord <- function(df, coordName, coordinates)
{
    stopifnot(dim(df)[1] == length(coordinates))
    df[[coordName]] <- coordinates
    return(df)
}

#' @importFrom rjson fromJSON
.loadScaleFacts <- function(scaleFactors, imgbasename=NULL)
{
    scf <- scaleFactors
    if(!(is.numeric(scf) || is.list(scf))) 
    {
        scf <- fromJSON(file=scaleFactors)
    }
    
    if(! ( is.numeric(scf) || is.null(imgbasename)) )
    {
        if( !isEmpty(grep("lowres", imgbasename)) ) 
        {
            scf <- scf$tissue_lowres_scalef
        } else if( !isEmpty(grep("hires", imgbasename)) ) {
            scf <- scf$tissue_lowres_scalef
        }
    }
    
    return(scf)
}
