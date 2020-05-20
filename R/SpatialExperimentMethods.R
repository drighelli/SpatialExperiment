

#' checkSpatialCoords
#' @description checks if all the spatial parameters have the right fields.
#' @param SpatialExperiment a SpatialExperiment class object
#'
#' @return the SpatialExperiment class object passed as input.
#' @export
#'
#' @examples
#' TBD
setMethod(f="checkSpatialCoords",
          signature="SpatialExperiment",
          definition=function(se, spatialCoords=data.frame())
{
    stopifnot(is(se, "SpatialExperiment"))
    stopifnot(("ID" %in% colnames(colData(se))))# ||
                  #("Barcodes" %in% colnames(colData(se))))
    
    # stopifnot(nrow(colData(sce)) == nrow(spatialCoords))
    # stopifnot(sum(colData(sce)$Barcodes %in% spatialCoords$Barcodes) 
    #             == nrow(colData(sce)))
    # a different approach is possible, 
    # if they aren't equal
    # a partial match is possible
    # otherwhise (if 0 match) stop stopifnot(sum(colData(sce)$Barcodes %in% spatialCoords$Barcodes) 
    #             != 0))

    # stopifnot(("Barcodes" %in% colnames(spatialCoords)))
    # stopifnot(sum(c("in_tissue", "array_row", "array_col", 
    #                 "pxl_col_in_fullres", "pxl_row_in_fullres")
    #                 %in% colnames(spatialCoords)) == 5)
    
    if("ID" %in% colnames(colData(se)))
        cDataIdx <- match(colData(se)$ID, spatialCoords$ID)
    else
        cDataIdx <- match(colData(se)$Barcodes, spatialCoords$Barcodes)
    
    int_colData(se) <- as(cbind(spatialCoords[cDataIdx,], int_colData(se)), 
                            "DataFrame")
    se@int_spcIdx <- which(colnames(int_colData(se)) %in% colnames(spatialCoords))
    return(se)
})


#' spatialCoords-getter
#' @description a getter method which returns the spatial coordinates previously
#' stored in a SpatialExperiment class object.
#' @param x A SpatialExperiment class object.
#'
#' @return a DataFrame within the spatial coordinates.
#' @export
#'
#' @examples
#' TBD
setMethod(f="spatialCoords", signature="SpatialExperiment", function(x)
{
    return(int_colData(x)[, x@int_spcIdx])
})

#' spatialCoords-setter
#' @description a setter method which sets/replaces the spatial coordinate in a 
#' SpatialExperiment class object.
#' @param x a SpatialExperiment class object
#' @param value a DataFrame with the new spatial coordinates to set.
#'
#' @return
#' @export
#'
#' @examples
setReplaceMethod(f="spatialCoords", signature="SpatialExperiment", 
                function(x, value)
{
    stopifnot(("ID" %in% colnames(value)))
    
    cDataIdx <- match(value$ID, int_colData(x)$ID)
    
    for (col in colnames(value))
    {
        colidx <- which(colnames(int_colData(x)) == col)
        validx <- which(colnames(value) == col)
        int_colData(x)[cDataIdx, colidx] <- value[,validx]
    }
    
    return(x)
})


#' spatialCoordsNames-getter
#' @description getter method for the spatial coordinates names in a 
#' SpatialExperiment class object.
#' @param x a SpatialExperiment class object.
#'
#' @return a vector with the colnames of the spatial coordinates.
#' @export
#'
#' @examples
#' TBD
setMethod(f="spatialCoordsNames", signature="SpatialExperiment", function(x)
{
    return(colnames(int_colData(x)[x@int_spcIdx]))
})
