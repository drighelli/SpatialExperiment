
setMethod(f="addScaleFactors",
          signature="VisiumExperiment",
          definition=function(ve, scaleFactors=list())
          {
              stopifnot(is(ve, "VisiumExperiment"))
              stopifnot(is(scaleFactors, "list"))
              stopifnot(sum(c("spot_diameter_fullres", "tissue_hires_scalef", 
                              "fiducial_diameter_fullres", "tissue_lowres_scalef")
                            %in% names(scaleFactors)) == 4)
              scaleFactors(ve) <- scaleFactors
              return(ve)
          }
)

setMethod(f="checkSpatialCoords",
          signature="VisiumExperiment",
          definition=function(ve, spatialCoords)
{
    stopifnot(is(ve, "VisiumExperiment"))
    stopifnot(("Barcodes" %in% colnames(colData(ve))))
    stopifnot(("Barcodes" %in% colnames(spatialCoords)))
    # stopifnot(nrow(colData(sce)) == nrow(spatialCoords))
    # stopifnot(sum(colData(sce)$Barcodes %in% spatialCoords$Barcodes) 
    #             == nrow(colData(sce)))
    # a different approach is possible, 
    # if they aren't equal
    # a partial match is possible
    # otherwhise (if 0 match) stop stopifnot(sum(colData(sce)$Barcodes %in% spatialCoords$Barcodes) 
    #             != 0))
    stopifnot(sum(c("in_tissue", "array_row", "array_col", 
                    "pxl_col_in_fullres", "pxl_row_in_fullres")
                    %in% colnames(spatialCoords)) == 5)
      
    cDataIdx <- match(colData(ve)$Barcodes, spatialCoords$Barcodes)
    colData(ve) <- cbind(spatialCoords[cDataIdx, 
                            -c(which(colnames(spatialCoords)=="Barcodes"))],
                            colData(ve))
    idx <- which(colnames(colData(ve)) %in% c("in_tissue", 
                                    "array_row", "array_col", 
                                    "pxl_col_in_fullres", "pxl_row_in_fullres"))
    ve@int_spcIdx <- idx
    return(ve)
})

### Getters/Setters

setMethod(f="scaleFactors", signature="VisiumExperiment", function(x)
{
    return(x@scaleFactors)
})


setReplaceMethod(f="scaleFactors", signature="VisiumExperiment", 
                function(x, value)
{
    x@scaleFactors <- value
    return(x)
})

setMethod(f="spatialCoords", signature="VisiumExperiment", function(x)
{
    return(colData(x)[, x@int_spcIdx])
})

setReplaceMethod(f="spatialCoords", signature="VisiumExperiment", 
                function(x, value)
{
    stopifnot(("Barcodes" %in% colnames(value)))
    stopifnot(sum(c("in_tissue", "array_row", "array_col", 
                "pxl_col_in_fullres", "pxl_row_in_fullres")
                %in% colnames(value)) == 5)
    
    cDataIdx <- match(value$Barcodes, colData(x)$Barcodes)
    
    for (col in c("in_tissue", "array_row", "array_col", 
                "pxl_col_in_fullres", "pxl_row_in_fullres"))
    {
        colidx <- which(colnames(colData(x)) == col)
        validx <- which(colnames(value) == col)
        colData(x)[cDataIdx, colidx] <- value[,validx]
    }
    
    return(x)
})

setMethod(f="isInTissue", signature="VisiumExperiment", function(x)
{
  return( (colData(x)$in_tissue == 1) )
})
