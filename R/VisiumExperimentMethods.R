
#' addScaleFactors
#' @description sets the scale factors given as input to the VisiumExperiment
#' class object.
#' @param ve a VisiumExperiment class object.
#' @param scaleFactors a list of 10x Visium scale factors.-
#'
#' @return a VisiumExperiment class object.
#' @keywords internal
#' @aliases addScaleFactors
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


#' checkVisiumSpatialCoords
#' @description checks if the VisiumExperiment class object has all the required
#' fields for the integrity of the spatial coordinates and sets them 
#' to the object.
#' @param ve a VisiumExperiment class object.
#' @param spatialCoords a DataFrame with visium spatial coordinates
#'
#' @return a VisiumExperiment class object.
#' @keywords internal
#' @aliases checkVisiumSpatialCoords
setMethod(f="checkVisiumSpatialCoords",
          signature="VisiumExperiment",
          definition=function(ve, spatialCoords=DataFrame()) #data.frame())
{
    stopifnot(is(ve, "VisiumExperiment"))
    stopifnot(("Barcodes" %in% colnames(colData(ve))))

    stopifnot(("Barcodes" %in% colnames(spatialCoords)))
    stopifnot(sum(c("in_tissue", "array_row", "array_col",
                  "pxl_col_in_fullres", "pxl_row_in_fullres")
                    %in% colnames(spatialCoords)) == 5)
    if(class(spatialCoords) == "data.frame")
    {
        spatialCoords <- as(spatialCoords, "DataFrame")
    }
    cDataIdx <- match(colData(ve)$Barcodes, spatialCoords$Barcodes)
  
    int_colData(ve) <- cbind(spatialCoords[cDataIdx,], int_colData(ve))
    ve@int_spcIdx <- base::which(colnames(int_colData(ve)) %in% 
                        c("in_tissue", "array_row", "array_col", 
                        "pxl_col_in_fullres", "pxl_row_in_fullres"))
    return(ve)
})

### Getters/Setters
#' scaleFactors-getter
#' @description gets the scale factors from a VisiumExperiment class object.
#' @param x a VisiumExperiment class object. 
#'
#' @return a DataFrame with the 10x Visium scale factors.
#' @export
#' @aliases scaleFactors
#' @examples
#' ve <- readRDS(file=system.file(file.path("extdata", "10x_visium",
#'                          "ve.RDS"), package="SpatialExperiment"))
# scaleFactors(ve)
setMethod(f="scaleFactors", signature="VisiumExperiment", function(x)
{
    return(x@scaleFactors)
})

#' scaleFactors-setter
#' @description sets the scale factors in a VisiumExperiment class object.
#' @param x a VisiumExperiment class object.
#' @param value a list of 10x Visium scale factors.
#'
#' @return a VisiumExperiment class object.
#' @export
#' @aliases scaleFactors<-
#' @examples
#' 
#' ve <- readRDS(file=system.file(file.path("extdata", "10x_visium",
#'                          "ve.RDS"), package="SpatialExperiment"))
# scaleFactors(ve)
# newscFactors <- list("spot_diameter_full_res"=100,
#                      "tissue_hires_scalef"=0.5,
#                      "fiducial_diameter_fullres"=144.49,
#                      "tissue_lowres_scalef"=0.05)
# scaleFactors(ve) <- newscFactors
# scaleFactors(ve)
setReplaceMethod(f="scaleFactors", signature="VisiumExperiment",
                function(x, value)
{
    x@scaleFactors <- value
    return(x)
})


#' spatialCoords-getter
#' @description gets the spatial coordinate from a VisiumExperiment class object
#' @param x a  VisiumExperiment class object.
#'
#' @return a DataFrame with the spatial coordinates.
#' @export
#' @aliases spatialCoords
#' @examples
#' ve <- readRDS(file=system.file(file.path("extdata", "10x_visium",
#'                          "ve.RDS"), package="SpatialExperiment"))
#' spatialCoords(ve)
setMethod(f="spatialCoords", signature="VisiumExperiment", function(x)
{
    idx1 <- base::which(colnames(int_colData(x)) == "Barcodes")
    return(int_colData(x)[, c(idx1, x@int_spcIdx)])
})

#' spatialCoords-setter
#' @description sets the spatial coordinate to a VisiumExperiment class object.
#' @param x a VisiumExperiment class object.
#' @param value a DataFrame of spatial coordinates
#'
#' @export
#' @aliases spatialCoords<-
#' @examples
#' ve <- readRDS(file=system.file(file.path("extdata", "10x_visium",
#'                          "ve.RDS"), package="SpatialExperiment"))
#' newSpCord <- spatialCoords(ve)
#' newSpCord$in_tissue = 0
#' spatialCoords(ve) <- newSpCord
setReplaceMethod(f="spatialCoords", signature="VisiumExperiment", 
                 function(x, value)
{
    stopifnot(("Barcodes" %in% colnames(value)))
    
    cDataIdx <- match(value$Barcodes, int_colData(x)$Barcodes)
    
    for (col in colnames(value))
    {
        colidx <- base::which(colnames(int_colData(x)) == col)
        validx <- base::which(colnames(value) == col)
        int_colData(x)[cDataIdx, colidx] <- value[,validx]
    }

    return(x)
})

#' isInTissue
#' @description returns a mask of TRUE/FALSE Barcodes spots, indicating which 
#' ones are in tissue and which ones are not.
#' @param x  a VisiumExperiment class object.
#'
#' @return a TRUE/FALSE mask.
#' @export
#' @aliases isInTissue
#' @examples
#' ve <- readRDS(file=system.file(file.path("extdata", "10x_visium",
#'                          "ve.RDS"), package="SpatialExperiment"))
#' isInTissue(ve)
#' sum(isInTissue(ve))
setMethod(f="isInTissue", signature="VisiumExperiment", function(x)
{
    return( (int_colData(x)$in_tissue == 1) )
})

