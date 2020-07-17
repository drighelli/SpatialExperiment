
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
#' example(VisiumExperiment)
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
    stopifnot(is(x, "VisiumExperiment"))
    stopifnot(is(value, "list"))
    stopifnot(sum(c("spot_diameter_fullres", "tissue_hires_scalef",
                    "fiducial_diameter_fullres", "tissue_lowres_scalef")
                    %in% names(value)) == 4)
    x@scaleFactors <- value
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
    return( (int_colData(x)$spatial$in_tissue == 1) )
})

