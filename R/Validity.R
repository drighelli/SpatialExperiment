.spe_validity <- function(object)
{
    msg <- NULL
    
    if( length(msg) ) { return(msg) }
    
    return(TRUE)
}

#' @importFrom S4Vectors setValidity2
setValidity2("SpatialExperiment", .spe_validity)


.ve_validity <- function(object)
{
    msg <- NULL
    if( sum(c("in_tissue", "array_row", "array_col",
            "pxl_col_in_fullres", "pxl_row_in_fullres")
            %in% colnames(spatialCoords(object))) != 5 ) 
    {
        msg <- c(msg, paste0("Please use the 10x Visium colnames for the",
            " spatial coordinates. (Defaults are 'in_tissue, 'array_row'", 
            " 'array_col', 'pxl_col_in_fullres', 'pxl_row_in_fullres')"))
    }
    
    if( sum(c("spot_diameter_fullres", "tissue_hires_scalef",
             "fiducial_diameter_fullres", "tissue_lowres_scalef")
           %in% names(scaleFactors(object))) != 4 )
    {
        msg <- c(msg, paste0("Please use the 10x Visium names for the",
            " scale factors. (Required are 'spot_diameter_fullres', 
            ' tissue_hires_scalef', 'fiducial_diameter_fullres', 
            ' tissue_lowres_scalef')"))
    }
    
    if(length(msg)) { return(msg) }
    
    return(TRUE)
}

#' @importFrom S4Vectors setValidity2
setValidity2("VisiumExperiment", .ve_validity)

