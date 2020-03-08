#' @export
#' @rdname SpatialExperiment
#' @slot scaleFactors list. 
#' @slot int_spcIdx integer. 
#' 
#' @importClassesFrom S4Vectors DataFrame
#' @importClasses SingleCellExperiment
#'
#' @examples
setClass("SpatialExperiment",
         slots=c(
             int_spcIdx="integer"
         ),
         contains = "SingleCellExperiment"#,
)


#' The SpatialExperiment class
#'
#' The SpatialExperiment class is designed to represent 10x Visium spatial 
#' Gene Expression data.
#' It inherits from the \linkS4class{SingleCellExperiment} class and is used in 
#' the same manner.
#' In addition, the class supports the integration with 10x Visium spatial 
#' coordinates and its scale factors.
#'
#' @param ... arguments to be passed to the \code{\link{SingleCellExperiment}} 
#' constructor to fill the slots of the base class.
#' @param spatialCoords the 10x Visium spatial coordinates
#' 
#' @author Dario Righelli
#' @examples
#' TBD
#' @docType class
#' @export
#' @importFrom SingleCellExperiment SingleCellExperiment
#' 
SpatialExperiment <- function(..., spatialCoords=data.frame())
{
    args <- list(...)
    stopifnot(sum(c("rowData", "colData", "assays") %in% names(args)) == 3)
 
    sce <- SingleCellExperiment::SingleCellExperiment(
        rowData=as(args$rowData, "DataFrame"),
        colData=as(args$colData, "DataFrame"),
        assays=args$assays)
    return(.sce_to_se(sce, spatialCoords=spatialCoords))
}


.sce_to_se <- function(sce, spatialCoords=data.frame()) 
{
    se <- new("SpatialExperiment", sce)
    
    .Object <- checkSpatialCoords(se, spatialCoords)
    return(.Object)
}


#' @exportMethod coerce
#' @importClassesFrom SingleCellExperiment SingleCellExperiment 
setAs("SingleCellExperiment", "SpatialExperiment", function(from) 
{
    .sce_to_se(from)
})





