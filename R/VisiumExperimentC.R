#' @export
#' @rdname VisiumExperiment
#' @slot scaleFactors list. 
#' @slot int_spcIdx integer. 
#' 
#' @importClassesFrom S4Vectors DataFrame
#' @importClasses SingleCellExperiment
#'
#' @examples
setClass("VisiumExperiment",
         slots=c(
             scaleFactors="list",
             int_spcIdx="integer"
         ),
         contains = "SingleCellExperiment"#,
         # prototype = prototype(
         #     int_metadata=list(
         #         version=packageVersion("VisiumExperiment")
         #     )
         # )
)


#' The VisiumExperiment class
#'
#' The VisiumExperiment class is designed to represent 10x Visium spatial 
#' Gene Expression data.
#' It inherits from the \linkS4class{SingleCellExperiment} class and is used in 
#' the same manner.
#' In addition, the class supports the integration with 10x Visium spatial 
#' coordinates and its scale factors.
#'
#' @param ... arguments to be passed to the \code{\link{SingleCellExperiment}} 
#' constructor to fill the slots of the base class.
#' @param spatialCoords the 10x Visium spatial coordinates
#' @param scaleFactors the 10x Visium image scale factors
#' 
#' @author Dario Righelli
#' @examples
#' TBD
#' @docType class
#' @export
#' @importFrom SingleCellExperiment SingleCellExperiment
#' 
VisiumExperiment <- function(..., spatialCoords=data.frame(), 
                             scaleFactors=list())
{
    args <- list(...)
    stopifnot(sum(c("rowData", "colData", "assays") %in% names(args)) == 3)
    sce <- SingleCellExperiment::SingleCellExperiment(
        rowData=as(args$rowData, "DataFrame"),
        colData=as(args$colData, "DataFrame"),
        assays=args$assays)
    return(.sce_to_ve(sce, spatialCoords=spatialCoords, 
                        scaleFactors=scaleFactors))
}


.sce_to_ve <- function(sce, spatialCoords=data.frame(), scaleFactors=list()) 
{
    ve <- new("VisiumExperiment", sce)
    
    .Object <- checkSpatialCoords(ve, spatialCoords)
    .Object <- addScaleFactors(.Object, scaleFactors)
    return(.Object)
}


#' @exportMethod coerce
#' @importClassesFrom SingleCellExperiment SingleCellExperiment 
setAs("SingleCellExperiment", "VisiumExperiment", function(from) 
{
    .sce_to_ve(from)
})





