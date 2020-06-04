#' @export
#' @rdname SpatialExperiment
#' @slot int_spcIdx integer. 
#' 
#' @importClassesFrom S4Vectors DataFrame
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
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
#' @docType class
#' @export
#' @importFrom SingleCellExperiment SingleCellExperiment
#' @examples
#' fishCoordFile <- system.file(file.path("extdata", "seqFISH",
#'                                "fcortex.coordinates.txt"), 
#'                                package="SpatialExperiment")
#' fishCoordinates <- read.table(fishCoordFile, header=FALSE, sep=" ")
#' colnames(fishCoordinates) <- c("ID", "Irrelevant", "x", "y")
#' 
#' fishCellLabsFile <- system.file(file.path("extdata", "seqFISH", 
#'                                          "seqfish_cell_labels.tsv"),
#'                             package="SpatialExperiment")
#' fishCellLabels <- read.table(file=fishCellLabsFile, header=FALSE, sep="\t")
#' colnames(fishCellLabels) <- c("ID", "cluster", "class", "classID", 
#'                               "Irrelevant", "Prob")
#' fishFeatCountsFile <- system.file(file.path("extdata", "seqFISH",
#'                              "seqfish_normalized_cortex_b2_testing.txt"), 
#'                              package="SpatialExperiment")
#' fishFeaturesCounts <- read.table(file=fishFeatCountsFile, 
#'                                  header=FALSE, sep="\t", row.names=1)
#'  se <- SpatialExperiment(rowData=rownames(fishFeaturesCounts),
#'                      colData=fishCellLabels,
#'                      assays=SimpleList(counts=as.matrix(fishFeaturesCounts)),
#'                      spatialCoords=fishCoordinates) 
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


#' @export
#' @rdname VisiumExperiment
#' @slot scaleFactors list
#' 
#' @importClassesFrom S4Vectors DataFrame

setClass("VisiumExperiment",
        slots=c(
            scaleFactors="list"
        ),
        contains = "SpatialExperiment"
)



#' The VisiumExperiment class
#'
#' The VisiumExperiment class is designed to represent 10x Visium spatial 
#' Gene Expression data.
#' It inherits from the \linkS4class{SpatialExperiment} class and is used in 
#' the same manner.
#' In addition, the class supports the integration with 10x Visium spatial 
#' coordinates and its scale factors.
#'
#' @param ... arguments to be passed to the \code{\link{SpatialExperiment}} 
#' constructor to fill the slots of the base class.
#' @param spatialCoords the 10x Visium spatial coordinates
#' @param scaleFactors the 10x Visium image scale factors
#' 
#' @author Dario Righelli
#' @docType class
#' @export
#' @examples
#' # TBD
VisiumExperiment <- function(..., spatialCoords=data.frame(), 
                             scaleFactors=list())
{
    args <- list(...)
    stopifnot(sum(c("rowData", "colData", "assays") %in% names(args)) == 3)
    se <- SpatialExperiment::SpatialExperiment(
        rowData=as(args$rowData, "DataFrame"),
        colData=as(args$colData, "DataFrame"),
        assays=args$assays)
    return(.se_to_ve(se, spatialCoords, scaleFactors=scaleFactors))
}


.se_to_ve <- function(se, spatialCoords, scaleFactors=list()) 
{
    ve <- new("VisiumExperiment", se)
    .Object <- checkVisiumSpatialCoords(ve, spatialCoords)
    .Object <- addScaleFactors(.Object, scaleFactors)
    return(.Object)
}


#' @exportMethod coerce
setAs("SpatialExperiment", "VisiumExperiment", function(from) 
{
    .se_to_ve(from)
})



