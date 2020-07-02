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
#' @aliases
#' coerce,SingleCellExperiment,SpatialExperiment-method
#' @export
#' @importClassesFrom S4Vectors DataFrame
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @examples
#' ## building random seqFISH data coordinates
#' fishCoords <- data.frame(ID=paste0("cell",c(1:30)), 
#'                 Irrelevant=100, 
#'                 x=sample(c(-4000:4000), size=30, replace=TRUE),
#'                 y=sample(c(-4000:4000), size=30, replace=TRUE))
#' ## building random seqFISH cell labels 
#' fishCellLabels <- data.frame(ID=paste0("cell",c(1:30)), 
#'                              class="neuron", 
#'                              classID=sample(c(0:5), size=30, replace=TRUE))
#' ## building random seqFISH count matrix
#' fishCounts <- matrix(sample(0:100, size=(30*30), replace=TRUE),
#'                      nrow=30, ncol=30,
#'                      dimnames=list(paste0("gene",c(1:30)), 
#'                                    paste0("cell",c(1:30))))
#' ## creating SpatialExperiment object
#' se <- SpatialExperiment(rowData=rownames(fishCounts),
#'                         colData=fishCellLabels,
#'                         assays=SimpleList(counts=as.matrix(fishCounts)),
#'                         spatialCoords=fishCoordinates)
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

#' coerce
#' @description Converts a SingleCellExperiment into a SpatialExperiment
#' @param sce a SingleCellExperiment object instance
#' @importFrom S4Vectors DataFrame
#' @importFrom methods new
.sce_to_se <- function(sce, spatialCoords=DataFrame()) 
{
    se <- new("SpatialExperiment", sce)
    spatialCoords(se) <- spatialCoords
    # .Object <- checkSpatialCoords(se, spatialCoords)
    return(se)
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
#' @importFrom methods new
#' @importClassesFrom S4Vectors DataFrame
#' @author Dario Righelli
#' @docType class
#' @export
#' @examples
#' 
#' barcodesFile <- system.file(file.path("extdata", "10x_visium",
#'                         "barcodes.tsv"),
#'                         package="SpatialExperiment")
#' barcodesEx <- read.csv(barcodesFile, sep="\t",
#'                         header=FALSE, col.names=c("Barcodes"))
#' featuresFile <- system.file(file.path("extdata", "10x_visium",
#'                         "features.tsv"), package="SpatialExperiment")
#' featuresEx <- read.csv(featuresFile, sep="\t",
#'                         header=FALSE, col.names=c("Barcodes", "Feature_name",
#'                         "Feature_type"))
#' countsFile <- system.file(file.path("extdata", "10x_visium",
#'                         "matrix.mtx"), package="SpatialExperiment")
#' countsEx <- Matrix::readMM(file=countsFile)
#' posFile <- system.file(file.path("extdata", "10x_visium",
#'                         "tissue_positions_list.tsv"),
#'                         package="SpatialExperiment")
#' tissPosEx <- read.csv(posFile,
#'                         sep="\t", header=FALSE,
#'                         col.names=c("Barcodes", "in_tissue",
#'                          "array_row", "array_col",
#'                          "pxl_col_in_fullres", "pxl_row_in_fullres"))
#' scaleFile <- system.file(file.path("extdata", "10x_visium",
#'                                    "scalefactors_json.json"), 
#'                          package="SpatialExperiment")
#' 
#' scalefactors <- rjson::fromJSON(file=scaleFile)
#' ve <- VisiumExperiment(rowData=featuresEx, colData=barcodesEx,
#'                          assays=c(counts=countsEx),
#'                          spatialCoords=tissPosEx,
#'                          scaleFactors=scalefactors)
#' 
#' ve
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

#' coerce
#' @description Converts a SpatialExperiment to a VisiumExperiment
#' @importFrom methods new
.se_to_ve <- function(se, spatialCoords, scaleFactors=NULL) 
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



