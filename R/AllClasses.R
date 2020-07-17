#' @rdname SpatialExperiment
#' @slot int_cellID character.
#' @slot int_spcIdx integer. 
#' 
#' @importClassesFrom S4Vectors DataFrame
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
setClass("SpatialExperiment",
        slots=c(
            int_cellID="character",
            int_spcIdx="integer"
        ),
        contains = "SingleCellExperiment"
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
#' @param spatialCoords the spatial coordinates 
#' @param cellColID the name of the spatialCoords column where the 
#' cell identifiers are stored (default is "Cell_ID").
#' @return none
#' @author Dario Righelli
#' @docType class
#' @aliases
#' coerce,SingleCellExperiment,SpatialExperiment-method 
#' @export
#' @importClassesFrom S4Vectors DataFrame
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @importFrom SingleCellExperiment SingleCellExperiment
#' @examples
#' ## building random seqFISH data coordinates
#' fishCoordinates <- data.frame(Cell_ID=paste0("cell",c(1:30)),
#'                 Irrelevant=100,
#'                 x=sample(c(-4000:4000), size=30, replace=TRUE),
#'                 y=sample(c(-4000:4000), size=30, replace=TRUE))
#' ## building random seqFISH cell labels
#' fishCellLabels <- data.frame(Cell_ID=paste0("cell",c(1:30)),
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
#' ##################
#' ## Otherwise, it's possible to provide user-defined cell identifier column,
#' ## by using the cellColID argument
#' fishCoordinates <- data.frame(MyCell_ID=paste0("cell",c(1:30)), 
#'                 Irrelevant=100, 
#'                 x=sample(c(-4000:4000), size=30, replace=TRUE),
#'                 y=sample(c(-4000:4000), size=30, replace=TRUE))
#'                 
#' fishCellLabels <- data.frame(MyCell_ID=paste0("cell",c(1:30)), 
#'                              class="neuron", 
#'                              classID=sample(c(0:5), size=30, replace=TRUE))
#' se <- SpatialExperiment(rowData=rownames(fishCounts),
#'                         colData=fishCellLabels,
#'                         assays=SimpleList(counts=as.matrix(fishCounts)),
#'                         spatialCoords=fishCoordinates,
#'                         cellColID="MyCell_ID")
#'                         
SpatialExperiment <- function(..., spatialCoords=data.frame(), 
                             cellColID="Cell_ID")
{
    sce <- SingleCellExperiment::SingleCellExperiment(...)
    return(.sce_to_se(sce, spatialCoords=spatialCoords, cellColID=cellColID))
}

#' @importClassesFrom S4Vectors DataFrame 
#' @importFrom S4Vectors DataFrame isEmpty 
#' @importFrom methods new
.sce_to_se <- function(sce, spatialCoords=DataFrame(), cellColID="ID") 
{
    old <- S4Vectors:::disableValidity()
    if (!isTRUE(old)) {
        S4Vectors:::disableValidity(TRUE)
        on.exit(S4Vectors:::disableValidity(old))
    }
    se <- new("SpatialExperiment", sce) 
    ## here it calls the validity ?
    se@int_cellID=cellColID
    if(!sum(S4Vectors::isEmpty(spatialCoords))) 
        spatialCoords(se) <- spatialCoords
    return(se)
}

#' @exportMethod coerce
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @import SingleCellExperiment
setAs(from="SingleCellExperiment", to="SpatialExperiment", function(from) 
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
#' @param scaleFactors the 10x Visium image scale factors.
#' @return none
#' @aliases
#' coerce,SpatialExperiment,VisiumExperiment-method
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
#'                          scaleFactors=scalefactors, cellColID="Barcodes")
#' 
#' ve
VisiumExperiment <- function(..., scaleFactors=list())
{
    se <- SpatialExperiment::SpatialExperiment(...)
    return(.se_to_ve(se, scaleFactors=scaleFactors))
}

#' @importFrom methods new
.se_to_ve <- function(se, scaleFactors=list()) 
{
    old <- S4Vectors:::disableValidity()
    if (!isTRUE(old)) {
        S4Vectors:::disableValidity(TRUE)
        on.exit(S4Vectors:::disableValidity(old))
    }
    ve <- new("VisiumExperiment", se)
    if(length(scaleFactors)>0) scaleFactors(ve) <- scaleFactors
    return(ve)
}

#' @exportMethod coerce
setAs("SpatialExperiment", "VisiumExperiment", function(from) 
{
    .se_to_ve(from)
})



