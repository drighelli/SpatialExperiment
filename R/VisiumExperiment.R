#' #' The VisiumExperiment class
#' #'
#' #' The VisiumExperiment class is designed to represent 10x Visium spatial 
#' #' Gene Expression data.
#' #' It inherits from the \linkS4class{SpatialExperiment} class and is used in 
#' #' the same manner.
#' #' In addition, the class supports the integration with 10x Visium spatial 
#' #' coordinates and its scale factors.
#' #'
#' #' @param ... arguments to be passed to the \code{\link{SpatialExperiment}} 
#' #' constructor to fill the slots of the base class.
#' #' @param scaleFactors the 10x Visium image scale factors.
#' #' @param imagePaths the list of the paths for the 10x Visium images.
#' #' @return none
#' #' @aliases
#' #' coerce,SpatialExperiment,VisiumExperiment-method
#' #' @importFrom methods new
#' #' @importClassesFrom S4Vectors DataFrame
#' #' @author Dario Righelli
#' #' @docType class
#' #' @export
#' #' @examples
#' #' 
#' #' barcodesFile <- system.file(file.path("extdata", "10x_visium",
#' #'                         "barcodes.tsv"),
#' #'                         package="SpatialExperiment")
#' #' barcodesEx <- read.csv(barcodesFile, sep="\t",
#' #'                         header=FALSE, col.names=c("Barcodes"))
#' #' featuresFile <- system.file(file.path("extdata", "10x_visium",
#' #'                         "features.tsv"), package="SpatialExperiment")
#' #' featuresEx <- read.csv(featuresFile, sep="\t",
#' #'                         header=FALSE, col.names=c("Barcodes", "Feature_name",
#' #'                         "Feature_type"))
#' #' countsFile <- system.file(file.path("extdata", "10x_visium",
#' #'                         "matrix.mtx"), package="SpatialExperiment")
#' #' countsEx <- Matrix::readMM(file=countsFile)
#' #' posFile <- system.file(file.path("extdata", "10x_visium",
#' #'                         "tissue_positions_list.tsv"),
#' #'                         package="SpatialExperiment")
#' #' tissPosEx <- read.csv(posFile,
#' #'                         sep="\t", header=FALSE,
#' #'                         col.names=c("Barcodes", "in_tissue",
#' #'                          "array_row", "array_col",
#' #'                          "pxl_col_in_fullres", "pxl_row_in_fullres"))
#' #' scaleFile <- system.file(file.path("extdata", "10x_visium",
#' #'                                    "scalefactors_json.json"), 
#' #'                          package="SpatialExperiment")
#' #' 
#' #' scalefactors <- rjson::fromJSON(file=scaleFile)
#' #' imagePaths <- list.files(system.file(file.path("extdata", "10x_visium",
#' #'                          "images"), 
#' #'                          package="SpatialExperiment"), full.names=TRUE)
#' #'                          
#' #' ve <- VisiumExperiment(rowData=featuresEx, colData=barcodesEx,
#' #'                          assays=c(counts=countsEx),
#' #'                          spatialCoords=tissPosEx,
#' #'                          scaleFactors=scalefactors, 
#' #'                          imagePaths=imagePaths)
#' #' 
#' #' ve
#' VisiumExperiment <- function(..., scaleFactors=list(), imagePaths=list())
#' {
#'     se <- SpatialExperiment::SpatialExperiment(...)
#'     return(.se_to_ve(se, scaleFactors=scaleFactors, imagePaths=imagePaths))
#' }
#' 
#' #' @importFrom methods new
#' .se_to_ve <- function(se, scaleFactors=S4Vectors::SimpleList(), 
#'     imagePaths=S4Vectors::SimpleList()) 
#' {
#'     old <- S4Vectors:::disableValidity()
#'     if (!isTRUE(old)) {
#'         S4Vectors:::disableValidity(TRUE)
#'         on.exit(S4Vectors:::disableValidity(old))
#'     }
#'     ve <- new("VisiumExperiment", se)
#'     if(length(scaleFactors)>0) scaleFactors(ve) <- scaleFactors
#'     if(length(imagePaths)>0) imagePaths(ve) <- imagePaths
#'     return(ve)
#' }
#' 
#' #' @exportMethod coerce
#' setAs("SpatialExperiment", "VisiumExperiment", function(from) 
#' {
#'     .se_to_ve(from)
#' })
#' 
#' # TODO:
#' # for backward compatibility, the following (temporary) wrapper 
#' # converts a 'VisiumExperiment' to the new 'SpatialExperiment'
#' .ve_to_spe <- function(x) {
#'     
#' }