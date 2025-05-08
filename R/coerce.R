#' @name SpatialExperiment-coercion
#' @title SpatialExperiment coercion methods
#' 
#' @description 
#' The \code{SpatialExperiment} class inherits from the
#' \code{SingleCellExperiment} class making it necessary to coerce between these
#' classes.
#' To do so, we designed two different methods: the traditional \code{as} method
#' and the \code{toSpatialExperiment} function (recommended).
#' The \code{as} method checks if the \code{SingleCellExperiment} object has
#' already populated \code{int_colData} with two elements:
#' \code{spatialCoords} and \code{imgData}.
#' It also checks if \code{colData} already contains a \code{sample_id}.
#' In case these checks pass the new \code{SpatialExperiment} will have the same
#' values as the \code{SingleCellExperiment} passed object.
#' Otherwise a \code{SpatialExperiment} with default values for these slots
#' will be created.
#' 
#' The \code{toSpatialExperiment} method expects a \code{SingleCellExperiment} 
#' object and additional arguments as explained in the related section of this
#' documentation. In case the \code{SingleCellExperiment} object has already
#' populated \code{int_colData} with \code{spatialCoords} and/or \code{imgData}, 
#' these will be respectively overwritten in case the arguments 
#' \code{spatialCoords/Names} and/or \code{imgData} are not \code{NULL}.
#' 
#' @param sce A \code{\link{SingleCellExperiment}} object.
#' @param sample_id A \code{character} sample identifier, which matches the
#'   \code{sample_id} in \code{\link{imgData}}. The \code{sample_id} will also
#'   be stored in a new column in \code{\link{colData}}, if not already present.
#'   Default = \code{sample01}.
#' @param spatialCoordsNames A \code{character} vector of column names from
#'   \code{\link{colData}} containing spatial coordinates, which will be
#'   accessible with \code{\link{spatialCoords}}. Alternatively, the
#'   \code{spatialCoords} argument may be provided. If both are provided,
#'   \code{spatialCoordsNames} is given precedence, and a warning is returned.
#'   Default = \code{c("x", "y")}.
#' @param spatialCoords A numeric matrix containing columns of spatial
#'   coordinates, which will be accessible with \code{\link{spatialCoords}}.
#'   Alternatively, \code{spatialCoordsNames} may be provided. If both are
#'   provided, \code{spatialCoordsNames} is given precedence, and a warning is
#'   returned.
#' @param scaleFactors Optional scale factors associated with the image(s). This
#'   can be provided as a numeric value, numeric vector, list, or file path to a
#'   JSON file for the 10x Genomics Visium platform. For 10x Genomics Visium,
#'   the correct scale factor will automatically be selected depending on the
#'   resolution of the image from \code{imageSources}. Default = \code{1}.
#' @param imgData Optional \code{\link{DataFrame}} containing the image data.
#'   Alternatively, this can be built from the arguments \code{imageSources} and
#'   \code{image_id} (see Details).
#' @param imageSources Optional file path(s) or URL(s) for one or more image
#'   sources.
#' @param image_id Optional character vector (same length as
#'   \code{imageSources}) containing unique image identifiers.
#' @param loadImage Logical indicating whether to load image into memory.
#'   Default = \code{FALSE}.

#' @aliases 
#' coerce, SingleCellExperiment, SpatialExperiment-method
#' toSpatialExperiment
#' 
#' @examples 
#' dir <- system.file(
#'     file.path("extdata", "10xVisium", "section1", "outs"),
#'     package = "SpatialExperiment")
#' 
#' # read in counts
#' fnm <- file.path(dir, "raw_feature_bc_matrix")
#' sce <- DropletUtils::read10xCounts(fnm)
#' 
#' # read in spatial coordinates
#' fnm <- file.path(dir, "spatial", "tissue_positions_list.csv")
#' xyz <- read.csv(fnm, header = FALSE,
#'     col.names = c("barcode", "in_tissue", "array_row", "array_col",
#'     "pxl_row_in_fullres", "pxl_col_in_fullres"))
#' 
#' # read in image data
#' img <- readImgData(
#'     path = file.path(dir, "spatial"),
#'     sample_id = "sample01")
#' 
#' ## as method
#' (spe <- as(sce, "SpatialExperiment"))
#' 
#' colData(sce) <- DataFrame(xyz[,c(1:4)])
#' int_colData(sce)$spatialCoords <- as.matrix(xyz[,c(5,6)])
#' 
#' ## Coercing an sce without imgData
#' (spe <- as(sce, "SpatialExperiment"))
#' 
#' ## Coercing an sce with imgData
#' int_colData(sce)$imgData <- img
#' (spe <- as(sce, "SpatialExperiment"))
#' 
#' ## toSpatialExperiment method
#' colData(sce) <- DataFrame(xyz)
#' (spe <- toSpatialExperiment(sce,
#'     imgData = img,
#'     spatialCoordsNames = c("pxl_col_in_fullres", "pxl_row_in_fullres"),
#'     sample_id = "sample01"))
NULL


setAs(
    from="SingleCellExperiment", 
    to="SpatialExperiment", 
    function(from) {
        sample_id <- unique(from$sample_id)
        if (is.null(sample_id)) sample_id <- "sample01"
        icd <- int_colData(from)
        spe <- .sce_to_spe(from, 
            sample_id=sample_id,
            spatialCoords=icd$spatialCoords,
            imgData=icd$imgData)
        return(spe)
    }
)

#' @export
toSpatialExperiment <- function(sce,
    sample_id="sample01", 
    spatialCoordsNames=NULL,
    spatialCoords=NULL,
    scaleFactors=1,
    imageSources=NULL,
    image_id=NULL,
    loadImage=TRUE,
    imgData=NULL) {
    
    stopifnot(is(sce, "SingleCellExperiment"))
    
    ## giving priority to passed arguments
    if (all(is.null(spatialCoords), is.null(spatialCoordsNames))) {
        spatialCoords <- int_colData(sce)$spatialCoords
    }
    if (is.null(imgData)) {
        imgData <- int_colData(sce)$imgData
    }
    spe <- .sce_to_spe(sce=sce,
        sample_id = sample_id,
        spatialCoordsNames = spatialCoordsNames,
        spatialCoords = spatialCoords,
        scaleFactors = scaleFactors,
        imageSources = imageSources,
        image_id = image_id,
        loadImage = loadImage,
        imgData = imgData)
    return(spe)
}
