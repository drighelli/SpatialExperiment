#' @rdname SpatialExperiment
#' @title The SpatialExperiment class
#' 
#' @description
#' The SpatialExperiment class is designed to represent spatial transcriptomics 
#' (ST) data. It inherits from the \linkS4class{SingleCellExperiment} class and
#' is used in the same manner. In addition, the class supports storage of images
#' for multiple samples and of different resolutions via \code{\link{imgData}}, 
#' and requires certain observation metadata specific to ST data to be present.
#'
#' @param ... 
#'   arguments to be passed to the \code{\link{SingleCellExperiment}} 
#'   constructor to fill the slots of the base class.
#' @param xyzData 
#'   a two/(three)-column numeric matrix containing 
#'   spatial xy(z)-coordinates for each observation
#' @param imgData 
#'   a \code{DataFrame} storing the image data (see details)
#' @param sample_id 
#'   a character vector of unique sample identifiers in
#'   conformity with the \code{sample_id}s in \code{imgData}
#' @param inTissue 
#'   a logical vector indicating whether or not
#'   an observation could be mapped onto the tissue

#' 
#' @details 
#' If any of arguments \code{sample_id}, \code{inTissue} and \code{xyzData}
#' are missing, the \code{SpatialExperiment} constructor will assume 
#' these are supplied via the input \code{colData}. 
#' 
#' @author Dario Righelli & Helena L. Crowell
#' 
#' @return a \code{SpatialExperiment}
#' 
#' @examples
#' dir <- system.file(
#'   file.path("extdata", "10xVisium", "section1"),
#'   package = "SpatialExperiment")
#' 
#' # read in counts
#' fnm <- file.path(dir, "raw_feature_bc_matrix.h5")
#' sce <- DropletUtils::read10xCounts(fnm)
#' 
#' # read in image data
#' img <- readImgData(
#'   path = file.path(dir, "spatial"), 
#'   sample_id="foo")
#' 
#' # read in spatial coordinates
#' fnm <- file.path(dir, "spatial", "tissue_positions_list.csv")
#' xyz <- read.csv(fnm, header = FALSE, row.names = 1, 
#'   col.names = c(
#'     "barcode", "inTissue", "array_row", "array_col", 
#'     "pxl_row_in_fullres", "pxl_col_in_fullres"))
#'    
#' # construct observation & feature metadata 
#' rd <- S4Vectors::DataFrame(
#'   symbol = rowData(sce)$Symbol) 
#'   
#' cd <- S4Vectors::DataFrame(
#'   sample_id = "foo",
#'   inTissue = as.logical(xyz$inTissue),
#'   xyzData = I(as.matrix(xyz[, grep("pxl", names(xyz))])))
#'   
#' # construct 'SpatialExperiment'
#' SpatialExperiment(
#'   assays = list(counts = assay(sce)),
#'   colData = cd, rowData = rd, imgData = img)

#' @importFrom S4Vectors DataFrame
#' @importFrom SingleCellExperiment SingleCellExperiment
#' @export
SpatialExperiment <- function(..., 
                              sample_id="Sample01",
                              spatialCoords=DataFrame(),
                              scaleFactors=1,
                              imageSources=NULL,
                              image_id=NULL,
                              loadImage=TRUE,
                              imgData=NULL)
{   
    sce <- SingleCellExperiment(...)
    spe <- .sce_to_spe(sce=sce,
                       sample_id=sample_id,
                       spatialCoords=spatialCoords,
                       scaleFactors=scaleFactors,
                       imageSources=imageSources,
                       loadImage=loadImage,
                       imgData=imgData)
    return(spe)
}

#' @importFrom methods new
#' @importFrom S4Vectors DataFrame
#' @importFrom SingleCellExperiment int_metadata<-
.sce_to_spe <- function(sce,
                        sample_id="Sample01",
                        spatialCoords=DataFrame(),
                        scaleFactors=1,
                        imageSources=NULL,
                        image_id=NULL,
                        loadImage=TRUE,
                        imgData=NULL)
    ## provide path to load 10x data with read10x function
{
    old <- S4Vectors:::disableValidity()
    if (!isTRUE(old)) {
        S4Vectors:::disableValidity(TRUE)
        on.exit(S4Vectors:::disableValidity(old))
    }
    
    if (is.null(sce$sample_id))
    {
        message("No sample_id provided in colData, assigning: ", sample_id)
        sce$sample_id <- sample_id ## check how to handle multiple sample_id(s)
    }
    
    spe <- new("SpatialExperiment", sce)
    
    spatialCoords(spe) <- spatialCoords
    
    if(is.null(imgData))
    {
        imgData(spe) <- imgData
    }
    
    if(!is.null(imageSources))
    {
        if(is.null(image_id))
        {
            image_id=paste0(sample_id, "_",
                            sub(pattern="(.*)\\..*$", 
                                replacement="\\1", 
                                basename(imageSources)), 
                            1:length(imageSources))
        } else {
            stopifnot(length(image_id) != length(imageSources))
        }
        
        for(i in 1:length(imageSources))
        {
            spe <- addImg(spe, imageSource=imageSources[i], 
                        scaleFactor=.loadScaleFacts(scaleFactors, 
                                            basename(imageSources[i])), 
                        sample_id=sample_id[i], image_id=image_id[[i]], 
                        load=loadImage)
        }
    }

    return(spe)
}

setAs(
    from="SingleCellExperiment", 
    to="SpatialExperiment", 
    function(from) .sce_to_spe(from))


################ global definitions to move into another file
EXPRSNAMES <- c("^([x|X]|pxl_col)", "^([y|Y]|pxl_row)", "^([z|Z])", 
                "in_tissue", "array_row", "array_col")
SPATDATANAMES <- c("x_coord", "y_coord", "z_coord", 
                  "in_tissue", "array_row", "array_col")
