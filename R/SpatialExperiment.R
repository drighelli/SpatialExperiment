#' @name SpatialExperiment
#' @rdname SpatialExperiment
#' @title The SpatialExperiment class
#' 
#' @description
#' The SpatialExperiment class is designed to represent spatial omics data. 
#' It inherits from the \linkS4class{SingleCellExperiment} class and
#' is used in the same manner. In addition, the class supports storage of images
#' for multiple samples and of different resolutions via \code{\link{imgData}}.
#' @param ... 
#'   arguments to be passed to the \code{\link{SingleCellExperiment}} 
#'   constructor to fill the slots of the base class.
#' @param sample_id 
#' a character of a sample identifier in
#' conformity with the \code{sample_id} in \code{imgData}
#' It is automatically detected from the colData, if not present 
#' it assigns the value present into this paramenter (default is "sample_01").
#' @param spatialData 
#' the spatial coordinates DataFrame can have multiple columns. 
#' The coordinates must be named as specified into the \code{spatialCoordsNames} 
#' argument.
#' @param spatialCoordsNames a character vector indicating the names of the 
#' coordinates into the \code{spatialData} structure. (Default is \code{c("x", "y")})
#' @param scaleFactors 
#' the scale factors to be associated with the image(s) (optional, default 1).
#' It can be a number, a file path linking to a JSON file or the values read 
#' from a 10x Visium scaleFactors JSON file.
#' In these last two cases (10x Visium scale factors), 
#' it automatically detects which factor 
#' has to be loaded, depending on the resolution of the loaded image 
#' (see \code{imageSources}).
#' @param imgData (optional)
#'   a \code{DataFrame} storing the image data (see details).
#' @param imageSources (optional)
#' one or more image sources, they can be local paths or URLs.
#' @param image_id (optional)
#' a character vector of the same length of \code{imageSources} within unique 
#' \code{image_id}(s).
#' @param loadImage 
#' a logical indicating if the image has to be loaded in memory 
#' (default is FALSE).
#' 
#' @details 
#' The contructor expects the user to provide a \code{sample_id} column into the
#' \code{colData}, otherwise it assigns the \code{sample_id} parameter value.
#' If the \code{imgData} argument is not \code{NULL}, it considers it 
#' already built, otherwise it builds it from the \code{imgSources}, 
#' combining the \code{image_id} if provided. 
#' Otherwise it builds the \code{image_id} from the \code{sample_id} and 
#' the \code{imageSources}.
#' If multiple samples have to be combined, please refer to 
#' \code{\link{combine}}.
#' 
#' 
#' @author Dario Righelli & Helena L. Crowell
#' 
#' @return a \code{SpatialExperiment} object
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
#'     "barcode", "in_tissue", "array_row", "array_col",
#'     "pxl_row_in_fullres", "pxl_col_in_fullres"))
#'     
#' # construct observation & feature metadata
#' rd <- S4Vectors::DataFrame(
#'   symbol = rowData(sce)$Symbol)
#'   
#' # construct 'SpatialExperiment'
#' (se <- SpatialExperiment(
#'     assays = list(counts = assay(sce)),
#'     colData = colData(sce), rowData = rd, imgData = img,
#'     spatialData=xyz, 
#'     spatialCoordsNames=c("array_col", "array_row"),
#'     sample_id="foo"))
NULL

#' @importFrom S4Vectors DataFrame
#' @importFrom SingleCellExperiment SingleCellExperiment
#' @export
SpatialExperiment <- function(..., 
                              sample_id="sample_01",
                              spatialData=NULL,
                              spatialCoordsNames=c("x", "y"),
                              scaleFactors=1,
                              imageSources=NULL,
                              image_id=NULL,
                              loadImage=FALSE,
                              imgData=NULL)
{
    sce <- SingleCellExperiment(...)
    spe <- .sce_to_spe(sce=sce,
                       sample_id=sample_id,
                       spatialData=spatialData,
                       spatialCoordsNames=spatialCoordsNames,
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
                        sample_id="sample_01",
                        spatialData=NULL,
                        spatialCoordsNames=c("x", "y"),
                        scaleFactors=1,
                        imageSources=NULL,
                        image_id=NULL,
                        loadImage=TRUE,
                        imgData=NULL)
{
    old <- S4Vectors:::disableValidity()
    if (!isTRUE(old)) {
        S4Vectors:::disableValidity(TRUE)
        on.exit(S4Vectors:::disableValidity(old))
    }
    
    if ( "Sample" %in% colnames(colData(sce)) )
    {
        if( is.null(sample_id) )
        {
            sce$sample_id <- sce$Sample
        } else {
            ## check how to handle multiple sample_id(s)
            sce$sample_id <- sample_id 
        }
        colData(sce) <- colData(sce)[,-which(colnames(colData(sce)) == "Sample")]
    } else {
        ## check how to handle multiple sample_id(s)
        if ( !isEmpty(colData(sce)) ) sce$sample_id <- sample_id 
    }
    spe <- new("SpatialExperiment", sce)

    
    if ( !is.null(spatialData) )
    {
        stopifnot( all(spatialCoordsNames %in% colnames(spatialData)) )
    }
    spe@spaCoordsNms <- spatialCoordsNames
    spatialData(spe) <- spatialData
        
    
    if ( !is.null(imgData) )
    {
        stopifnot( all(imgData$sample_id == spe$sample_id) )
        imgData(spe) <- imgData
    } else if ( !is.null(imageSources) ) {
        if ( is.null(image_id) )
        {
            image_id=paste0(sample_id, "_",
                            sub(pattern="(.*)\\..*$", 
                                replacement="\\1", 
                                basename(imageSources)), 
                            seq_along(imageSources))
        } else {
            stopifnot(length(image_id) != length(imageSources))
        }
        
        for ( i in seq_along(imageSources) )
        {
            spe <- addImg(spe, imageSource=imageSources[i], 
                        scaleFactor=.loadScaleFacts(scaleFactors, 
                                            basename(imageSources[i])), 
                        sample_id=sample_id, image_id=image_id[i], 
                        load=loadImage)
        }
    }

    return(spe)
        
}

setAs(
    from="SingleCellExperiment", 
    to="SpatialExperiment", 
    function(from) .sce_to_spe(from, sample_id=NULL))

