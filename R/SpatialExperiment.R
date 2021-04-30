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
#' fnm <- file.path(dir, "raw_feature_bc_matrix")
#' sce <- DropletUtils::read10xCounts(fnm)
#' 
#' # read in image data
#' img <- readImgData(
#'   path = file.path(dir, "spatial"),
#'   sample_id="foo")
#' 
#' # read in spatial coordinates
#' fnm <- file.path(dir, "spatial", "tissue_positions_list.csv")
#' xyz <- read.csv(fnm, header = FALSE,
#'   col.names = c(
#'     "barcode", "in_tissue", "array_row", "array_col",
#'     "pxl_row_in_fullres", "pxl_col_in_fullres"))
#'     
#' # construct observation & feature metadata
#' rd <- S4Vectors::DataFrame(
#'   symbol = rowData(sce)$Symbol)
#'   
#' # construct 'SpatialExperiment'
#' (spe <- SpatialExperiment(
#'     assays = list(counts = assay(sce)),
#'     colData = colData(sce), rowData = rd, imgData = img,
#'     spatialData=DataFrame(xyz), 
#'     spatialCoordsNames=c("pxl_col_in_fullres", "pxl_row_in_fullres"),
#'     sample_id="foo"))
NULL

#' @importFrom S4Vectors DataFrame
#' @importFrom SingleCellExperiment SingleCellExperiment
#' @export
SpatialExperiment <- function(..., 
    sample_id="sample1",
    spatialDataNames=NULL,
    spatialCoordsNames=NULL,
    spatialData=NULL,
    spatialCoords=NULL,
    scaleFactors=1,
    imageSources=NULL,
    image_id=NULL,
    loadImage=FALSE,
    imgData=NULL) {
    
    sce <- SingleCellExperiment(...)
    spe <- .sce_to_spe(sce=sce,
        sample_id=sample_id,
        spatialDataNames=spatialDataNames,
        spatialCoordsNames=spatialCoordsNames,
        spatialData=spatialData,
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
    sample_id="sample1", 
    spatialDataNames=NULL,
    spatialCoordsNames=NULL,
    spatialData=NULL,
    spatialCoords=NULL,
    scaleFactors=1,
    imageSources=NULL,
    image_id=NULL,
    loadImage=TRUE,
    imgData=NULL) {
    
    old <- S4Vectors:::disableValidity()
    if (!isTRUE(old)) {
        S4Vectors:::disableValidity(TRUE)
        on.exit(S4Vectors:::disableValidity(old))
    }
    
    if (is.null(sce$sample_id)) {
        stopifnot(
            is.character(sample_id),
            any(length(sample_id) == c(1, ncol(sce))))
        if (ncol(sce) == 0) 
            sample_id <- character()
        sce$sample_id <- sample_id
    } else {
        if (!is.character(sce$sample_id))
            sce$sample_id <- as.character(sce$sample_id)
        sample_id <- unique(sce$sample_id)
    }
    
    spe <- new("SpatialExperiment", sce)

    # in the following code chunk, we give precedence 
    # to spatialData/CoordsNames over spatialData/Coords
    #   where spatialDataNames should be in colData,
    #   and spatialCoordsNames can be in both colData and spatialData
    # if both spatialData/Coords and -Names are supplied
    #   we give an informative warning notifying the user
    #   that spatialData/CoordsNames will be used 
    
    msg <- function(.) message(sprintf(paste(                
        "both '%s' and '%sNames'  have been supplied; using '%s'.",
        "Set either to NULL to suppress this message"), ., ., .))
    
    if (!is.null(spatialCoordsNames)) {
        stopifnot(
            is.character(spatialCoordsNames),
            all(spatialCoordsNames %in% names(colData(spe)))
            || all(spatialCoordsNames %in% names(spatialData)))
        if (!is.null(spatialCoords)) 
            msg("spatialCoords")
        if (all(spatialCoordsNames %in% names(colData(spe)))) {
            i <- spatialCoordsNames
            j <- setdiff(names(colData(spe)), i)
            spatialCoords(spe) <- as.matrix(colData(spe)[i])
            colData(spe) <- colData(spe)[j]
        } else {
            i <- spatialCoordsNames
            j <- setdiff(names(spatialData), i)
            spatialCoords(spe) <- as.matrix(spatialData[i])
            spatialData <- spatialData[j]
        }
    } else if (!is.null(spatialCoords)) {
        stopifnot(
            is.matrix(spatialCoords),
            is.numeric(spatialCoords),
            nrow(spatialCoords) == ncol(spe))
        spatialCoords(spe) <- spatialCoords
    } else {
        spatialCoords(spe) <- NULL
    }

    if (!is.null(spatialDataNames)) {
        stopifnot(
            is.character(spatialDataNames), 
            spatialDataNames %in% names(colData(spe)))
        if (!is.null(spatialData)) 
            msg("spatialData")
        spatialDataNames(spe) <- spatialDataNames
    } else if (!is.null(spatialData)) {
        stopifnot(
            is(spatialData, "DFrame"),
            nrow(spatialData) == ncol(spe))
        colData(spe) <- cbind(colData(spe), spatialData)
        spatialDataNames(spe) <- names(spatialData)
    } else {
        spatialData(spe) <- NULL
    }

    if (!is.null(imgData)) {
        stopifnot(imgData$sample_id %in% spe$sample_id)
        imgData(spe) <- imgData
    } else if (!is.null(imageSources) ){
        if (is.null(image_id)) {
            image_id <- sub("(.*)\\..*$", "\\1", basename(imageSources))
            image_id <- paste0(sample_id, "_", image_id, seq_along(imageSources))
        } else {
            stopifnot(length(image_id) != length(imageSources))
        }
        for (i in seq_along(imageSources)) {
            scaleFactor <- .get_scaleFactor(scaleFactors, imageSources[i])
            spe <- addImg(spe, 
                imageSource=imageSources[i], scaleFactor=scaleFactor, 
                sample_id=sample_id[i], image_id=image_id[i], load=loadImage)
        }
    } else {
        imgData(spe) <- NULL
    }
    return(spe)
}

setAs(
    from="SingleCellExperiment", 
    to="SpatialExperiment", 
    function(from) .sce_to_spe(from, sample_id=NULL))

#' @importFrom rjson fromJSON
.get_scaleFactor <- function(scaleFactors, imageSource=NULL) {
    
    scf <- scaleFactors
    if (is.numeric(scf))
        return(scf)
    if (!is.list(scf)) {
        if (!(is.character(scaleFactors) && grepl("\\.json$", scaleFactors)))
            stop("scaleFactors should be numeric, a list or JSON file.")
        scf <- fromJSON(file=scaleFactors)
    }
    if (!is.null(imageSource)) {
        img_fnm <- basename(imageSource)
        if (grepl("lowres", img_fnm)) {
            scf <- scf$tissue_lowres_scalef
        } else if (grepl("hires", img_fnm)) {
            scf <- scf$tissue_lowres_scalef
        } else {
            stop("Couldn't match input imageSources and scaleFactors; please",
                " provide a numeric value or a list of scaleFactors instead.")
        }
    }
    return(scf)
}