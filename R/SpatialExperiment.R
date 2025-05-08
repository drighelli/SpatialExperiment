#' @name SpatialExperiment
#' 
#' @title The SpatialExperiment class
#' 
#' @aliases
#' SpatialExperiment
#' SpatialExperiment-class
#' 
#' @description
#' The \code{SpatialExperiment} class is designed to represent spatially
#' resolved transcriptomics (ST) data. It inherits from the
#' \code{\link{SingleCellExperiment}} class and is used in the same manner. In
#' addition, the class supports storage of spatial information via
#' \code{\link{spatialCoords}} and storage of images via \code{\link{imgData}}.
#' 
#' @param ... Arguments passed either to the \code{\link{SingleCellExperiment}}
#'   constructor to fill the slots of the base class, or to
#'   \code{\link{addImg}} for user-defined columns in \code{\link{imgData}} 
#'   (same length as \code{imageSources}).
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
#' @param spatialDataNames (Deprecated) A \code{character} vector of column
#'   names from \code{\link{colData}} to include in \code{\link{spatialData}}.
#'   Alternatively, the \code{spatialData} argument may be provided. If both are
#'   provided, \code{spatialDataNames} is given precedence, and a warning is
#'   returned. (Note: \code{spatialData} and \code{spatialDataNames} have been
#'   deprecated; \code{colData} and \code{spatialCoords} should be used for all
#'   columns. The arguments have been retained for backward compatibility but
#'   may be removed in the future.)
#' @param spatialData (Deprecated) A \code{\link{DataFrame}} containing columns
#'   to store in \code{\link{spatialData}}, which must contain at least the
#'   columns of spatial coordinates. Alternatively, \code{spatialDataNames} may
#'   be provided. If both are provided, \code{spatialDataNames} is given
#'   precedence, and a warning is returned. (Note: \code{spatialData} and
#'   \code{spatialDataNames} have been deprecated; \code{colData} and
#'   \code{spatialCoords} should be used for all columns. The arguments have
#'   been retained for backward compatibility but may be removed in the future.)
#' 
#' @details
#' In this class, rows represent genes, and columns represent spots (for
#' spot-based ST platforms) or cells (for molecule-based ST platforms). As for
#' \code{\link{SingleCellExperiment}}, \code{counts} and \code{logcounts} can be
#' stored in the \code{\link{assays}} slot, and row and column metadata in
#' \code{\link{rowData}} and \code{\link{colData}}. For molecule-based ST data,
#' the additional measurements per molecule per cell can be stored in a
#' \code{BumpyMatrix}-formatted \code{assay} named \code{\link{molecules}}.
#' 
#' The additional arguments in the constructor documented above (e.g.
#' \code{spatialCoords}, \code{imgData}, and others) represent the extensions to
#' the \code{\link{SingleCellExperiment}} class to store associated spatial and
#' imaging information for ST data.
#' 
#' The constructor expects \code{colData} to contain a column named
#' \code{sample_id}. If this is not present, it will assign the value from the
#' \code{sample_id} argument. If the \code{imgData} argument is provided, the
#' constructor expects the \code{\link{imgData}} \code{\link{DataFrame}} to
#' already be built. Otherwise, it will build it from the \code{imageSources}
#' and (optional) \code{image_id} arguments. If \code{image_id} is not provided,
#' this will be assumed from \code{sample_id} and \code{imageSources} instead.
#' To combine multiple samples within a single object, see
#' \code{\link{combine}}.
#' 
#' For 10x Genomics Visium datasets, the \code{TENxVisiumList} and \code{import}
#' functions from the \code{VisiumIO} package can be used to load data into a 
#' \code{SpatialExperiment} object directly from Space Ranger output files.
#' 
#' @seealso
#' \code{?"\link{SpatialExperiment-methods}"}, which includes:
#' \code{\link{spatialCoords}}, \code{\link{spatialCoordsNames}},
#' \code{\link{imgData}}, \code{\link{scaleFactors}}
#' 
#' \code{?"\link{SpatialExperiment-assays}"}, which includes:
#' \code{\link{molecules}}
#' 
#' \code{?"\link{SpatialExperiment-colData}"}
#' 
#' \code{?"\link{SpatialExperiment-combine}"}
#' 
#' \code{?"\link{SpatialExperiment-subset}"}
#' 
#' \code{?"\link{SpatialExperiment-misc}"}
#' 
#' \code{\link{readImgData}}
#' 
#' \code{?"\link{imgData-methods}"}
#' 
#' \code{\link{SpatialImage}}
#' 
#' @author Dario Righelli and Helena L. Crowell
#' 
#' @return A \code{SpatialExperiment} object.
#' 
#' @examples
#' #########################################################
#' # Example 1: Spot-based ST (10x Visium) using constructor
#' #########################################################
#' 
#' dir <- system.file(
#'     file.path("extdata", "10xVisium", "section1", "outs"),
#'     package = "SpatialExperiment")
#' 
#' # read in counts
#' fnm <- file.path(dir, "raw_feature_bc_matrix")
#' sce <- DropletUtils::read10xCounts(fnm)
#' 
#' # read in image data
#' img <- readImgData(
#'     path = file.path(dir, "spatial"),
#'     sample_id="foo")
#' 
#' # read in spatial coordinates
#' fnm <- file.path(dir, "spatial", "tissue_positions_list.csv")
#' xyz <- read.csv(fnm, header = FALSE,
#'     col.names = c(
#'         "barcode", "in_tissue", "array_row", "array_col",
#'         "pxl_row_in_fullres", "pxl_col_in_fullres"))
#'     
#' # construct observation & feature metadata
#' rd <- S4Vectors::DataFrame(
#'     symbol = rowData(sce)$Symbol)
#'   
#' # construct 'SpatialExperiment'
#' (spe <- SpatialExperiment(
#'     assays = list(counts = assay(sce)),
#'     rowData = rd,
#'     colData = DataFrame(xyz),
#'     spatialCoordsNames = c("pxl_col_in_fullres", "pxl_row_in_fullres"),
#'     imgData = img,
#'     sample_id = "foo"))
#'     
#' #############################################################
#' # Example 2: Spot-based ST (10x Visium) using 'VisiumIO'
#' #############################################################
#' 
#' library(VisiumIO)
#' dir <- list.dirs(system.file(
#'   file.path("extdata", "10xVisium"),
#'   package = "SpatialExperiment"), 
#'   recursive = FALSE, full.names = TRUE)
#' (spe <- import(TENxVisiumList(
#'   sampleFolders = dir, 
#'   sample_ids = basename(dir), 
#'   processing = "raw", 
#'   images = "lowres")))
#' 
#' ##############################
#' # Example 3: Molecule-based ST
#' ##############################
#' 
#' # create simulated data
#' n <- 1000; ng <- 50; nc <- 20
#' # sample xy-coordinates in [0,1]
#' x <- runif(n)
#' y <- runif(n)
#' # assign each molecule to some gene-cell pair
#' gs <- paste0("gene", seq(ng))
#' cs <- paste0("cell", seq(nc))
#' gene <- sample(gs, n, TRUE)
#' cell <- sample(cs, n, TRUE)
#' # construct data.frame of molecule coordinates
#' df <- data.frame(gene, cell, x, y)
#' 
#' # (assure gene & cell are factor so that
#' # missing observations aren't dropped)
#' df$gene <- factor(df$gene, gs)
#' df$cell <- factor(df$cell, cs)
#' 
#' # construct BumpyMatrix
#' mol <- BumpyMatrix::splitAsBumpyMatrix(
#'     df[, c("x", "y")],
#'     row = df$gene, column = df$cell)
#'
#' # get count matrix
#' y <- with(df, table(gene, cell))
#' y <- as.matrix(unclass(y))
#' 
#' # construct SpatialExperiment
#' (spe_mol <- SpatialExperiment(
#'     assays = list(
#'         counts = y, 
#'         molecules = mol)))
NULL

#' @importFrom S4Vectors DataFrame
#' @importFrom SingleCellExperiment SingleCellExperiment
#' @export
SpatialExperiment <- function(..., 
    sample_id="sample01",
    spatialCoordsNames=NULL,
    spatialCoords=NULL,
    scaleFactors=1,
    imageSources=NULL,
    image_id=NULL,
    loadImage=FALSE,
    imgData=NULL,
    spatialDataNames=NULL,
    spatialData=NULL) {
  
    args <- list(...)
  
    # Get names of arguments for parent constructors.
    p_arg_nms <- unlist(sapply(
        c("SingleCellExperiment", "SummarizedExperiment"),
        function(x) names(formals(x))[names(formals(x)) != "..."],
        simplify = FALSE,
        USE.NAMES = FALSE
    ))
    
    # A list of arguments for parent constructors.
    p_args <- args[p_arg_nms][!sapply(args[p_arg_nms], is.null)]
    
    # A list of user-defined arguments for addImg().
    other_arg_nms <- names(args)[!(names(args) %in% p_arg_nms)]
    other_args <- args[other_arg_nms][!sapply(args[other_arg_nms], is.null)]
    
    sce <- do.call(
        SingleCellExperiment,
        p_args)
    spe <- do.call(
        .sce_to_spe,
        c(list(sce=sce,
            sample_id=sample_id,
            spatialCoordsNames=spatialCoordsNames,
            spatialCoords=spatialCoords,
            scaleFactors=scaleFactors,
            imageSources=imageSources,
            image_id=image_id,
            loadImage=loadImage,
            imgData=imgData,
            spatialDataNames=spatialDataNames,
            spatialData=spatialData),
        other_args))
    return(spe)
}

#' @importFrom methods new
#' @importFrom S4Vectors DataFrame
#' @importFrom SingleCellExperiment int_metadata<-
.sce_to_spe <- function(sce,
    sample_id="sample01", 
    spatialCoordsNames=NULL,
    spatialCoords=NULL,
    scaleFactors=1,
    imageSources=NULL,
    image_id=NULL,
    loadImage=TRUE,
    imgData=NULL,
    spatialDataNames=NULL,
    spatialData=NULL,
    ...) {
    
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
    
    if (!is.null(spatialData) || !is.null(spatialDataNames)) {
      .msg_spatialData()
    }
    
    spe <- new("SpatialExperiment", sce)

    # in the following code chunk, we give precedence 
    # to spatialData/CoordsNames over spatialData/Coords
    #   where spatialDataNames should be in colData,
    #   and spatialCoordsNames can be in both colData and spatialData
    # if both spatialData/Coords and -Names are supplied
    #   we give an informative warning notifying the user
    #   that spatialData/CoordsNames will be used 
    
    msg <- function(.) message(sprintf(                
        "Both '%s' and '%sNames'  have been supplied;\nusing '%s'. ", ., ., .),
        "Set either to NULL to suppress this message.")
    
    if (!is.null(spatialCoordsNames)) {
        stopifnot(is.character(spatialCoordsNames),
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
        i <- spatialDataNames
        j <- setdiff(names(colData(spe)), i)
        spd <- colData(spe)[i]
        colData(spe) <- colData(spe)[j]
        spatialData(spe) <- spd
    } else if (!is.null(spatialData)) {
        stopifnot(
            is(spatialData, "DFrame"),
            nrow(spatialData) == ncol(spe))
        spatialData(spe) <- spatialData
    } else {
        spatialData(spe) <- NULL
    }

    if (!is.null(imgData)) {
        stopifnot(imgData$sample_id %in% spe$sample_id)
        imgData(spe) <- imgData
    } else if (!is.null(imageSources) ){
        # Handle extra arguments.
        args <- list(...)
        arg_lens <- vapply(args, length, FUN.VALUE = integer(1), USE.NAMES = TRUE)
        stopifnot(all(arg_lens == length(imageSources)))
        
        if (is.null(image_id)) {
            image_id <- sub("(.*)\\..*$", "\\1", basename(imageSources))
            image_id <- paste0(sample_id, "_", image_id, seq_along(imageSources))
        } else {
            stopifnot(length(image_id) == length(imageSources))
        }
        for (i in seq_along(imageSources)) {
            scaleFactor <- ifelse(
                length(scaleFactors) > 1 && is.numeric(scaleFactors),
                scaleFactors[i],
                .get_scaleFactor(scaleFactors, imageSources[i]))
            spe <- do.call(
                addImg,
                c(list(spe,
                    imageSource=imageSources[i],
                    scaleFactor=scaleFactor, 
                    sample_id=ifelse(length(sample_id) > 1,sample_id[i], sample_id),
                    image_id=image_id[i],
                    load=loadImage),
                lapply(args, `[`, i)))
        }
    } else {
        imgData(spe) <- NULL
    }
    return(spe)
}

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