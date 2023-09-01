#' @rdname read10xVisium
#' 
#' @title Load data from a 10x Genomics Visium experiment
#' 
#' @description 
#' Creates a \code{\link{SpatialExperiment}} from the Space Ranger 
#' output directories for 10x Genomics Visium spatial gene expression data.
#' 
#' @param samples a character vector specifying one or more directories, 
#'   each corresponding to a 10x Genomics Visium sample (see Details);
#'   if provided, names will be used as sample identifiers
#' @param sample_id character string specifying unique sample identifiers,
#'   one for each directory specified via \code{samples}; 
#'   ignored if \code{!is.null(names(samples))}
#' @param type character string specifying 
#'   the type of format to read count data from
#'   (see \code{read10xCounts})
#' @param data character string specifying whether to read in
#'   filtered (spots mapped to tissue) or raw data (all spots).
#' @param images character vector specifying which images to include. 
#'   Valid values are \code{"lowres", "hires", "fullres", "detected", "aligned"}
#' @param load logical; should the image(s) be loaded into memory
#'   as a \code{grob}? If FALSE, will store the path/URL instead.
#'   
#' @details 
#' The constructor assumes data from each sample are located 
#' in a single output directory as returned by Space Ranger, 
#' thus having the following file organization (where "raw/filtered" 
#' refers to either "raw" or "filtered" to match the `data` argument.) 
#' The base directory "outs/" from Space Ranger can either be included 
#' manually in the paths provided in `samples`, or can be ignored; 
#' if ignored, it will be added automatically. The `.h5` files are 
#' used if `type = "HDF5"`. (Note that `tissue_positions.csv` was 
#' renamed in Space Ranger v2.0.0.)
#' 
#' sample \cr
#' · | — outs \cr
#' · · | — raw/filtered_feature_bc_matrix.h5 \cr
#' · · | — raw/filtered_feature_bc_matrix    \cr
#' · · · | — barcodes.tsv.gz \cr
#' · · · | — features.tsv.gz \cr
#' · · · | — matrix.mtx.gz   \cr
#' · · | — spatial \cr
#' · · · | — scalefactors_json.json    \cr
#' · · · | — tissue_lowres_image.png   \cr
#' · · · | — tissue_positions.csv \cr
#'
#' @return a \code{\link{SpatialExperiment}} object
#'
#' @author Helena L. Crowell
#'
#' @examples
#' dir <- system.file(
#'   file.path("extdata", "10xVisium"), 
#'   package = "SpatialExperiment")
#'   
#' sample_ids <- c("section1", "section2")
#' samples <- file.path(dir, sample_ids, "outs")
#'   
#' list.files(samples[1])
#' list.files(file.path(samples[1], "spatial"))
#' file.path(samples[1], "raw_feature_bc_matrix")
#' 
#' (spe <- read10xVisium(samples, sample_ids, 
#'   type = "sparse", data = "raw", 
#'   images = "lowres", load = FALSE))
#' 
#' # base directory 'outs/' from Space Ranger can also be omitted
#' samples2 <- file.path(dir, sample_ids)
#' (spe2 <- read10xVisium(samples2, sample_ids, 
#'   type = "sparse", data = "raw", 
#'   images = "lowres", load = FALSE))
#' 
#' # tabulate number of spots mapped to tissue
#' cd <- colData(spe)
#' table(
#'   in_tissue = cd$in_tissue, 
#'   sample_id = cd$sample_id)
#' 
#' # view available images
#' imgData(spe)
#' 
#' @importFrom rjson fromJSON
#' @importFrom methods as
#' @importFrom S4Vectors DataFrame 
#' @importFrom SummarizedExperiment assays rowData
#' @export
read10xVisium <- function(samples="",
    sample_id=paste0("sample", sprintf("%02d", seq_along(samples))),
    type=c("HDF5", "sparse"),
    data=c("filtered", "raw"),
    images="lowres",
    load=TRUE) {
    
    # check if DropletUtils is installed
    if (!requireNamespace("DropletUtils", quietly = TRUE)) {
        warning("DropletUtils package must be installed to use read10xVisium()")
    }
    
    # check validity of input arguments
    type <- match.arg(type)
    data <- match.arg(data)
    imgs <- c("lowres", "hires", "detected", "aligned")
    imgs <- match.arg(images, imgs, several.ok=TRUE)

    # check sample identifiers
    if (is.null(sids <- names(samples))) {
        if (is.null(sids <- sample_id)) {
            stop("'sample_id' mustn't be NULL when 'samples' are unnamed")
        } else if (!is.character(sample_id) 
            && length(unique(sample_id)) != length(samples))
            stop("'sample_id' should contain as many unique values as 'samples'")
    } else if (length(unique(sids)) != length(samples))
        stop("names of 'samples' should be unique")
    names(samples) <- sids
    
    # add "outs/" directory if not already included
    i <- basename(samples) != "outs"
    samples[i] <- file.path(samples[i], "outs")
    
    # setup file paths
    fns <- paste0(
        data, "_feature_bc_matrix", 
        switch(type, HDF5=".h5", ""))
    counts <- file.path(samples, fns)
    
    # TODO: check that these files exist & are of valid format
    # otherwise things will fail & give unhelpful error messages
    
    dir <- file.path(samples, "spatial")
    suffix <- c("", "_list")
    xyz <- file.path(
        rep(dir, each = length(suffix)), 
        sprintf(
            "tissue_positions%s.csv", 
            rep(suffix, length(sids))))
    xyz <- xyz[file.exists(xyz)]
    sfs <- file.path(dir, "scalefactors_json.json")
    names(xyz) <- names(sfs) <- sids
    
    # read image data
    img_fns <- list(
        lowres="tissue_lowres_image.png",
        hires="tissue_hires_image.png",
        detected="detected_tissue_image.jpg",
        aligned="aligned_fiducials.jpg")
    
    img_fns <- img_fns[imgs]
    img_fns <- lapply(dir, file.path, img_fns)
    img_fns <- unlist(img_fns)
    
    nan <- !file.exists(img_fns)
    if (all(nan)) {
        stop(sprintf(
            "No matching files found for 'images=c(%s)", 
            paste(dQuote(imgs), collapse=", ")))
    } else if (any(nan)) {
        message("Skipping missing images\n  ", 
            paste(img_fns[nan], collapse="\n  "))
        img_fns <- img_fns[!nan]
    }
    img <- readImgData(samples, sids, img_fns, sfs, load)
    
    # read spatial coordinates
    spel <- lapply(seq_along(counts), function(i) {
        # read count data as 'SingleCellExperiment'
        sce <- DropletUtils::read10xCounts(
            samples=counts[i], 
            sample.names=sids[i],
            col.names=TRUE)
        # read in spatial data
        spd <- .read_xyz(xyz[i])
        # match ordering
        obs <- intersect(
            colnames(sce), 
            rownames(spd))
        sce <- sce[, obs]
        spd <- spd[obs, ]
        # construct 'SpatialExperiment'
        SpatialExperiment(
            assays=assays(sce),
            rowData=DataFrame(symbol=rowData(sce)$Symbol),
            sample_id=sids[i],
            colData=DataFrame(spd),
            spatialCoordsNames=c("pxl_col_in_fullres", "pxl_row_in_fullres"))
    }) 
    spe <- do.call(cbind, spel)
    imgData(spe) <- img
    return(spe)
}

#' @importFrom S4Vectors DataFrame
#' @importFrom utils read.csv
.read_xyz <- function(x) {
    cnms <- c(
        "barcode", "in_tissue", "array_row", "array_col", 
        "pxl_row_in_fullres", "pxl_col_in_fullres")
    df <- lapply(seq_along(x), function(i) 
    {
        df <- read.csv(x[i], 
            header=!grepl("list", x[i]), 
            row.names=1, col.names=cnms)
        if (length(x) > 1) rownames(df) <- paste(i, rownames(df), sep="_")
        if (!is.null(names(x))) cbind(sample_id=names(x)[i], df)
        df
    })
    df <- do.call(rbind, df)
    df$in_tissue <- as.logical(df$in_tissue)
    return(df)
}
