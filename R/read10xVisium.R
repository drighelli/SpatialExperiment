#' @rdname read10xVisium
#' @title Load data from a 10X Visium experiment
#' 
#' @description 
#' Creates a \code{\link{SpatialExperiment}} from the CellRanger 
#' output directories for 10X Visium spatial gene expression data.
#' 
#' @param samples a character vector specifying one or more directories, 
#'   each corresponding to a 10X Visium sample (see details);
#'   if provided, names will be used as sample identifiers
#' @param sample_id character string specifying unique sample identifiers,
#'   one for each directory specified via \code{samples}; 
#'   ignored if \code{!is.null(names(samples))}
#' @param type character string specifying 
#'   the type of format to read count data from
#'   (see \code{\link{read10xCounts}})
#' @param images character vector specifying which images to include.
#' @param load logical; should the image(s) be loaded into memory
#'   as a \code{grob}? If FALSE, will store the path/URL instead.
#'   
#' @details 
#' The constructor assumes data from each sample are located 
#' in a single output directory as returned by Space Range, 
#' thus having the following file organization:
#' 
#' sample \cr
#' |—outs \cr
#' · · |—raw/filtered_feature_bc_matrix.h5 \cr
#' · · |—raw/filtered_feature_bc_matrix    \cr
#' · · · · |—barcodes.tsv \cr
#' · · · · |—features.tsv \cr
#' · · · · |—matrix.mtx   \cr
#' · · |—spatial \cr
#' · · · · |—scalefactors_json.json    \cr
#' · · · · |—tissue_lowres_image.png   \cr
#' · · · · |—tissue_positions_list.csv \cr
#'
#' @return a \code{\link{SpatialExperiment}}
#'
#' @author Helena L. Crowell
#'
#' @examples
#' dir <- system.file(
#'   file.path("extdata", "10xVisium"), 
#'   package = "SpatialExperiment")
#'   
#' sample_ids <- c("section1", "section2")
#' samples <- file.path(dir, sample_ids)
#'   
#' list.files(samples[1])
#' list.files(file.path(samples[1], "spatial"))
#' list.files(file.path(samples[1], "raw_feature_bc_matrix"))
#' 
#' (ve <- read10xVisium(samples, sample_ids, type="HDF5",
#'   images = c("lowres"), load = FALSE))
#' 
#' # tabulate number of spots mapped to tissue
#' table(
#'   in_tissue = ve$in_tissue, 
#'   sample_id = ve$sample_id)
#' 
#' # view available images
#' imgData(ve)
#' 
#' @importFrom rjson fromJSON
#' @importFrom DropletUtils read10xCounts
#' @importFrom methods as
#' @importFrom S4Vectors DataFrame metadata 
#' @importFrom SummarizedExperiment assay assay<- rowData rowData<- metadata<-
#' @export
read10xVisium <- function(samples="",
    sample_id=paste0("sample", seq_along(samples)),
    type=c("HDF5", "sparse"),
    images=c("fullres", "lowres", "hires", "detected", "aligned"),
    load=TRUE)
{
    type <- match.arg(type)
    imgs <- match.arg(images, several.ok=TRUE)

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
    
    # setup file paths
    fn <- paste0(
        "raw_feature_bc_matrix", 
        switch(type, HDF5=".h5", ""))
    counts <- file.path(samples, fn)
    
    # TODO: check that these files exist & are of valid format
    # otherwise things will fail & give unhelpful error messages
    
    dir <- file.path(samples, "spatial")
    xyz <- file.path(dir, "tissue_positions_list.csv")
    sfs <- file.path(dir, "scalefactors_json.json")
    names(xyz) <- names(sfs) <- sids
    
    img_fns <- list(
        lowres="tissue_lowres_image.png",
        hires="tissue_hires_image.png",
        detected="detected_tissue_image.jpg",
        aligned="aligned_fiducials.jpg")
    img <- unlist(lapply(dir, function(.) 
        file.path(., img_fns[imgs])))
    
    # read count data
    scelist <- lapply(seq_along(counts), function(i) 
    {
        read10xCounts(
            samples=counts[i], 
            sample.names=sids[i],
            col.names=TRUE)
    }) 
    # sce <- read10xCounts(
    #     samples=counts, 
    #     sample.names=sids,
    #     col.names=TRUE)
    # TODO: 
    # read10xCounts() gives a 'DelayedMatrix',
    # which doesn't work with 'scater'...
    # any way to better fix that?
    #assay(sce) <- as(as.matrix(assay(sce)), "dgCMatrix")
    
    # read spatial data
    # colData(sce) <- .read_xyz(xyz)[colnames(sce), ] see below
    
    # construct 'SpatialExperiment'
    spelist <- lapply(scelist, function(sce)
    {
        rowData(sce) <- DataFrame(symbol=rowData(sce)$Symbol)
        metadata(sce)$Sample <- NULL
        return(as(sce, "SpatialExperiment"))
    })
    
    coords <- lapply(xyz, function(xxx) 
    {
        .read_xyz(xxx)
    })
    spelist <- lapply(spelist, function(spe) {
        spatialCoords(spe) <- coords[[1]][colnames(spe),]
        return(spe)
    })
    
    spe <- do.call(cbind, spelist)
    # sce$sample_id <- sce$Sample
    # rowData(sce) <- DataFrame(symbol=rowData(sce)$Symbol)
    # metadata(sce)$Sample <- NULL
    # spe <- as(sce, "SpatialExperiment")
    # SpatialExperiment(sce, spatialCoords=.read_xyz(xyz)[colnames(sce), ])
    ##### using the setter
    # spatialCoords(spe) <- .read_xyz(xyz)[colnames(sce), ] 
    # read image data
    id <- readImgData(samples, sids, img, sfs, load)
    imgData(spe) <- id
    return(spe)
}


#' @importFrom S4Vectors DataFrame
#' @importFrom utils read.csv
.read_xyz <- function(x) {
    cnms <- c(
        "barcode", "in_tissue", "array_row", "array_col", 
        "pxl_col_in_fullres", "pxl_row_in_fullres")
    df <- lapply(seq_along(x), function(i) 
    {
        df <- read.csv(x[i], header=FALSE, row.names=1, col.names=cnms)
        if (length(x) > 1) rownames(df) <- paste(i, rownames(df), sep = "_")
        if(!is.null(names(x))) cbind(sample_id=names(x)[i], df)
        df
    })
    df <- do.call(rbind, df)
    df$in_tissue <- as.logical(df$in_tissue)
    return(df)
    ###### they are not stored as matrix anymore
    # xy <- grep("pxl", names(df))
    # mat <- as.matrix(df[, xy])
    # DataFrame(df[, -xy], xy_coords=I(mat))
}
