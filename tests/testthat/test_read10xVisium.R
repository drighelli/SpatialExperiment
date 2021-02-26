dir <- system.file(
    file.path("extdata", "10xVisium"), 
    package="SpatialExperiment")

sample_ids <- c("section1", "section2")
samples <- file.path(dir, sample_ids)

test_that("data are read correctly", {
    x <- read10xVisium(samples, sample_ids,
        type="sparse", data="raw", 
        images="lowres", load=FALSE)
    
    expect_is(x, "SpatialExperiment")
    expect_true(all(vapply(getImg(x, TRUE, TRUE), 
        function(.) is(., "SpatialImage"), logical(1))))
    
    cnms <- c(
        "barcode", "in_tissue", "array_row", "array_col", 
        "pxl_col_in_fullres", "pxl_row_in_fullres")
    xyz <- file.path(samples, "spatial", "tissue_positions_list.csv")
    xyz <- lapply(xyz, read.csv, header=FALSE, row.names=1, col.names=cnms)
    
    sids <- rep.int(sample_ids, vapply(xyz, nrow, numeric(1)))
    xyz <- data.frame(sample_id=sids, do.call(rbind, xyz))
    xyz$in_tissue <- as.logical(xyz$in_tissue)
    
    cd <- cbind(colData(x), spatialData(x))
    expect_identical(
        table(cd$sample_id, cd$in_tissue),
        table(xyz$sample_id, xyz$in_tissue))

    sfs <- file.path(samples, "spatial", "scalefactors_json.json")
    sfs <- lapply(sfs, function(.) fromJSON(file=.))
    sfs <- vapply(sfs, function(.) .[["tissue_lowres_scalef"]], numeric(1))
    expect_identical(imgData(x)$scaleFactor, sfs)
})

test_that("names of 'samples' are used as 'sample_id's", {
    names(samples) <- sample_ids
    x <- read10xVisium(samples, 
        type="sparse", data="raw", 
        images="lowres", load=FALSE)
    expect_identical(unique(x$sample_id), sample_ids)
})

test_that("'sample_id's are used when 'samples' are unnamed", {
    x <- read10xVisium(samples, sample_ids, 
        type="sparse", data="raw", 
        images="lowres", load=FALSE)
    expect_identical(unique(x$sample_id), sample_ids)
})

test_that("'load=TRUE' returns 'LoadedSpatialImage's", {
    x <- read10xVisium(samples, 
        type="sparse", data="raw", 
        images="lowres", load=TRUE)
    expect_true(all(vapply(getImg(x, TRUE, TRUE), 
        function(.) is(., "LoadedSpatialImage"), logical(1))))
})
