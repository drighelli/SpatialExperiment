dir <- system.file(
    file.path("extdata", "10xVisium"), 
    package="SpatialExperiment")

sample_ids <- c("section1", "section2")
samples <- file.path(dir, sample_ids, "outs")

test_that("data are read correctly", {
    x <- read10xVisium(samples, sample_ids,
        type="sparse", data="raw", 
        images="lowres", load=FALSE)
    
    expect_is(x, "SpatialExperiment")
    expect_true(all(vapply(getImg(x, TRUE, TRUE), 
        function(.) is(., "VirtualSpatialImage"), logical(1))))
    
    cnms <- c(
        "barcode", "in_tissue", "array_row", "array_col", 
        "pxl_col_in_fullres", "pxl_row_in_fullres")
    xyz <- file.path(samples, "spatial", "tissue_positions_list.csv")
    xyz <- lapply(xyz, read.csv, header=FALSE, row.names=1, col.names=cnms)
    
    sids <- rep.int(sample_ids, vapply(xyz, nrow, numeric(1)))
    xyz <- data.frame(sample_id=sids, do.call(rbind, xyz))
    xyz$in_tissue <- as.logical(xyz$in_tissue)

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

test_that("adding 'outs/' directory works correctly for one or more samples", {
    samples2 <- samples3 <- file.path(dir, sample_ids)
    samples3[1] <- file.path(samples3[1], "outs")
    
    x1 <- read10xVisium(samples, sample_ids, type = "sparse", 
                        data = "raw", images = "lowres", load = FALSE)
    x2 <- read10xVisium(samples2, sample_ids, type = "sparse", 
                        data = "raw", images = "lowres", load = FALSE)
    x3 <- read10xVisium(samples3, sample_ids, type = "sparse", 
                        data = "raw", images = "lowres", load = FALSE)
    
    expect_identical(x1, x2)
    expect_identical(x1, x3)
})

test_that(paste0("tissue positions files are read in correct sample order and ", 
                 "spatial coordinates are in correct order in SPE object", 
                 "in datasets with multiple samples"), {
    # read in 4 samples and check spatial coordinates are in correct order
    # correct sample order is: section1, section2, section1, section2
    samples <- c(samples, samples)
    sample_ids <- c(sample_ids, paste0(sample_ids, "rep"))
    
    spatial_coords_1 <- spatialCoords(
      read10xVisium(samples[1], sample_ids[1], type = "sparse", 
                    data = "raw", images = "lowres", load = FALSE))
    spatial_coords_2 <- spatialCoords(
      read10xVisium(samples[2], sample_ids[2], type = "sparse", 
                    data = "raw", images = "lowres", load = FALSE))
    
    spatial_coords_multi <- spatialCoords(
      read10xVisium(samples, sample_ids, type = "sparse", 
                    data = "raw", images = "lowres", load = FALSE))
    
    # correct number of spatial locations
    expect_equal(
        nrow(spatial_coords_multi), 
        2 * nrow(spatial_coords_1) + 2 * nrow(spatial_coords_2))
    
    # spatial coordinates are in correct order
    expect_identical(
        spatial_coords_multi, 
        rbind(spatial_coords_1, spatial_coords_2, 
              spatial_coords_1, spatial_coords_2))
})
