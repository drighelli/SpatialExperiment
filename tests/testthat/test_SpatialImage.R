# This tests the SpatialImage architecture.
# library(testthat); library(SpatialExperiment); source("test_spatial_image.R")

path <- system.file("extdata", "10xVisium", "section1", "outs", "spatial", 
    "tissue_lowres_image.png", package="SpatialExperiment")

test_that("SpatialImage constructors and basic methods work as expected", {
    fspi <- SpatialImage(path)
    expect_s4_class(fspi, "StoredSpatialImage")

    img <- imgRaster(fspi)
    expect_s3_class(img, "raster")
    expect_identical(dim(fspi), dim(img))

    lspi <- as(fspi, "LoadedSpatialImage")
    expect_s4_class(lspi, "LoadedSpatialImage")

    img2 <- imgRaster(lspi)
    expect_identical(img, img2)
    expect_identical(dim(lspi), dim(fspi))
})

test_that("SpatialImage in-memory caching works as expected", {
    SpatialExperiment:::.flush_cache()
    cache <- SpatialExperiment:::image.cache
    expect_identical(cache$cached, list())

    # Creation doesn't trigger the cache.
    fspi <- SpatialImage(path)
    expect_identical(cache$cached, list())

    # Loads an image into the cache.
    img <- imgRaster(fspi)
    expect_identical(length(cache$cached), 1L)

    # Just uses whatever's in the cache, rather than reloading.
    cache$cached[[1]] <- cache$cached[[1]][1:10,1:10]
    dims <- dim(fspi)
    expect_identical(dims, c(10L, 10L))

    # Rotation of last-used image works as expected.
    img2 <- img[1:500,1:500]
    cache$cached <- c(cache$cached, list(A=img2, B=img2, C=img2))
    dummy <- imgRaster(fspi)
    expect_identical(
        normalizePath(names(cache$cached), mustWork=FALSE), 
        normalizePath(c(LETTERS[1:3], path), mustWork=FALSE))

    # LRU eviction policy works as intended.
    SpatialExperiment:::.flush_cache()
    oldo <- options("SpatialExperiment.cache.size"=as.double(object.size(img)))

    cache$cached <- list(A=img2, B=img2, C=img2)
    img <- imgRaster(fspi)
    expect_identical(
        normalizePath(names(cache$cached), mustWork=FALSE), 
        normalizePath(path, mustWork=FALSE))

    options(oldo)
})

test_that("SpatialImage URL caching works as expected", {
    SpatialExperiment:::.flush_cache()
    val <- tempfile()
    oldc <- options("SpatialExperiment.remote.cache.path"=val)

    tst <- "https://jeroen.github.io/images/frink.png" # same as that in ?image_read.
    rspi <- SpatialImage(tst)
    expect_false(dir.exists(val))

    img <- imgRaster(rspi)
    expect_true(dir.exists(val))
    expect_s3_class(img, "raster")

    cache <- SpatialExperiment:::image.cache
    expect_identical(names(cache$cached), tst)

    options(oldc)
})
