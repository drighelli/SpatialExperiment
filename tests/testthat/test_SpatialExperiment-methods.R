example(SpatialExperiment, echo = FALSE)

test_that("spatialCoordsNames()", {
    expect_identical(
        spatialCoordsNames(spe), 
        colnames(int_colData(spe)$spatialCoords))
})

test_that("spatialCoordsNames<-,character", {
    old <- spatialCoordsNames(spe)
    new <- sample(letters, length(old))
    spatialCoordsNames(spe) <- new
    expect_identical(spatialCoordsNames(spe), new)
    expect_identical(spatialCoordsNames(spe), 
        colnames(int_colData(spe)$spatialCoords))
})

test_that("spatialCoordsNames<-,NULL", {
    old <- spatialCoords(spe)
    spatialCoordsNames(spe) <- NULL
    expect_null(spatialCoordsNames(spe))
    expect_equivalent(spatialCoords(spe), old)
})

test_that("scaleFactors()", {
    sfs <- scaleFactors(spe, sample_id=TRUE, image_id=TRUE)
    expect_is(sfs, "numeric")
    expect_true(length(sfs) == nrow(imgData(spe)))
    expect_identical(sfs, imgData(spe)$scaleFactor)
})
