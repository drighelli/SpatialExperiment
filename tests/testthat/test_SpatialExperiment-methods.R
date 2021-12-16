example(read10xVisium, echo = FALSE)

test_that("spatialData()", {
    spd <- spatialData(spe)
    expect_is(spd, "DFrame")
    expect_identical(spd, colData(spe)[spatialDataNames(spe)])
})

test_that("spatialData(),NULL", {
    tmp <- spe; spatialData(tmp) <- NULL
    spd <- spatialData(tmp)
    expect_equal(dim(spd), c(ncol(spe), 0))
    expect_identical(spatialDataNames(tmp), character())
    expect_identical(spd, colData(tmp)[spatialDataNames(tmp)])
})

test_that("spatialData<-,ANY", {
    mat <- matrix(0, ncol(spe), 2)
    expect_error(spatialData(spe) <- mat)
    df <- data.frame(mat)
    expect_error(spatialData(spe) <- mat)
})

test_that("spatialDataNames()", {
    expect_identical(
        spatialDataNames(spe), 
        int_metadata(spe)$spatialDataNames)
})

test_that("spatialDataNames()<-,character", {
    # invalid replacement (anything not in colData)
    expect_error(spatialDataNames(spe) <- "peter_pan")
    # valid replacement (anything in colData)
    i <- spatialDataNames(spe) <- sample(names(colData(spe)), 2)
    expect_identical(int_metadata(spe)$spatialDataNames, i)
})

test_that("spatialDataNames()<-,NULL", {
    cd <- colData(spe)
    old <- spatialDataNames(spe)
    spatialDataNames(spe) <- NULL
    new <- spatialDataNames(spe)
    # colData shouldn't be affected
    expect_identical(colData(spe), cd)
    expect_identical(new, character(0))
})

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
