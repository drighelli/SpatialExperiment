example(read10xVisium, echo = FALSE)

test_that("spatialData(),spatialCoords=FALSE", {
    spd <- spatialData(spe, spatialCoords=FALSE)
    expect_is(spd, "DFrame")
    expect_identical(spd, int_colData(spe)$spatialData)
})

test_that("spatialData(),spatialCoords=TRUE", {
    spd <- spatialData(spe, spatialCoords=TRUE)
    expect_is(spd, "DFrame")
    expect_identical(spd, cbind(
        int_colData(spe)$spatialData, 
        spatialCoords(spe)))
})

test_that("spatialData(),NULL", {
    tmp <- spe; spatialData(tmp) <- NULL
    spd <- spatialData(tmp, spatialCoords=TRUE)
    expect_identical(as.matrix(spd), spatialCoords(tmp))
    spd <- spatialData(tmp, spatialCoords=FALSE)
    expect_equal(dim(spd), c(ncol(spe), 0))
    expect_identical(spd, int_colData(tmp)$spatialData)
})

test_that("spatialData<-,NULL", {
    tmp <- spe; spatialData(tmp) <- NULL
    spd <- spatialData(tmp, spatialCoords=FALSE)
    expect_equal(dim(spd), c(ncol(spe), 0))
    expect_identical(spatialDataNames(tmp), character())
})

test_that("spatialData<-,ANY", {
    mat <- matrix(0, ncol(spe), 2)
    expect_error(spatialData(spe) <- mat)
    df <- data.frame(mat)
    expect_error(spatialData(spe) <- mat)
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
