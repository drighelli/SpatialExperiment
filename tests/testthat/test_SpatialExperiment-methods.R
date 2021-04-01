example(read10xVisium, echo = FALSE)

test_that("spatialData(),spatialCoords=colData=FALSE", {
    spd <- spatialData(spe, spatialCoords=FALSE, colData=FALSE)
    expect_identical(spd, colData(spe)[spatialDataNames(spe)])
})

test_that("spatialData(),spatialCoords=TRUE,colData=FALSE", {
    spd <- spatialData(spe, spatialCoords=TRUE, colData=FALSE)
    expect_identical(spd, cbind(colData(spe)[spatialDataNames(spe)], spatialCoords(spe)))
})

test_that("spatialData(),spatialCoords=TRUE,colData=TRUE", {
    spd <- spatialData(spe, spatialCoords=TRUE, colData=TRUE)
    expect_identical(spd, cbind(colData(spe), spatialCoords(spe)))
})

test_that("spatialData(),NULL", {
    tmp <- spe; spatialData(tmp) <- NULL
    spd <- spatialData(tmp, spatialCoords=TRUE, colData=FALSE)
    expect_identical(as.matrix(spd), spatialCoords(tmp))
    spd <- spatialData(tmp, spatialCoords=FALSE, colData=TRUE)
    expect_identical(spd, colData(tmp))
})

test_that("spatialData<-,NULL", {
    old <- spatialData(spe)
    spatialData(spe) <- NULL
    expect_is(spatialData(spe), "DFrame")
    expect_identical(spatialDataNames(spe), character())
    expect_true(!any(names(old) %in% names(colData(spe))))
    expect_equal(dim(spatialData(spe)), c(ncol(spe), 0))
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

