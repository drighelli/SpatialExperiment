example(read10xVisium, echo = FALSE)

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

test_that("spatialCoords<-,matrix", {
    # shuffle coordinates
    xyz <- spatialCoords(spe)
    xyz <- xyz[sample(ncol(spe)), ]
    # rownames(value) != colnames(x) fails
    expect_error(spatialCoords(spe) <- xyz)
    # but passes with 'withDimnames=FALSE'
    expect_silent(spatialCoords(spe, withDimnames=FALSE) <- xyz)
    # no 'rownames(value)' passes
    rownames(xyz) <- NULL
    expect_silent(spatialCoords(spe) <- xyz)
})
