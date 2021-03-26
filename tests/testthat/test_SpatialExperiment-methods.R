example(read10xVisium)

# spatialData ------------------------------------------------------------------

test_that("spatialData(); cd_bind=NULL doesn't include colData", {
    cd_nms <- names(colData(ve))
    spd <- spatialData(ve, cd_bind=NULL)
    expect_identical(colnames(spd), spatialDataNames(ve))
})

test_that("spatialData(); cd_bind=TRUE includes all colData", {
    cd_nms <- names(colData(ve))
    spd <- spatialData(ve, cd_bind=TRUE)
    expect_identical(spd, spatialData(ve, cd_bind=cd_nms))
    expect_identical(colnames(spd), c(spatialDataNames(ve), cd_nms))
})

test_that("spatialData(); cd_bind needs to be character in names(colData(x))", {
    expect_error(spatialData(ve, cd_bind=123))
    expect_error(spatialData(ve, cd_bind="x"))
    
    cd_cnms <- names(colData(ve))
    expect_error(spatialData(ve, cd_bind=c("x", cd_cnms)))
    
    cd_keep <- sample(cd_cnms, 1)
    expect_silent(spd <- spatialData(ve, cd_bind=cd_keep))
    expect_identical(colnames(spd), c(spatialDataNames(ve), cd_keep))
})

test_that("spatialData(); as_df=FALSE/TRUE returns matrix/data.frame", {
    expect_is(spatialData(ve, as_df=FALSE), "matrix")
    expect_is(spatialData(ve, as_df=TRUE), "DFrame")
})

test_that("spatialData(); sample_id=TRUE retains all observations", {
    spd <- spatialData(ve, sample_id=TRUE)
    expect_identical(spd, spatialData(ve, sample_id=unique(ve$sample_id)))
    expect_equivalent(spd, as(ve@spatialData, class(spd)[1]))
})

test_that("spatialData(); sample_id should be character in x$sample_id", {
    expect_error(spatialData(ve, sample_id=123))    
    expect_error(spatialData(ve, sample_id="x"))    
    
    id_keep <- sample(unique(ve$sample_id), 1)
    expect_silent(spd <- spatialData(ve, sample_id=id_keep))
    expect_equivalent(spd, spatialData(ve)[ve$sample_id == id_keep, ])
    expect_equivalent(spd, spatialData(ve[, ve$sample_id == id_keep]))
})

test_that("spatialData()<- allows matrix/data.frame/DFrame", {
    spd <- as.matrix(spatialData(ve))
    tmp <- ve; spatialData(tmp) <- spd
    expect_identical(
        spatialData(tmp, as_df=FALSE), 
        spatialData(ve, as_df=FALSE))
    
    spd <- as.data.frame(spatialData(ve))
    tmp <- ve; spatialData(tmp) <- spd
    expect_identical(
        spatialData(tmp, as_df=TRUE), 
        spatialData(ve, as_df=TRUE))
})

test_that("spatialData()<- of wrong dimension throws error", {
    spd <- spatialData(ve)
    idx <- seq_len(sample(seq(2, nrow(spd)-1), 1))
    expect_error(spatialData(ve) <- spd[idx, ])
    expect_error(spatialData(ve[, idx]) <- spd)
})

test_that("spatialData()<-NULL drops spatialData", {
    tmp <- ve; spatialData(tmp) <- NULL
    expect_is(spatialData(tmp), "DFrame")
    expect_true(isEmpty(spatialData(tmp)))
})

test_that("spatialDataNames() returns colnames(spatialData(x))", {
    expect_is(spatialDataNames(ve), "character")
    expect_identical(spatialDataNames(ve), colnames(spatialData(ve)))

    tmp <- ve; spatialData(tmp) <- NULL
    expect_identical(spatialDataNames(tmp), character(0))
})

# spatialCoords ----------------------------------------------------------------

test_that("spatialCoords(); as_df=FALSE/TRUE returns matrix/DFrame", {
    expect_is(spatialCoords(ve, as_df=FALSE), "matrix")
    expect_is(spatialCoords(ve, as_df=TRUE), "DFrame")
})

test_that("spatialCoords(); sample_id=TRUE retains all observations", {
    x <- spatialCoords(ve, sample_id=TRUE)
    expect_identical(x, spatialCoords(ve, sample_id=unique(ve$sample_id)))
    expect_equivalent(x, spatialData(ve, as_df=FALSE)[, spatialCoordsNames(ve)])
})

test_that("spatialCoordNames()<- requires character of correct length", {
    expect_error(spatialCoordsNames(ve) <- 123)
    expect_error(spatialCoordsNames(ve) <- "x")
    nms <- letters[seq_along(spatialCoordsNames(ve))]
    tmp <- ve; expect_silent(spatialCoordsNames(tmp) <- nms)
    expect_identical(spatialCoordsNames(tmp), nms)
})

test_that("scaleFactors()", {
    sfs <- scaleFactors(ve, sample_id=TRUE, image_id=TRUE)
    expect_is(sfs, "numeric")
    expect_true(length(sfs) == nrow(imgData(ve)))
    expect_identical(sfs, imgData(ve)$scaleFactor)
})

