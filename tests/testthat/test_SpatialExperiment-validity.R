# note that, in order to enforce invalid replacements,
# the tests below purposefully use
#   @colData<- instead of colData<-
#   in_metadata$imgData<- instead of imgData<-
#   int_colData$spatialCoords <- instead of spatialCoords<-

test_that("colData", {
    # initialize mock SPE
    img <- system.file(
        "extdata", "10xVisium", "section1", "outs", "spatial", 
        "tissue_lowres_image.png", package="SpatialExperiment")
    spe <- SpatialExperiment(
        assays=diag(n <- 10),
        colData=DataFrame(a=seq(n)),
        sample_id="foo")
    spe <- addImg(spe, 
        imageSource=img, 
        scaleFactor=1, 
        sample_id="foo",
        image_id="foo",
        load=FALSE)
    # complete removal of colData
    tmp <- spe
    tmp@colData <- make_zero_col_DFrame(n)
    expect_error(validObject(tmp))
    # removal of sample_ids
    tmp <- spe
    tmp@colData$sample_id <- NULL
    expect_error(validObject(tmp))
    # mismatch with sample_id in imgData
    tmp <- spe
    tmp@colData$sample_id <- "x"
    expect_error(validObject(tmp))
})

test_that("spatialCoords", {
    # initialize mock SPE
    spe <- SpatialExperiment(
        assays=diag(n <- 10))
    # invalid replacements
    foo <- list(
        NULL,
        rep(1, n),
        rep("", n),
        factor(seq(n)),
        matrix("", n, 2),
        data.frame(seq(n)),
        DataFrame(seq(n)))
    for (. in foo) {
        int_colData(spe)$spatialCoords <- .
        expect_error(validObject(spe)) 
    }
    # valid replacements
    foo <- list(
        matrix(1, n),
        matrix(1, n, 2),
        matrix(NA_real_, n),
        matrix(NA_integer_, n))
    for (. in foo) {
        int_colData(spe)$spatialCoords <- .
        expect_true(validObject(spe)) 
    }
})

test_that("imgData", {
    # initialize mock SPE
    spe <- SpatialExperiment(
        assays=diag(n <- 10))
    # complete removal of imgData
    int_metadata(spe)$imgData <- NULL
    expect_error(validObject(spe))
})
