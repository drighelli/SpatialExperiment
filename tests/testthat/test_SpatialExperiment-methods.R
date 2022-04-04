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

test_that("rotateCoords, non-null sample_id", {
    #   Subset followed by identity rotation should be equivalent to simply
    #   subsetting
    id <- unique(colData(spe)$sample_id)[1]
    x <- rotateCoords(spe, sample_id = id, degrees = 0)
    sub_spe <- spe[, colData(spe)$sample_id == id]
    expect_identical(sub_spe, x)
    
    #   Garbage value for 'sample_id' should throw an error
    expect_error(rotateCoords(spe, sample_id = "foo"))
})

test_that("mirrorCoords, identity mirror", {
    #   Mirroring twice vertically should do nothing
    x <- mirrorCoords(spe, sample_id = NULL, axis = "v")
    x <- mirrorCoords(x, sample_id = NULL, axis = "v")
    expect_identical(spe, x)
    
    #   Mirroring twice horizontally should do nothing
    x <- mirrorCoords(spe, sample_id = NULL, axis = "h")
    x <- mirrorCoords(x, sample_id = NULL, axis = "h")
    expect_identical(spe, x)
})

test_that("mirrorCoords, non-null sample_id", {
    #   Subset followed by mirror should be equivalent to mirror followed by
    #   subset
    id <- unique(colData(spe)$sample_id)[1]
    x1 <- mirrorCoords(spe, sample_id = id, axis = "h")
    x2 <- mirrorCoords(spe, sample_id = NULL, axis = "h")
    x2 <- x2[, colData(x2)$sample_id == id]
    expect_identical(x1, x2)
    
    #   Garbage value for 'sample_id' should throw an error
    expect_error(mirrorCoords(spe, sample_id = "foo", axis = "v"))
})

test_that("rotateObject, angles adding to 0", {
    #   Note that in the below tests, entire objects may not be identical
    #   since StoredSpatialImages may become LoadedSpatialImages with the same
    #   internal data (raster)
    
    #   Rotation by 0 degrees should do nothing
    x <- rotateObject(spe, sample_id = NULL, image_id = NULL, degrees = 0)
    expect_identical(spatialCoords(spe), spatialCoords(x))
    expect_identical(imgRaster(spe), imgRaster(x))
    
    #   Rotation by 180 degrees twice should do nothing
    x <- rotateObject(spe, sample_id = NULL, image_id = NULL, degrees = 180)
    x <- rotateObject(x, sample_id = NULL, image_id = NULL, degrees = 180)
    expect_identical(spatialCoords(spe), spatialCoords(x))
    expect_identical(imgRaster(spe), imgRaster(x))
    
    #   Rotation by 90 and then 270 degrees should do nothing
    x <- rotateObject(spe, sample_id = NULL, image_id = NULL, degrees = 90)
    x <- rotateObject(x, sample_id = NULL, image_id = NULL, degrees = 270)
    expect_identical(spatialCoords(spe), spatialCoords(x))
    expect_identical(imgRaster(spe), imgRaster(x))
})

test_that("mirrorObject, symmetric composition", {
    #   Note that in the below tests, entire objects may not be identical
    #   since StoredSpatialImages may become LoadedSpatialImages with the same
    #   internal data (raster)
    
    #   Order: mirrorCoords -> mirrorImg vs. mirrorImg -> mirrorCoords
    x1 <- mirrorObject(spe, sample_id = NULL, axis = "v")
    x2 <- mirrorCoords(spe, sample_id = NULL, axis = "v")
    x2 <- mirrorImg(x2, sample_id = TRUE, axis = "v")
    
    expect_identical(spatialCoords(x1), spatialCoords(x2))
    expect_identical(imgRaster(x1), imgRaster(x2))
    
    x1 <- mirrorObject(spe, sample_id = NULL, axis = "h")
    x2 <- mirrorCoords(spe, sample_id = NULL, axis = "h")
    x2 <- mirrorImg(x2, sample_id = TRUE, axis = "h")
    
    expect_identical(spatialCoords(x1), spatialCoords(x2))
    expect_identical(imgRaster(x1), imgRaster(x2))
})

test_that("mirrorObject, identity transformation", {
    #   Note that in the below tests, entire objects may not be identical
    #   since StoredSpatialImages may become LoadedSpatialImages with the same
    #   internal data (raster)
    
    #   Mirroring an object twice should do nothing
    x <- mirrorObject(spe, sample_id = NULL, axis = "h")
    x <- mirrorObject(x, sample_id = NULL, axis = "h")
    
    expect_identical(spatialCoords(x), spatialCoords(spe))
    expect_identical(imgRaster(x), imgRaster(spe))
    
    #   Mirroring an object twice should do nothing
    x <- mirrorObject(spe, sample_id = NULL, axis = "v")
    x <- mirrorObject(x, sample_id = NULL, axis = "v")
    
    expect_identical(spatialCoords(x), spatialCoords(spe))
    expect_identical(imgRaster(x), imgRaster(spe))
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
