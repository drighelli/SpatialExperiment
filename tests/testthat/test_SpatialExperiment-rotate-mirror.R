example(read10xVisium, echo = FALSE)

test_that("rotateCoords, non-null sample_id", {
    #   Subset followed by identity rotation should be equivalent to simply
    #   subsetting
    id <- unique(colData(spe)$sample_id)[1]
    x <- rotateCoords(spe, sample_id = id, degrees = 0, warn = FALSE)
    sub_spe <- spe[, colData(spe)$sample_id == id]
    expect_identical(sub_spe, x)
    
    #   Garbage value for 'sample_id' should throw an error
    expect_error(rotateCoords(spe, sample_id = "foo"))
})

test_that("mirrorCoords, identity mirror", {
    #   Mirroring twice vertically should do nothing
    x <- mirrorCoords(spe, sample_id = NULL, axis = "v", warn = FALSE)
    x <- mirrorCoords(x, sample_id = NULL, axis = "v", warn = FALSE)
    expect_identical(spe, x)
    
    #   Mirroring twice horizontally should do nothing
    x <- mirrorCoords(spe, sample_id = NULL, axis = "h", warn = FALSE)
    x <- mirrorCoords(x, sample_id = NULL, axis = "h", warn = FALSE)
    expect_identical(spe, x)
})

test_that("mirrorCoords, non-null sample_id", {
    #   Subset followed by mirror should be equivalent to mirror followed by
    #   subset
    id <- unique(colData(spe)$sample_id)[1]
    x1 <- mirrorCoords(spe, sample_id = id, axis = "h", warn = FALSE)
    x2 <- mirrorCoords(spe, sample_id = NULL, axis = "h", warn = FALSE)
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
    x2 <- mirrorCoords(spe, sample_id = NULL, axis = "v", warn = FALSE)
    x2 <- mirrorImg(x2, sample_id = TRUE, axis = "v")
    
    expect_identical(spatialCoords(x1), spatialCoords(x2))
    expect_identical(imgRaster(x1), imgRaster(x2))
    
    x1 <- mirrorObject(spe, sample_id = NULL, axis = "h")
    x2 <- mirrorCoords(spe, sample_id = NULL, axis = "h", warn = FALSE)
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
