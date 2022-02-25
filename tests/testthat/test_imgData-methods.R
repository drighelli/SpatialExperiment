example(read10xVisium, echo = FALSE)

test_that("getImg/imgRaster/Source w/o imgData return NULL", {
    x <- spe
    imgData(x) <- NULL
    expect_null(getImg(x))
    expect_null(imgRaster(x))
    expect_null(imgSource(x))
})

# getImg -----------------------------------------------------------------------

test_that("getImg,sample_id=image_id='foo' throws error", {
    expect_error(getImg(spe, sample_id='foo', image_id='foo'))
})

test_that("getImg,sample_id=image_id=NULL returns a 'VirtualSpatialImage'", {
    x <- getImg(spe, sample_id=NULL, image_id=NULL)
    y <- imgData(spe)$data[[1]]
    expect_is(x, "VirtualSpatialImage")
    expect_identical(x, y)
})

test_that("getImg,sample_id=image_id=TRUE returns a list", {
    x <- getImg(spe, sample_id=TRUE, image_id=TRUE)
    y <- imgData(spe)$data
    expect_is(x, "list")
    expect_identical(x, y)
})

# addImg -----------------------------------------------------------------------

test_that("addImg for existing sample_id,image_id throws error", {
    i <- sample(nrow(df <- imgData(spe)), 1)
    expect_error(addImg(spe, 
        imageSource=imgSource(spe), scaleFactor=NA_real_,
        sample_id=df$sample_id[i], image_id=df$image_id[i]))
})

# rmvImg -----------------------------------------------------------------------

test_that("rmvImg drops specified imgData entry", {
    i <- sample(nrow(df <- imgData(spe)), 1)
    x <- rmvImg(spe, sample_id=df$sample_id[i], image_id=df$image_id[i])
    expect_identical(imgData(x), imgData(spe)[-i, ])
})

# imgSource --------------------------------------------------------------------

test_that("imgSource,sample_id=image_id='foo' throws error", {
    expect_error(imgSource(spe, sample_id='foo', image_id='foo'))
})

test_that("imgSource,sample_id=image_id=NULL returns character", {
    x <- imgSource(spe, sample_id=NULL, image_id=NULL)
    y <- imgSource(imgData(spe)$data[[1]])
    expect_is(x, "character")
    expect_identical(x, y)
})

test_that("imgSource,sample_id=image_id=TRUE returns character vector", {
    x <- imgSource(spe, sample_id=TRUE, image_id=TRUE)
    y <- vapply(imgData(spe)$data, imgSource, character(1))
    expect_is(x, "character")
    expect_identical(x, y)
})

# imgRaster --------------------------------------------------------------------

test_that("imgRaster,sample_id=image_id='foo' throws error", {
    expect_error(imgRaster(spe, sample_id='foo', image_id='foo'))
})

test_that("imgRaster,sample_id=image_id=NULL returns character", {
    x <- imgRaster(spe, sample_id=NULL, image_id=NULL)
    y <- imgRaster(imgData(spe)$data[[1]])
    expect_is(x, "raster")
    expect_identical(x, y)
})

test_that("imgRaster,sample_id=image_id=TRUE returns raster list", {
    x <- imgRaster(spe, sample_id=TRUE, image_id=TRUE)
    y <- lapply(imgData(spe)$data, imgRaster)
    expect_is(x, "list")
    expect_true(all(vapply(x, class, character(1)) == "raster"))
    expect_identical(x, y)
})
