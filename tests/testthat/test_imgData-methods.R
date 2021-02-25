example(read10xVisium)

# getImg -----------------------------------------------------------------------

test_that("getImg,sample_id=image_id='foo' throws error", {
    expect_error(getImg(ve, sample_id='foo', image_id='foo'))
})

test_that("getImg,sample_id=image_id=NULL returns a 'SpatialImage'", {
    x <- getImg(ve, sample_id=NULL, image_id=NULL)
    y <- imgData(ve)$data[[1]]
    expect_is(x, "SpatialImage")
    expect_identical(x, y)
})

test_that("getImg,sample_id=image_id=TRUE returns a list of 'SpatialImage's", {
    x <- getImg(ve, sample_id=TRUE, image_id=TRUE)
    y <- imgData(ve)$data
    expect_is(x, "list")
    expect_identical(x, y)
})

# addImg -----------------------------------------------------------------------

test_that("addImg for existing sample_id,image_id throws error", {
    i <- sample(nrow(df <- imgData(ve)), 1)
    expect_error(addImg(ve, 
        imageSource=imgSource(ve), scaleFactor=NA_real_,
        sample_id=df$sample_id[i], image_id=df$image_id[i]))
})

# rmvImg -----------------------------------------------------------------------

test_that("rmvImg drops specified imgData entry", {
    i <- sample(nrow(df <- imgData(ve)), 1)
    x <- rmvImg(ve, sample_id=df$sample_id[i], image_id=df$image_id[i])
    expect_identical(imgData(x), imgData(ve)[-i, ])
})

# imgSource --------------------------------------------------------------------

test_that("imgSource,sample_id=image_id='foo' throws error", {
    expect_error(imgSource(ve, sample_id='foo', image_id='foo'))
})

test_that("imgSource,sample_id=image_id=NULL returns character", {
    x <- imgSource(ve, sample_id=NULL, image_id=NULL)
    y <- imgSource(imgData(ve)$data[[1]])
    expect_is(x, "character")
    expect_identical(x, y)
})

test_that("imgSource,sample_id=image_id=TRUE returns character vector", {
    x <- imgSource(ve, sample_id=TRUE, image_id=TRUE)
    y <- vapply(imgData(ve)$data, imgSource, character(1))
    expect_is(x, "character")
    expect_identical(x, y)
})