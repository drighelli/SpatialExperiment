example(read10xVisium, echo = FALSE)

# getImg -----------------------------------------------------------------------

test_that("getImg,sample_id=image_id='foo' throws error", {
    expect_error(getImg(spe, sample_id='foo', image_id='foo'))
})

test_that("getImg,sample_id=image_id=NULL returns a 'SpatialImage'", {
    x <- getImg(spe, sample_id=NULL, image_id=NULL)
    y <- imgData(spe)$data[[1]]
    expect_is(x, "SpatialImage")
    expect_identical(x, y)
})

test_that("getImg,sample_id=image_id=TRUE returns a list of 'SpatialImage's", {
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

test_that("imgSource,sample_id=image_id=TRUE returns character spector", {
    x <- imgSource(spe, sample_id=TRUE, image_id=TRUE)
    y <- vapply(imgData(spe)$data, imgSource, character(1))
    expect_is(x, "character")
    expect_identical(x, y)
})
