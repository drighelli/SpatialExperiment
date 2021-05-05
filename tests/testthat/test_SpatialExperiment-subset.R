example(read10xVisium, echo = FALSE)

test_that("missing i/j retains all", {
    expect_identical(spe, spe[, ])
    expect_identical(spe, spe[TRUE, ])
    expect_identical(spe, spe[, TRUE])
})

test_that("i=TRUE,j=TRUE retains all data", {
    expect_identical(spe, spe[TRUE, TRUE])
})

test_that("i=TRUE,j=FALSE drops all samples and imgData", {
    x <- spe[TRUE, FALSE]
    expect_true(ncol(x) == 0)
    expect_true(nrow(x) == nrow(spe))
    expect_true(isEmpty(imgData(x)))
})

test_that("i=FALSE,j=TRUE drops all features but keeps imgData", {
    x <- spe[FALSE, TRUE]
    expect_true(nrow(x) == 0)
    expect_true(ncol(x) == ncol(spe))
    expect_identical(imgData(x), imgData(spe))
})

test_that("j=sample_id keeps only specified sample", {
    id <- sample(unique(spe$sample_id), 1)
    x <- spe[, spe$sample_id == id]
    expect_true(all(x$sample_id == id))
    expect_true(all(imgData(x)$sample_id == id))
})
