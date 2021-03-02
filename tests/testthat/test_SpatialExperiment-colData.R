example(read10xVisium)

test_that("colData()<-NULL throws warning", {
    expect_warning(colData(ve) <- NULL)
})

test_that("x$sample_id<- needs one-to-one mapping", {
    expect_warning(ve$sample_id <- NULL)
    expect_warning(ve$sample_id <- sample(c(1, 2), ncol(ve), TRUE))
})

test_that("valid x$sample_id replacement updates imgData", {
    old <- unique(ve$sample_id)
    new <- paste0(old, "x")
    i <- match(ve$sample_id, old)
    tmp <- ve; tmp$sample_id <- new[i]
    for (i in seq_along(old))
        expect_equivalent(
            ve[, ve$sample_id == old[i]],
            tmp[, tmp$sample_id == new[i]])
    i <- match(imgData(ve)$sample_id, old)
    expect_identical(imgData(tmp)$sample_id, new[i])
})
