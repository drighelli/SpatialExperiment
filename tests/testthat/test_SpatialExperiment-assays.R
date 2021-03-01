example(read10xVisium)

test_that("molecules()/<- gets/sets assay(., 'molecules')", {
    tmp <- ve
    expect_identical(
        molecules(tmp) <- assay(ve),
        assay(tmp, "molecules") <- assay(ve))
    expect_identical(
        assayNames(tmp), 
        c(assayNames(ve), "molecules"))
})
