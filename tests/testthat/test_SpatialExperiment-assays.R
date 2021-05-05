example(read10xVisium, echo = FALSE)

test_that("molecules()/<- gets/sets assay(., 'molecules')", {
    tmp <- spe
    expect_identical(
        molecules(tmp) <- assay(spe),
        assay(tmp, "molecules") <- assay(spe))
    expect_identical(
        assayNames(tmp), 
        c(assayNames(spe), "molecules"))
})
