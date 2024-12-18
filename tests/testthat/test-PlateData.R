test_that("CreatePlateData", {
    # Create layout without measurements
    pd <- CreatePlateData(
        layout = rbind(
            dummyPlate("24-well", plate_name = "P1"),
            dummyPlate("6-well", plate_name = "P2"),
            dummyPlate("48-well", plate_name = "P3"),
            dummyPlate("96-well", plate_name = "P4")
        ),
        key = "key"
    )
    pd
    testthat::expect_true(TRUE) # Just test if it ran gracefully
})


test_that("Visualize PlateData", {
    pd <- CreatePlateData(
        layout = rbind(
            dummyPlate("24-well", plate_name = "P1"),
            dummyPlate("6-well", plate_name = "P2"),
            dummyPlate("48-well", plate_name = "P3"),
            dummyPlate("96-well", plate_name = "P4")
        ),
        key = "key"
    )
    plot_plateLayout(pd)
    testthat::expect_true(TRUE) # Just test if it ran gracefully No visual check.
})


test_that("Test introduction", {
    pd <- CreatePlateData(
        layout = rbind(
            dummyPlate("24-well", plate_name = "P1"),
            dummyPlate("6-well", plate_name = "P2"),
            dummyPlate("48-well", plate_name = "P3"),
            dummyPlate("96-well", plate_name = "P4")
        ),
        key = "key"
    )
    plot_plateLayout(pd)
    data(pd) <- data.frame(
        "key" = row.names(layout(pd)),
        "replicate" = rep(1:4, times = nrow(layout(pd)) * 2)
    )
    data(pd)$count <- rnorm(nrow(data(pd)), 1, 2)
    plot_series(pd, "replicate", "count", col = "key", facet_rows = 1)
    testthat::expect_true(TRUE) # Just test if it ran gracefully No visual check.
})
