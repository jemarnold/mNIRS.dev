test_that("palette returns correct colours", {
    expect_length(mnirs_palette(), 12)
    expect_length(mnirs_palette(3), 3)
    expect_length(mnirs_palette(15), 15)
    expect_type(mnirs_palette(), "character")
    expect_match(mnirs_palette(1), "^#[0-9a-f]{6}$")
})




test_that("plot.mnirs.data moxy.perfpro works", {
    file_path <- system.file("extdata/moxy_ramp_example.xlsx",
                             package = "mnirs")

    df <- read_data(
        file_path = file_path,
        nirs_channels = c(smo2_left = "SmO2 Live",
                         smo2_right = "SmO2 Live(2)"),
        sample_channel = c(time = "hh:mm:ss"),
        verbose = FALSE)

    ## visual check
    plot <- plot(df)
    expect_s3_class(plot, "ggplot")
})




# test_that("plot.mnirs.data Oxysoft multiple channels works", {
#     file_path <- system.file("extdata/oxysoft_interval_example.xlsx",
#                              package = "mnirs")
#     expect_equal(class(file_path), "character")
#
#     df <- read_data(
#         file_path = file_path,
#         nirs_channels = c("O2Hb 1" = 2,
#                          "HHb 1" = 3,
#                          "tHb 1" = 4,
#                          "O2Hb 2" = 5,
#                          "HHb 2" = 6,
#                          "tHb 2" = 7),
#         sample_channel = c(sample = 1),
#         verbose = FALSE)
#
#     ## visual check
#     plot <- plot(df)
#     expect_s3_class(plot, "ggplot")
# })
