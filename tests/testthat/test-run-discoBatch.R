context("test-rundisco")

test_that("Batch execution", {
  indata <- discoGetSimu()
  example_res <- discoBatch(indata,aov_method = "Equal Variance",
                            osc_method="CS")
  expect_that(example_res, is_a("list"))
  expect_that(length(example_res), equals(1))
})

# test_that("Generate Report", {
#   indata <- discoGetSimu()
#   example_res <- discoBatch(indata,report = "DiscoRhythm_report.html",
#                           aov_method = "Equal Variance",
#                           osc_method = "CS")
#   expect_that(example_res, is_a("list"))
#   expect_that(length(example_res), equals(1))
#   expect_that(file.exists("DiscoRhythm_report.html"),equals(TRUE))
# })
