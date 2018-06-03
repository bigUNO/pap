# Placement Analysis Project
#
# Unit test
source("../calculate_performance_metrics.R")

test_that("CalculatePerformanceMetrics", {
   expect_equal(CalculatePerformanceMetrics(25, 68, "ctr"), (25 / 68) * 100)
   expect_equal(CalculatePerformanceMetrics(45, 53, "cpc"), (45 / 53))
})
