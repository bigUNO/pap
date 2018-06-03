# Placement Analysis Project
#
# Unit test
source("../calculate_performance_metrics.R")

test_that("CalculatePerformanceMetrics", {
   expect_equal(CalculatePerformanceMetrics(25, 68, "ctr"), (25 / 68) * 100)
   expect_equal(CalculatePerformanceMetrics(0, 11, "ctr"), (0 / 1) * 100)
   expect_equal(CalculatePerformanceMetrics(2.3, 0, "ctr"), (2.3 / 0) * 100)

   expect_equal(CalculatePerformanceMetrics(45, 53, "cpc"), (45 / 53))
   expect_equal(CalculatePerformanceMetrics(0, 53, "cpc"), (0 / 53))
   expect_equal(CalculatePerformanceMetrics(0.4, 0, "cpc"), (0.4 / 0))
   #cat(CalculatePerformanceMetrics(0, 53, "ctr"))
})
