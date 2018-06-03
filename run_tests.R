# Placement Analysis Project
#
#install.packages("testthat", repos="http://cran.us.r-project.org")
library(testthat)

test.results <- test_dir(
   "tests/",
   reporter="summary")
