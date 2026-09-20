# Run the expectation suite by hand:
#
#   Rscript expectations/run.R
#
# It is outside tests/ on purpose. These are not adgl's own unit tests; they
# are a record of what users of the neighbouring packages expect, and they are
# meant to be read as much as run. The skipped ones are the gaps.

library(testthat)
result <- as.data.frame(test_file("expectations/test-expectations.R",
                                  reporter = "summary"))
cat("\nPASS ", sum(result$passed),
    "  FAIL ", sum(result$failed),
    "  ERROR ", sum(result$error),
    "  SKIP (gaps) ", sum(result$skipped), "\n", sep = "")
