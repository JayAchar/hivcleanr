context("oca_hiv_cleanr testthat")
library(hivcleanr)

path <- system.file("testdata", "raw.rds", package="hivcleanr")
raw <- readRDS(path)
data <- oca_hiv_cleanr(raw)

test_that(
    "Test that structure of output is correct",
    {
    expect_equal(ncol(data), 171) 
    expect_equal(names(data)[2], "recent_fu")
    expect_equivalent(mean(data$age), 41.3, tolerance = 0.04)
    expect_equal(class(data$art_start), "Date")
    expect_equal(class(data$date_60), "Date")
    }
)

test_that("Expect errors",
          {
    expect_error(oca_hiv_cleanr(c(1,2,3)), "x is not a data frame")          
              
              
              
              
              
          })
