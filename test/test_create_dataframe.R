suppressPackageStartupMessages({
  library(testthat)
})

# Run like:
#jh1889@mirovia:~/work/teaching/SEPwC_assessments/sediment_assessment/test$ Rscript test_script.R 
# Test passed 🥇
# Test passed 🌈
# Test passed 🎊
# Test passed 🥳
# ── Warning: check main# 
# ...
#

# load in the script you want to test
source("../terrain_analysis.R")

test_that("create_dataframe", {
  library(terra)
  
  template <- rast("data/raster_template.tif")
  point <- vect("data/test_point.shp")
  raster_stack <- rast(list(template, template, template))
  df <- create_dataframe(raster_stack, point, 0)
  expect_s3_class(df, "data.frame")
  expect_equal(length(df[[1]]),2)
  expect_identical(colnames(df), c('raster_template', 'raster_template.1', 'raster_template.2', 'ls'))
  expected <- c(0,0)
  expected <- as.factor(expected)
  expect_equal(df$ls, expected)
})

##Snippet of test_script.R
##Isolated to make testing create_dataframe easier/result clearer