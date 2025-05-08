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

# tests --------------------
# check the get_plot_limit function
test_that("extract_values_from_raster", {
  library(terra)
  
  template <- rast("data/raster_template.tif")
  point <- vect("data/test_point.shp")
  values <- extract_values_from_raster(template, point)
  expect_equal(length(values[[1]]), 2)
  expect_equal(values[1,1], 2509.687)
  expect_equal(values[2,1], 2534.5088)
})

##Snippet of test_script.R
##Isolated to make testing extract_values easier/result clearer
