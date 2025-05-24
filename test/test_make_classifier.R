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

test_that("make_classifier", {
  
  test_data <- runif(20)
  data <- data.frame("x1" = test_data,
                     "x2" = test_data * 2.45,
                     "ls"  = as.factor(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1))
  )
  classifier = make_classifier(data)
  expect_equal(classifier$forest$nclass, 2)
  expect_equal(classifier$classes, c("0","1"))
  expect_no_error(predict(classifier,data))
})

##Snippet of test_script.R
##Isolated to make testing make classifier easier/result clearer