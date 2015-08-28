context("Skip grams")
test_that("Skip grams", {
  x <- list(
    c('one'),
    c('sent', 'one'),
    c('this', 'is', 'sentence', 'two'),
    c('this', 'is', 'sentence', 'three', 'sentence', 'three'),
    c('finally', 'we', 'have', 'a', 'fourth', 'longer', 'sentence'),
    character(0),
    NULL,
    NA,
    NaN
  )
  for (N in 1:10){
    out <- find_skip_grams(x, n=N, verbose=FALSE)
    expect_true(length(out) == length(x))
  }
})
