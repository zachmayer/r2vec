context("ngrams")
test_that("ngrams", {
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
  l1 <- sapply(x, length)
  i <- which(l1 <= 1)

  for (N in 1:10){
    out <- find_ngrams(x, n=N, verbose=FALSE)
    l2 <- sapply(out, length)
    expect_true(length(out) == length(x))
    expect_true(all(l2 >= l1))
    expect_true(all(l2[i] == l1[i]))
  }

  for (N in 1:10){
    out <- find_ngrams(x[i], n=N, verbose=FALSE)
    l2 <- sapply(out, length)
    expect_true(length(out) == length(x[i]))
    expect_true(all(l2 >= l1[i]))
    expect_true(all(l2 == l1[i]))
  }

  i <- 1
  for (N in 1:10){
    out <- find_ngrams(x[i], n=N, verbose=FALSE)
    l2 <- sapply(out, length)
    expect_true(length(out) == length(x[i]))
    expect_true(all(l2[i] >= l1[i]))
    expect_true(all(l2[i] == l1[i]))
  }
})
