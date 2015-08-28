context("Tokenization")
test_that("Tokenization", {
  a <- list(
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
  b <- sapply(a, paste, collapse=' ')

  out <- find_tokens(b, split='', regex=FALSE)
  expect_equal(length(out), length(a))

  out <- find_tokens(b, split=' ', regex=FALSE)
  expect_equal(length(out), length(a))

  l1 <- sapply(a, length)
  l2 <- sapply(out, length)

  a <- l1 == l2
  b <- 1 == l2
  expect_true(all(a[l1 > 0]))
  expect_true(all(b[l1 == 0]))

  text <- 'weird.split.token'
  tokens <- find_tokens(text, split='.', regex=FALSE)
  expect_equal(length(tokens), 1)
  expect_equal(tokens[[1]], c("weird", "split", "token"))

  text <- 'weird.split-token'
  tokens <- find_tokens(text, split='[[:punct:]]', regex=TRUE)
  expect_equal(length(tokens), 1)
  expect_equal(tokens[[1]], c("weird", "split", "token"))
})
