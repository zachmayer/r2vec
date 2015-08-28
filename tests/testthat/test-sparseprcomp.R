context("Sparse PCA")
test_that("Sparse PCA", {

  set.seed(1)
  N <- 100
  i <- sample(1:100, N, replace=TRUE)
  j <- sample(1:50, N, replace=TRUE)
  x <- rnorm(N)
  M <- Matrix::sparseMatrix(i=i, j=j, x=x)
  M <- Matrix::drop0(M)

  cmp <- 47
  model <- sparseprcomp(M, n=cmp, retx=TRUE, fold_in_eigens=FALSE)
  expect_equal(dim(model$rotation), c(ncol(M), cmp))
  expect_equal(dim(model$x), c(nrow(M), cmp))

  cmp <- 47
  model <- sparseprcomp(M, n=cmp, retx=TRUE, fold_in_eigens=TRUE)
  expect_equal(dim(model$rotation), c(ncol(M), cmp))
  expect_equal(dim(model$x), c(nrow(M), cmp))
})
