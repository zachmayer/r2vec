#' Fast PCA using the irlba library for SVD
#'
#' This functions performs principle components analysis via SVD,
#' using the irlba library.  Note that it is not possible to
#' center and scale a sparse matrix.   Folding in the
#' eigenvalues might help compensate for this.
#'
#' @param x a sparse matrix
#' @param n number of principle components to calculate
#' @param retx whether to return the principle components of the input data
#' @param fold_in_eigens whether to add the eigenvectors to the principle component rotations
#' @param ... arguments passed to irlbda
#' @export
#' @importFrom irlba irlba
#' @importFrom Matrix Matrix Diagonal
#' @importMethodsFrom Matrix %*%
#' @import methods
#' @return a sparse.prcomp object, which inherits from prcomp
sparseprcomp <- function (x, n = 5, retx = FALSE, fold_in_eigens=FALSE, ...) {
  s <- irlba(x, nu = 0, nv=n, ...)
  s$v <- Matrix(s$v)
  s$u <- Matrix(s$u)
  if (fold_in_eigens)
    s$v <- s$v %*% Diagonal(x=s$d)
  s$d <- s$d/sqrt(max(1, nrow(x) - 1))
  dimnames(s$v) <- list(colnames(x), paste0("PC", seq_len(ncol(s$v))))
  r <- list(sdev = s$d, rotation = s$v, center = FALSE, scale = FALSE)
  if (retx)
    r$x <- as.matrix(x %*% s$v)
  class(r) <- c("sparseprcomp", "prcomp")
  r
}

#' Fast PCA using the irlba library for SVD
#'
#' This functions performs principle components analysis via SVD,
#' using the irlba library.  Note that it is not possible to
#' center and scale a sparse matrix.   Folding in the
#' eigenvalues might help compensate for this.
#'
#' @param object a sparse.prcomp object
#' @param newdata a sparse matrix of new data
#' @param ... ignored
#' @method predict sparseprcomp
#' @importMethodsFrom Matrix %*%
#' @import methods
#' @export
#' @return a sparse matrix
predict.sparseprcomp <- function (object, newdata, ...) {
  if (missing(newdata)) {
    if (!is.null(object$x))
      return(object$x)
    else stop("no scores are available: refit with 'retx=TRUE'")
  }
  if (length(dim(newdata)) != 2L)
    stop("'newdata' must be a matrix or data frame")
  nm <- rownames(object$rotation)
  if (!is.null(nm)) {
    if (!all(nm %in% colnames(newdata)))
      stop("'newdata' does not have named columns matching one or more of the original columns")
    newdata <- newdata[, nm, drop = FALSE]
  }
  else {
    if (NCOL(newdata) != NROW(object$rotation))
      stop("'newdata' does not have the correct number of columns")
  }
  newdata %*% object$rotation
}
